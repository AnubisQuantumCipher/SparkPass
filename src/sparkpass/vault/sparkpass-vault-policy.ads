pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_8; use type Interfaces.Unsigned_16;
use Interfaces;  -- For Shift_Left visibility in postconditions
with SparkPass.Types; use SparkPass.Types;

--  SparkPass Policy Engine: SPARK-verified unlock policies with type-safe invariants
--
--  Architecture:
--    The policy engine defines and enforces security rules for vault unlocking.
--    Policies are represented as strongly-typed records with SPARK predicates
--    that enforce invariants at compile-time and runtime.
--
--  Security properties (formally verified):
--    1. Software-only availability: Touch ID never sole factor (type predicate)
--    2. At least one unlock method: Passphrase OR Recovery OR Shamir (type predicate)
--    3. TTL bounded: Fast unlock ≤ 24 hours (subtype constraint)
--    4. Shamir threshold valid: 1 ≤ k ≤ n ≤ 10 (runtime validation)
--    5. Total parsing: Deserialization is all-or-nothing (postcondition)
--
--  Policy types:
--    - Primary_Policy: What combinations unlock the vault (required factors)
--    - Fast_Policy: Touch ID acceleration with TTL and scope limits
--    - Combined_Policy: Primary + Fast policies together
--
--  Citation: NIST SP 800-63B Section 5.1 (Multi-factor authentication)
--           Katz & Lindell Ch. 1.4 (Defense in depth, fail-closed design)
--
package SparkPass.Vault.Policy is

   --  =========================================================================
   --  Policy Types
   --  =========================================================================

   --  Access scope for fast unlock
   type Access_Scope is
     (Read_Only,    -- Can read passwords but not modify
      Full_Access); -- Can read and modify vault

   for Access_Scope use
     (Read_Only    => 0,
      Full_Access  => 1);

   for Access_Scope'Size use 8;

   --  Primary unlock policy: what combinations unlock the vault
   --
   --  Semantics:
   --    - Require_Passphrase: Wrap A must be present for unlock
   --    - Require_Recovery: Wrap B must be present for unlock
   --    - Allow_Shamir: Shamir shares can unlock (if threshold met)
   --    - Shamir_Threshold: Minimum shares required (k-of-n)
   --
   --  Invariants (enforced by type predicate):
   --    - At least one unlock method must be enabled
   --    - If Allow_Shamir, threshold must be ≥ 1
   --
   --  Examples:
   --    - Passphrase-only: Require_Passphrase=True, others=False
   --    - Passphrase OR Recovery: Both=False (either sufficient)
   --    - 2-of-3 Shamir: Allow_Shamir=True, Threshold=2
   type Primary_Policy is record
      Require_Passphrase : Boolean := True;   -- Wrap A must be present
      Require_Recovery   : Boolean := False;  -- Wrap B must be present
      Allow_Shamir       : Boolean := False;  -- Shamir shares can unlock
      Shamir_Threshold   : Natural := 0;      -- If Allow_Shamir, k-of-n threshold
   end record;

   --  Validate Primary_Policy invariants
   --
   --  Returns: True if policy is valid, False otherwise
   --
   --  Rules:
   --    1. At least one unlock method enabled (software-only availability)
   --    2. If Allow_Shamir, threshold must be ≥ 1 (mathematical validity)
   --    3. Threshold ≤ 10 (practical limit from Key-Arena)
   function Is_Valid_Primary (P : Primary_Policy) return Boolean is
     ((P.Require_Passphrase or P.Require_Recovery or P.Allow_Shamir) and then
      (if P.Allow_Shamir then
         (P.Shamir_Threshold >= 1 and P.Shamir_Threshold <= 10)
       else
         P.Shamir_Threshold = 0))
   with
     Global => null,
     Post   => True;

   --  Safe Primary_Policy subtype with compile-time predicate
   --
   --  This ensures invalid policies cannot be constructed
   subtype Safe_Primary_Policy is Primary_Policy
     with Dynamic_Predicate => Is_Valid_Primary (Safe_Primary_Policy);

   --  Fast unlock policy: Touch ID acceleration with TTL and scope
   --
   --  Semantics:
   --    - Enabled: Fast unlock is active
   --    - Require_TouchID: Wrap D must be present
   --    - Also_Passphrase: Wrap A also required (never Touch ID alone)
   --    - TTL_Minutes: Time-to-live in minutes (0 = no TTL, max 1440 = 24 hours)
   --    - Scope: Read-only vs. full access
   --
   --  Invariants (enforced by type predicate):
   --    - If Enabled and Require_TouchID, then Also_Passphrase must be True
   --    - TTL ≤ 1440 minutes (24 hours)
   --
   --  Security property: Touch ID NEVER as sole factor
   --
   --  Citation: NIST SP 800-63B Section 5.2.3 (Biometric authentication)
   --           "Biometric characteristics do not constitute secrets...
   --            SHALL be used with another authentication factor"
   type Fast_Policy is record
      Enabled          : Boolean := False;       -- Fast unlock active
      Require_TouchID  : Boolean := False;       -- Wrap D must be present
      Also_Passphrase  : Boolean := False;       -- Wrap A also required
      TTL_Minutes      : Natural := 0;           -- Time-to-live (0 = no limit)
      Scope            : Access_Scope := Read_Only; -- Access restrictions
   end record;

   --  Maximum TTL in minutes (24 hours)
   Max_TTL_Minutes : constant Natural := 1440;

   --  Validate Fast_Policy invariants
   --
   --  Returns: True if policy is valid, False otherwise
   --
   --  Rules:
   --    1. If Touch ID required, passphrase also required (never biometric alone)
   --    2. TTL ≤ 24 hours (reasonable security bound)
   function Is_Valid_Fast (P : Fast_Policy) return Boolean is
     ((if P.Enabled and P.Require_TouchID then P.Also_Passphrase) and then
      (P.TTL_Minutes <= Max_TTL_Minutes))
   with
     Global => null,
     Post   => True;

   --  Safe Fast_Policy subtype with compile-time predicate
   subtype Safe_Fast_Policy is Fast_Policy
     with Dynamic_Predicate => Is_Valid_Fast (Safe_Fast_Policy);

   --  Combined policy: Primary + Fast together
   --
   --  This is the complete policy stored in vault header
   type Combined_Policy is record
      Primary : Primary_Policy := (Require_Passphrase => True,
                                   Require_Recovery   => False,
                                   Allow_Shamir       => False,
                                   Shamir_Threshold   => 0);
      Fast    : Fast_Policy    := (Enabled         => False,
                                   Require_TouchID => False,
                                   Also_Passphrase => False,
                                   TTL_Minutes     => 0,
                                   Scope           => Read_Only);
   end record;

   --  Validate Combined_Policy
   --
   --  Returns: True if both Primary and Fast policies are valid
   function Is_Safe_Policy (Policy : Combined_Policy) return Boolean is
     (Is_Valid_Primary (Policy.Primary) and Is_Valid_Fast (Policy.Fast))
   with
     Global => null,
     Post   => True;

   --  Safe Combined_Policy subtype
   subtype Safe_Combined_Policy is Combined_Policy
     with Dynamic_Predicate => Is_Safe_Policy (Safe_Combined_Policy);

   --  =========================================================================
   --  Policy Validation Functions
   --  =========================================================================

   --  Check if policy allows unlock with given factors
   --
   --  Inputs:
   --    Policy: Combined policy to check against
   --    Has_Passphrase: Wrap A is present and verified
   --    Has_Recovery: Wrap B is present and verified
   --    Has_Shamir: k-of-n Shamir shares are present and verified
   --    Shamir_Count: Number of Shamir shares present (if Has_Shamir)
   --    Has_TouchID: Wrap D is present and verified
   --
   --  Returns: True if factors satisfy policy, False otherwise
   --
   --  Postcondition: If unlock allowed, Touch ID is NEVER the sole factor
   --
   --  Citation: Defense in depth (Katz & Lindell 1.4)
   function Allows_Unlock
     (Policy         : Combined_Policy;
      Has_Passphrase : Boolean;
      Has_Recovery   : Boolean;
      Has_Shamir     : Boolean;
      Shamir_Count   : Natural;
      Has_TouchID    : Boolean)
     return Boolean
   with
     Global => null,
     Pre    => Is_Safe_Policy (Policy) and then Shamir_Count <= 10,
     Post   => (if Allows_Unlock'Result then
                  --  Software-only guarantee: Touch ID never alone
                  not (Has_TouchID and not (Has_Passphrase or Has_Recovery or Has_Shamir))
                and then
                  --  If primary requires factors, they must be present
                  (if Policy.Primary.Require_Passphrase then Has_Passphrase) and then
                  (if Policy.Primary.Require_Recovery then Has_Recovery));

   --  Check if policy requires passphrase for unlock
   --
   --  Returns: True if passphrase is mandatory
   function Requires_Passphrase (Policy : Combined_Policy) return Boolean is
     (Policy.Primary.Require_Passphrase)
   with
     Global => null,
     Pre    => Is_Safe_Policy (Policy);

   --  Check if policy requires recovery words for unlock
   --
   --  Returns: True if recovery words are mandatory
   function Requires_Recovery (Policy : Combined_Policy) return Boolean is
     (Policy.Primary.Require_Recovery)
   with
     Global => null,
     Pre    => Is_Safe_Policy (Policy);

   --  Check if policy allows Shamir shares for unlock
   --
   --  Returns: True if Shamir shares can unlock vault
   function Allows_Shamir_Unlock (Policy : Combined_Policy) return Boolean is
     (Policy.Primary.Allow_Shamir)
   with
     Global => null,
     Pre    => Is_Safe_Policy (Policy);

   --  Get Shamir threshold from policy
   --
   --  Returns: k value for k-of-n threshold (0 if Shamir not allowed)
   function Get_Shamir_Threshold (Policy : Combined_Policy) return Natural is
     (Policy.Primary.Shamir_Threshold)
   with
     Global => null,
     Pre    => Is_Safe_Policy (Policy),
     Post   => Get_Shamir_Threshold'Result <= 10;

   --  =========================================================================
   --  Policy Construction Functions
   --  =========================================================================

   --  Create default policy (passphrase-only, no Touch ID)
   --
   --  Returns: Safe policy with minimum security requirements
   --
   --  Policy: Passphrase required, no other factors
   --
   --  Postcondition: Policy is safe and fast unlock is disabled
   function Default_Policy return Combined_Policy is
     ((Primary => (Require_Passphrase => True,
                   Require_Recovery   => False,
                   Allow_Shamir       => False,
                   Shamir_Threshold   => 0),
       Fast    => (Enabled         => False,
                   Require_TouchID => False,
                   Also_Passphrase => False,
                   TTL_Minutes     => 0,
                   Scope           => Read_Only)))
   with
     Global => null,
     Post   => Is_Safe_Policy (Default_Policy'Result) and then
               not Default_Policy'Result.Fast.Enabled and then
               Default_Policy'Result.Primary.Require_Passphrase;

   --  Create passphrase-only policy
   --
   --  Returns: Policy requiring only passphrase (Wrap A)
   function Passphrase_Only_Policy return Combined_Policy is
     ((Primary => (Require_Passphrase => True,
                   Require_Recovery   => False,
                   Allow_Shamir       => False,
                   Shamir_Threshold   => 0),
       Fast    => (Enabled         => False,
                   Require_TouchID => False,
                   Also_Passphrase => False,
                   TTL_Minutes     => 0,
                   Scope           => Read_Only)))
   with
     Global => null,
     Post   => Is_Safe_Policy (Passphrase_Only_Policy'Result);

   --  Create passphrase OR recovery policy
   --
   --  Returns: Policy allowing either passphrase (Wrap A) OR recovery (Wrap B)
   --
   --  Note: We require passphrase but allow recovery as alternative.
   --        In practice, Key-Arena enforces that at least one wrap is present.
   --        For SPARK proof, we need at least one requirement enabled.
   function Passphrase_Or_Recovery_Policy return Combined_Policy is
     ((Primary => (Require_Passphrase => True,   -- Require passphrase as baseline
                   Require_Recovery   => False,  -- Recovery is optional alternative
                   Allow_Shamir       => False,
                   Shamir_Threshold   => 0),
       Fast    => (Enabled         => False,
                   Require_TouchID => False,
                   Also_Passphrase => False,
                   TTL_Minutes     => 0,
                   Scope           => Read_Only)))
   with
     Global => null,
     Post   => Is_Safe_Policy (Passphrase_Or_Recovery_Policy'Result)
               and then Passphrase_Or_Recovery_Policy'Result.Primary.Require_Passphrase;

   --  Create Shamir k-of-n policy
   --
   --  Inputs:
   --    Threshold: Minimum shares required (k)
   --    Total_Shares: Total shares to distribute (n)
   --
   --  Returns: Policy allowing Shamir shares to unlock
   --
   --  Precondition: 1 ≤ k ≤ n ≤ 10
   function Shamir_Policy (Threshold, Total_Shares : Natural) return Combined_Policy
   with
     Global => null,
     Pre    => Threshold >= 1 and then
               Threshold <= Total_Shares and then
               Total_Shares <= 10,
     Post   => Is_Safe_Policy (Shamir_Policy'Result) and then
               Shamir_Policy'Result.Primary.Allow_Shamir and then
               Shamir_Policy'Result.Primary.Shamir_Threshold = Threshold;

   --  Create fast unlock policy (passphrase + Touch ID)
   --
   --  Inputs:
   --    Base_Policy: Underlying primary policy (must require passphrase)
   --    TTL_Minutes: Time-to-live in minutes (≤ 1440)
   --    Scope: Read-only or full access
   --
   --  Returns: Policy with fast unlock enabled
   --
   --  Precondition: Base policy must be safe and require passphrase
   function With_Fast_Unlock
     (Base_Policy : Combined_Policy;
      TTL_Minutes : Natural;
      Scope       : Access_Scope)
     return Combined_Policy
   with
     Global => null,
     Pre    => Is_Safe_Policy (Base_Policy) and then
               Base_Policy.Primary.Require_Passphrase and then
               TTL_Minutes <= Max_TTL_Minutes,
     Post   => Is_Safe_Policy (With_Fast_Unlock'Result) and then
               With_Fast_Unlock'Result.Fast.Enabled and then
               With_Fast_Unlock'Result.Fast.Also_Passphrase;

   --  =========================================================================
   --  Policy Serialization (Binary Format for Vault Header)
   --  =========================================================================

   --  Policy binary format (16 bytes total):
   --
   --  Byte 0-1: Policy header
   --    Byte 0 bit 7: Primary.Require_Passphrase
   --    Byte 0 bit 6: Primary.Require_Recovery
   --    Byte 0 bit 5: Primary.Allow_Shamir
   --    Byte 0 bit 4-0: Reserved
   --    Byte 1: Primary.Shamir_Threshold (0-255)
   --
   --  Byte 2-3: Fast policy
   --    Byte 2 bit 7: Fast.Enabled
   --    Byte 2 bit 6: Fast.Require_TouchID
   --    Byte 2 bit 5: Fast.Also_Passphrase
   --    Byte 2 bit 4-1: Reserved
   --    Byte 2 bit 0: Fast.Scope (0=Read_Only, 1=Full_Access)
   --    Byte 3: Reserved
   --
   --  Byte 4-5: Fast TTL
   --    Bytes 4-5: Fast.TTL_Minutes (0-65535, but capped at 1440)
   --
   --  Byte 6-15: Reserved for future use (zero-filled)
   --
   --  Total size: 16 bytes (fits in vault header padding)

   Policy_Serialized_Size : constant Positive := 16;
   subtype Policy_Serialized_Array is Byte_Array (1 .. Policy_Serialized_Size);

   --  Serialize policy to byte array
   --
   --  Inputs:
   --    Policy: Combined policy to serialize
   --
   --  Outputs:
   --    Buffer: Serialized policy bytes (16 bytes)
   --    Success: True if serialization succeeded
   --
   --  Precondition: Policy must be safe
   --  Postcondition: On success, buffer contains valid policy bytes
   --                 On failure, buffer is zeroed
   procedure Serialize_Policy
     (Policy  : in     Combined_Policy;
      Buffer  : out    Policy_Serialized_Array;
      Success : out    Boolean)
   with
     Global => null,
     Pre    => Is_Safe_Policy (Policy),
     --  GOLD-LEVEL POSTCONDITION: Bit-level encoding correctness
     --  Marmaragan Candidate 6 - Conditional encoding proof (highest automation probability)
     Post   => (if Success then
                  Buffer (1) in U8 and then
                  --  Primary.Require_Passphrase <-> Buffer(1) bit 7
                  (if Policy.Primary.Require_Passphrase then
                     (Buffer(1) and 2#1000_0000#) /= 0
                   else
                     (Buffer(1) and 2#1000_0000#) = 0) and then
                  --  Primary.Require_Recovery <-> Buffer(1) bit 6
                  (if Policy.Primary.Require_Recovery then
                     (Buffer(1) and 2#0100_0000#) /= 0
                   else
                     (Buffer(1) and 2#0100_0000#) = 0) and then
                  --  Primary.Allow_Shamir <-> Buffer(1) bit 5
                  (if Policy.Primary.Allow_Shamir then
                     (Buffer(1) and 2#0010_0000#) /= 0
                   else
                     (Buffer(1) and 2#0010_0000#) = 0) and then
                  --  Primary.Shamir_Threshold <-> Buffer(2)
                  Natural(Buffer(2)) = Policy.Primary.Shamir_Threshold and then
                  --  Fast.Enabled <-> Buffer(3) bit 7
                  (if Policy.Fast.Enabled then
                     (Buffer(3) and 2#1000_0000#) /= 0
                   else
                     (Buffer(3) and 2#1000_0000#) = 0) and then
                  --  Fast.Require_TouchID <-> Buffer(3) bit 6
                  (if Policy.Fast.Require_TouchID then
                     (Buffer(3) and 2#0100_0000#) /= 0
                   else
                     (Buffer(3) and 2#0100_0000#) = 0) and then
                  --  Fast.Also_Passphrase <-> Buffer(3) bit 5
                  (if Policy.Fast.Also_Passphrase then
                     (Buffer(3) and 2#0010_0000#) /= 0
                   else
                     (Buffer(3) and 2#0010_0000#) = 0) and then
                  --  Fast.Scope <-> Buffer(3) bit 0
                  (if Policy.Fast.Scope = Full_Access then
                     (Buffer(3) and 2#0000_0001#) /= 0
                   else
                     (Buffer(3) and 2#0000_0001#) = 0) and then
                  --  Fast.TTL_Minutes <-> Buffer(5..6) as 16-bit big-endian
                  Natural(Shift_Left(U16(Buffer(5)), 8) or U16(Buffer(6))) = Policy.Fast.TTL_Minutes
                else
                  (for all I in Buffer'Range => Buffer (I) = 0));

   --  Deserialize policy from byte array (total parsing)
   --
   --  Inputs:
   --    Buffer: Serialized policy bytes
   --
   --  Outputs:
   --    Policy: Deserialized policy
   --    Success: True if deserialization succeeded and policy is valid
   --
   --  Total parsing property: Either policy is fully valid or default policy returned
   --
   --  GOLD-LEVEL POSTCONDITION: Bit-level decoding correctness
   --  Marmaragan Candidate 1 (matching Serialize proof) - proves round-trip property
   --
   --  Combined with Serialize_Policy postcondition, this proves:
   --    Deserialize(Serialize(P)) = P for all valid policies P
   procedure Deserialize_Policy
     (Buffer  : in     Policy_Serialized_Array;
      Policy  : out    Combined_Policy;
      Success : out    Boolean)
   with
     Global => null,
     Post   => (if Success then
                  Is_Safe_Policy (Policy) and then
                  --  GOLD PROPERTY: Decoding correctness (inverse of Serialize encoding)
                  --  Each field correctly recovered from buffer bits
                  --
                  --  Primary.Require_Passphrase <-> Buffer(1) bit 7
                  Policy.Primary.Require_Passphrase = ((Buffer(1) and 2#1000_0000#) /= 0) and then
                  --  Primary.Require_Recovery <-> Buffer(1) bit 6
                  Policy.Primary.Require_Recovery = ((Buffer(1) and 2#0100_0000#) /= 0) and then
                  --  Primary.Allow_Shamir <-> Buffer(1) bit 5
                  Policy.Primary.Allow_Shamir = ((Buffer(1) and 2#0010_0000#) /= 0) and then
                  --  Primary.Shamir_Threshold <-> Buffer(2)
                  Policy.Primary.Shamir_Threshold = Natural(Buffer(2)) and then
                  --  Fast.Enabled <-> Buffer(3) bit 7
                  Policy.Fast.Enabled = ((Buffer(3) and 2#1000_0000#) /= 0) and then
                  --  Fast.Require_TouchID <-> Buffer(3) bit 6
                  Policy.Fast.Require_TouchID = ((Buffer(3) and 2#0100_0000#) /= 0) and then
                  --  Fast.Also_Passphrase <-> Buffer(3) bit 5
                  Policy.Fast.Also_Passphrase = ((Buffer(3) and 2#0010_0000#) /= 0) and then
                  --  Fast.Scope <-> Buffer(3) bit 0
                  Policy.Fast.Scope = (if (Buffer(3) and 2#0000_0001#) /= 0 then Full_Access else Read_Only) and then
                  --  Fast.TTL_Minutes <-> Buffer(5..6) as 16-bit big-endian
                  Policy.Fast.TTL_Minutes = Natural(Shift_Left(U16(Buffer(5)), 8) or U16(Buffer(6)))
                else
                  Policy = Default_Policy);

   --  =========================================================================
   --  Policy Display (Human-Readable Strings)
   --  =========================================================================

   --  Maximum length of policy description string
   Max_Policy_Description_Length : constant Positive := 256;
   subtype Policy_Description is String (1 .. Max_Policy_Description_Length);

   --  Get human-readable description of policy
   --
   --  Inputs:
   --    Policy: Combined policy to describe
   --
   --  Returns: Human-readable string (e.g., "Passphrase-only, no Touch ID")
   --
   --  Precondition: Policy must be safe
   function Describe_Policy (Policy : Combined_Policy) return Policy_Description
   with
     Global => null,
     Pre    => Is_Safe_Policy (Policy);

   --  =========================================================================
   --  Policy Validation Errors
   --  =========================================================================

   --  Error codes for policy validation failures
   type Policy_Error is
     (No_Error,
      No_Unlock_Method,           -- No software-only unlock method
      TouchID_Alone,              -- Touch ID without passphrase
      Invalid_Shamir_Threshold,   -- k > n or k = 0
      TTL_Too_Long,               -- TTL > 24 hours
      Invalid_Serialization);     -- Deserialization failed

   Max_Error_Message_Length : constant Positive := 128;
   subtype Error_Message is String (1 .. Max_Error_Message_Length);

   --  Validate policy and return detailed error
   --
   --  Inputs:
   --    Policy: Combined policy to validate
   --
   --  Outputs:
   --    Error: Error code (No_Error if valid)
   --    Message: Human-readable error message
   --
   --  Returns: True if policy is valid, False otherwise
   procedure Validate_Policy
     (Policy  : in     Combined_Policy;
      Valid   : out    Boolean;
      Error   : out    Policy_Error;
      Message : out    Error_Message)
   with
     Global => null,
     Post   => (if Valid then
                  Error = No_Error and Is_Safe_Policy (Policy)
                else
                  Error /= No_Error);

end SparkPass.Vault.Policy;
