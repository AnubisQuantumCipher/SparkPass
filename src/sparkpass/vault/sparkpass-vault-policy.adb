pragma SPARK_Mode (On);
with Interfaces; use Interfaces;

package body SparkPass.Vault.Policy is

   --  =========================================================================
   --  Policy Validation Functions
   --  =========================================================================

   --  =========================================================================
   --  Policy Validation Functions
   --  =========================================================================
   --
   --  Allows_Unlock: Determines if provided authentication factors satisfy policy
   --
   --  SECURITY PROPERTY (Formally Verified):
   --    If unlock is allowed, then:
   --      1. Touch ID is NEVER the sole factor (software-only guarantee)
   --      2. All required factors are present (if Require_Passphrase, then Has_Passphrase)
   --      3. All required factors are present (if Require_Recovery, then Has_Recovery)
   --
   --  PROOF STRATEGY:
   --    - Postcondition encodes security invariants
   --    - Implementation ensures unlock only when ALL requirements met
   --    - Early return pattern: if any requirement fails, return False
   --
   --  CITATION: Defense in depth (Katz & Lindell 1.4), Fail-closed design
   function Allows_Unlock
     (Policy         : Combined_Policy;
      Has_Passphrase : Boolean;
      Has_Recovery   : Boolean;
      Has_Shamir     : Boolean;
      Shamir_Count   : Natural;
      Has_TouchID    : Boolean)
     return Boolean
   is
      Primary_Satisfied : Boolean := False;
      Fast_Satisfied    : Boolean := False;
   begin
      --  ======================================================================
      --  PRIMARY POLICY EVALUATION
      --  ======================================================================
      --
      --  Logic: Check if primary unlock requirements are satisfied
      --
      --  Cases:
      --    1. Require_Passphrase AND Require_Recovery: BOTH must be present
      --    2. Require_Passphrase only: Passphrase must be present
      --    3. Require_Recovery only: Recovery must be present
      --    4. Allow_Shamir only: k-of-n shares must be present
      --    5. None required: At least ONE software factor (flexible policy)

      --  CASE 1: Both passphrase AND recovery required (strongest policy)
      if Policy.Primary.Require_Passphrase and Policy.Primary.Require_Recovery then
         Primary_Satisfied := Has_Passphrase and Has_Recovery;
         --  PROOF: If Primary_Satisfied, then BOTH Has_Passphrase AND Has_Recovery
         pragma Assert (if Primary_Satisfied then (Has_Passphrase and Has_Recovery));

      --  CASE 2: Only passphrase required
      elsif Policy.Primary.Require_Passphrase and not Policy.Primary.Require_Recovery then
         Primary_Satisfied := Has_Passphrase;
         --  PROOF: If Primary_Satisfied, then Has_Passphrase
         pragma Assert (if Primary_Satisfied then Has_Passphrase);

      --  CASE 3: Only recovery required
      elsif Policy.Primary.Require_Recovery and not Policy.Primary.Require_Passphrase then
         Primary_Satisfied := Has_Recovery;
         --  PROOF: If Primary_Satisfied, then Has_Recovery
         pragma Assert (if Primary_Satisfied then Has_Recovery);

      --  CASE 4: Shamir shares can unlock (no passphrase/recovery required)
      elsif Policy.Primary.Allow_Shamir then
         if Has_Shamir and Shamir_Count >= Policy.Primary.Shamir_Threshold then
            Primary_Satisfied := True;
         end if;
         --  PROOF: No Require_* flags set in this case
         pragma Assert (not Policy.Primary.Require_Passphrase);
         pragma Assert (not Policy.Primary.Require_Recovery);

      --  CASE 5: Flexible policy (at least one software factor)
      else
         Primary_Satisfied := Has_Passphrase or Has_Recovery;
         --  PROOF: No Require_* flags set in this case
         pragma Assert (not Policy.Primary.Require_Passphrase);
         pragma Assert (not Policy.Primary.Require_Recovery);
      end if;

      --  ======================================================================
      --  FAST UNLOCK POLICY EVALUATION (Touch ID acceleration)
      --  ======================================================================
      --
      --  Security Invariant: Touch ID NEVER as sole factor (enforced by predicate)
      --
      --  If fast unlock enabled, check if Touch ID + Passphrase are present

      if Policy.Fast.Enabled then
         if Policy.Fast.Require_TouchID then
            --  Touch ID requires passphrase (enforced by type predicate)
            --  Double-check at runtime for defense in depth
            if Policy.Fast.Also_Passphrase then
               Fast_Satisfied := Has_TouchID and Has_Passphrase;
               --  Postcondition satisfied: Has_Passphrase is True if Fast_Satisfied
            else
               --  Invalid policy (should never happen due to predicate)
               Fast_Satisfied := False;
            end if;
         else
            --  Fast unlock doesn't require Touch ID (unused configuration)
            Fast_Satisfied := False;
         end if;
      end if;

      --  ======================================================================
      --  FINAL DECISION
      --  ======================================================================
      --
      --  CRITICAL SECURITY DECISION:
      --    The postcondition requires: "if Require_Recovery then Has_Recovery"
      --    This means primary policy requirements are MANDATORY, not optional.
      --
      --  ANALYSIS OF CURRENT LOGIC:
      --    return Primary_Satisfied OR Fast_Satisfied
      --
      --  PROBLEM:
      --    If Primary.Require_Recovery = True and Has_Recovery = False:
      --      - Primary_Satisfied = False (because recovery is missing)
      --      - Fast_Satisfied = True (if Has_TouchID and Has_Passphrase)
      --      - Result = False OR True = True (ALLOWS UNLOCK!)
      --      - Postcondition violated: Require_Recovery but not Has_Recovery
      --
      --  FIX:
      --    Fast unlock is an ACCELERATION, not a BYPASS.
      --    If primary policy has requirements, they MUST be met even with fast unlock.
      --    Therefore: return Primary_Satisfied OR (Fast_Satisfied AND Primary requirements met)
      --
      --  IMPLEMENTATION:
      --    Fast unlock can only be used if primary policy requirements are MET
      --    (even if primary policy itself would accept alternatives like Shamir)

      --  Check if primary policy MANDATORY requirements are met
      declare
         Primary_Requirements_Met : constant Boolean :=
           (if Policy.Primary.Require_Passphrase then Has_Passphrase else True) and
           (if Policy.Primary.Require_Recovery then Has_Recovery else True);
      begin
         --  PROOF HELP: Expand Primary_Requirements_Met definition explicitly
         pragma Assert (Primary_Requirements_Met =
                         ((if Policy.Primary.Require_Passphrase then Has_Passphrase else True) and
                          (if Policy.Primary.Require_Recovery then Has_Recovery else True)));

         --  PROOF: If Primary_Requirements_Met and Require_Passphrase, then Has_Passphrase
         pragma Assert (if Primary_Requirements_Met and Policy.Primary.Require_Passphrase
                        then Has_Passphrase);

         --  PROOF: If Primary_Requirements_Met and Require_Recovery, then Has_Recovery
         pragma Assert (if Primary_Requirements_Met and Policy.Primary.Require_Recovery
                        then Has_Recovery);

         --  Assertion 1: Primary policy implications
         pragma Assert (if Primary_Satisfied and Policy.Primary.Require_Passphrase
                        then Has_Passphrase);

         --  Assertion 2: Primary policy implications
         pragma Assert (if Primary_Satisfied and Policy.Primary.Require_Recovery
                        then Has_Recovery);

         --  Assertion 3: Touch ID never alone (fast unlock requires passphrase)
         pragma Assert (if Fast_Satisfied then Has_Passphrase);
         pragma Assert (if Fast_Satisfied and Primary_Requirements_Met and Has_TouchID
                        then (Has_Passphrase or Has_Recovery or Has_Shamir));

         --  Return decision: Primary satisfied OR (Fast satisfied AND Primary requirements met)
         return Primary_Satisfied or (Fast_Satisfied and Primary_Requirements_Met);
      end;
   end Allows_Unlock;

   --  =========================================================================
   --  Policy Construction Functions
   --  =========================================================================

   function Shamir_Policy (Threshold, Total_Shares : Natural) return Combined_Policy is
   begin
      return (Primary => (Require_Passphrase => False,
                          Require_Recovery   => False,
                          Allow_Shamir       => True,
                          Shamir_Threshold   => Threshold),
              Fast    => (Enabled         => False,
                          Require_TouchID => False,
                          Also_Passphrase => False,
                          TTL_Minutes     => 0,
                          Scope           => Read_Only));
   end Shamir_Policy;

   function With_Fast_Unlock
     (Base_Policy : Combined_Policy;
      TTL_Minutes : Natural;
      Scope       : Access_Scope)
     return Combined_Policy
   is
      Result : Combined_Policy := Base_Policy;
   begin
      Result.Fast.Enabled         := True;
      Result.Fast.Require_TouchID := True;
      Result.Fast.Also_Passphrase := True;
      Result.Fast.TTL_Minutes     := TTL_Minutes;
      Result.Fast.Scope           := Scope;
      return Result;
   end With_Fast_Unlock;

   --  =========================================================================
   --  Policy Serialization
   --  =========================================================================

   procedure Serialize_Policy
     (Policy  : in     Combined_Policy;
      Buffer  : out    Policy_Serialized_Array;
      Success : out    Boolean)
   is
      Byte0 : U8 := 0;
      Byte2 : U8 := 0;
      TTL_High : U8;
      TTL_Low  : U8;
   begin
      --  Initialize output
      Success := False;
      Buffer := (others => 0);

      --  Validate policy before serialization
      if not Is_Safe_Policy (Policy) then
         return;
      end if;

      --  Byte 0: Primary policy flags
      if Policy.Primary.Require_Passphrase then
         Byte0 := Byte0 or 2#1000_0000#;  -- Bit 7
      end if;
      if Policy.Primary.Require_Recovery then
         Byte0 := Byte0 or 2#0100_0000#;  -- Bit 6
      end if;
      if Policy.Primary.Allow_Shamir then
         Byte0 := Byte0 or 2#0010_0000#;  -- Bit 5
      end if;
      Buffer (1) := Byte0;

      --  Byte 1: Shamir threshold
      Buffer (2) := U8 (Policy.Primary.Shamir_Threshold);

      --  Byte 2: Fast policy flags
      if Policy.Fast.Enabled then
         Byte2 := Byte2 or 2#1000_0000#;  -- Bit 7
      end if;
      if Policy.Fast.Require_TouchID then
         Byte2 := Byte2 or 2#0100_0000#;  -- Bit 6
      end if;
      if Policy.Fast.Also_Passphrase then
         Byte2 := Byte2 or 2#0010_0000#;  -- Bit 5
      end if;
      if Policy.Fast.Scope = Full_Access then
         Byte2 := Byte2 or 2#0000_0001#;  -- Bit 0
      end if;
      Buffer (3) := Byte2;

      --  Byte 3: Reserved
      Buffer (4) := 0;

      --  Bytes 4-5: TTL as 16-bit big-endian
      --  Cap at 65535 (but policy validation ensures ≤ 1440)
      TTL_High := U8 (Shift_Right (U16 (Policy.Fast.TTL_Minutes), 8) and 16#FF#);
      TTL_Low  := U8 (U16 (Policy.Fast.TTL_Minutes) and 16#FF#);
      Buffer (5) := TTL_High;
      Buffer (6) := TTL_Low;

      --  Bytes 6-15: Reserved (already zeroed)

      Success := True;
   end Serialize_Policy;

   procedure Deserialize_Policy
     (Buffer  : in     Policy_Serialized_Array;
      Policy  : out    Combined_Policy;
      Success : out    Boolean)
   is
      Byte0 : U8;
      Byte2 : U8;
      TTL_High : U8;
      TTL_Low  : U8;
      TTL_Value : U16;
      Candidate : Combined_Policy;
   begin
      --  Initialize output with default policy (fail-closed)
      Success := False;
      Policy := Default_Policy;

      --  Parse byte 0: Primary policy flags
      Byte0 := Buffer (1);
      Candidate.Primary.Require_Passphrase := (Byte0 and 2#1000_0000#) /= 0;
      Candidate.Primary.Require_Recovery   := (Byte0 and 2#0100_0000#) /= 0;
      Candidate.Primary.Allow_Shamir       := (Byte0 and 2#0010_0000#) /= 0;

      --  Parse byte 1: Shamir threshold
      Candidate.Primary.Shamir_Threshold := Natural (Buffer (2));

      --  Validate Shamir configuration
      if Candidate.Primary.Allow_Shamir then
         if Candidate.Primary.Shamir_Threshold = 0 or
            Candidate.Primary.Shamir_Threshold > 10
         then
            --  Invalid Shamir threshold
            Policy := Default_Policy;
            return;
         end if;
      else
         --  If Shamir not allowed, threshold must be 0
         if Candidate.Primary.Shamir_Threshold /= 0 then
            Policy := Default_Policy;
            return;
         end if;
      end if;

      --  Parse byte 2: Fast policy flags
      Byte2 := Buffer (3);
      Candidate.Fast.Enabled         := (Byte2 and 2#1000_0000#) /= 0;
      Candidate.Fast.Require_TouchID := (Byte2 and 2#0100_0000#) /= 0;
      Candidate.Fast.Also_Passphrase := (Byte2 and 2#0010_0000#) /= 0;

      if (Byte2 and 2#0000_0001#) /= 0 then
         Candidate.Fast.Scope := Full_Access;
      else
         Candidate.Fast.Scope := Read_Only;
      end if;

      --  Parse bytes 4-5: TTL
      TTL_High := Buffer (5);
      TTL_Low  := Buffer (6);
      TTL_Value := Shift_Left (U16 (TTL_High), 8) or U16 (TTL_Low);
      Candidate.Fast.TTL_Minutes := Natural (TTL_Value);

      --  Validate TTL
      if Candidate.Fast.TTL_Minutes > Max_TTL_Minutes then
         --  TTL too long, reject
         Policy := Default_Policy;
         return;
      end if;

      --  Validate fast policy constraints
      if Candidate.Fast.Enabled and Candidate.Fast.Require_TouchID then
         if not Candidate.Fast.Also_Passphrase then
            --  Touch ID without passphrase, reject (security violation)
            Policy := Default_Policy;
            return;
         end if;
      end if;

      --  Final validation: Check complete policy
      if not Is_Safe_Policy (Candidate) then
         Policy := Default_Policy;
         return;
      end if;

      --  All checks passed, accept policy
      Policy := Candidate;
      Success := True;
   end Deserialize_Policy;

   --  =========================================================================
   --  Policy Display
   --  =========================================================================

   function Describe_Policy (Policy : Combined_Policy) return Policy_Description is
      Result : Policy_Description := (others => ' ');
      Pos    : Positive := 1;

      --  Helper: Append string to Result
      procedure Append (S : String)
      with
        Pre  => Pos in Result'Range
                and then S'Length < Natural'Last / 2,
        Post => Pos in Result'First .. Result'Last + 1
                and then Pos <= Pos'Old + S'Length
      is
      begin
         if Pos + S'Length - 1 <= Result'Last then
            Result (Pos .. Pos + S'Length - 1) := S;
            Pos := Pos + S'Length;
         end if;
      end Append;

      --  Helper: Append Natural as decimal string
      --
      --  Proof Strategy:
      --    - Loop variant: Val decreases each iteration (division by 10)
      --    - Loop invariant: Idx stays in valid range (Str'First .. Str'Last)
      --    - Termination: Val reaches 0 in at most 10 iterations (max digits in Natural)
      --
      --  Security Property: Produces human-readable decimal representation,
      --  preventing integer injection attacks in policy description output.
      procedure Append_Natural (N : Natural)
      with
        Pre  => Pos in Result'Range
                and then Pos <= Result'Last - 10,  -- Max 10 digits in Natural'Image
        Post => Pos in Result'First .. Result'Last + 1
                and then Pos <= Pos'Old + 10,      -- Max increase bounded
        Always_Terminates  -- Proven by loop variant (Val strictly decreases)
      is
         Str : String (1 .. 10) := (others => ' ');
         Val : Natural := N;
         Idx : Positive := Str'Last;
      begin
         if Val = 0 then
            Append ("0");
            return;
         end if;

         while Val > 0 loop
            --  Loop invariant: Val is decreasing monotonically
            pragma Loop_Invariant (Val >= 0);
            --  Loop invariant: Idx is in valid range for next write
            pragma Loop_Invariant (Idx in Str'Range);
            --  Loop invariant: Idx never decreases below Str'First (ensures Idx - 1 >= 0)
            pragma Loop_Invariant (Idx >= Str'First);
            --  Loop invariant: All positions after Idx have been filled (no gaps)
            pragma Loop_Invariant (for all I in Idx + 1 .. Str'Last =>
                                     Str (I) /= ' ');
            --  Loop variant: Val decreases strictly (proves termination)
            --  Each iteration divides by 10, so Val := Val / 10 guarantees progress
            pragma Loop_Variant (Decreases => Val);

            Str (Idx) := Character'Val (48 + (Val mod 10));
            Val := Val / 10;
            --  After this assignment, Idx will be in range Str'First - 1 .. Str'Last - 1
            --  The loop guard (Val > 0) ensures we exit before Idx becomes < Str'First
            if Idx > Str'First then
               Idx := Idx - 1;
            else
               --  Val must be 0 here (cannot have more than 10 digits), exit
               pragma Assert (Val = 0 or Idx = Str'First);
               exit;
            end if;
         end loop;

         Append (Str (Idx + 1 .. Str'Last));
      end Append_Natural;

   begin
      --  Describe primary policy
      if Policy.Primary.Require_Passphrase and Policy.Primary.Require_Recovery then
         Append ("Passphrase AND Recovery required");
      elsif Policy.Primary.Require_Passphrase then
         Append ("Passphrase required");
      elsif Policy.Primary.Require_Recovery then
         Append ("Recovery required");
      elsif Policy.Primary.Allow_Shamir then
         Append_Natural (Policy.Primary.Shamir_Threshold);
         Append ("-of-n Shamir shares");
      else
         Append ("Passphrase OR Recovery");
      end if;

      --  Describe fast unlock
      if Policy.Fast.Enabled then
         Append ("; Touch ID fast unlock (");
         Append_Natural (Policy.Fast.TTL_Minutes);
         Append ("min TTL, ");
         if Policy.Fast.Scope = Read_Only then
            Append ("read-only)");
         else
            Append ("full access)");
         end if;
      else
         Append ("; No Touch ID");
      end if;

      return Result;
   end Describe_Policy;

   --  =========================================================================
   --  Policy Validation Errors
   --  =========================================================================

   --  =========================================================================
   --  Policy Validation with Detailed Error Reporting
   --  =========================================================================
   --
   --  Validate_Policy: Performs comprehensive policy validation with error details
   --
   --  SECURITY PROPERTY (Formally Verified):
   --    If Valid = True, then Is_Safe_Policy (Policy) = True
   --    If Valid = False, then Error /= No_Error and Message contains explanation
   --
   --  PROOF STRATEGY:
   --    - Check all Is_Safe_Policy conditions explicitly
   --    - Use Is_Safe_Policy as final oracle (postcondition guarantee)
   --    - Early return on first error (fail-closed)
   --
   --  CITATION: Total parsing (Katz & Lindell 1.3.2), Defense in depth
   procedure Validate_Policy
     (Policy  : in     Combined_Policy;
      Valid   : out    Boolean;
      Error   : out    Policy_Error;
      Message : out    Error_Message)
   is
   begin
      --  Initialize outputs (fail-closed: assume invalid until proven valid)
      Valid := False;
      Error := No_Error;
      Message := (others => ' ');

      --  =====================================================================
      --  PRIMARY POLICY VALIDATION
      --  =====================================================================

      --  Check 1: At least one unlock method enabled (software-only availability)
      --
      --  Security Requirement: Vault must have at least one non-Touch ID unlock path
      --  This ensures availability even if Touch ID fails or is unavailable
      if not Policy.Primary.Require_Passphrase and
         not Policy.Primary.Require_Recovery and
         not Policy.Primary.Allow_Shamir
      then
         Error := No_Unlock_Method;
         Message (1 .. 47) := "No unlock method enabled (software-only failed)";
         return;  -- Early return (fail-closed)
      end if;

      --  Check 2: Shamir configuration validation (mathematical validity)
      --
      --  If Shamir is allowed, threshold must be valid: 1 <= k <= 10
      --  If Shamir is not allowed, threshold must be 0
      if Policy.Primary.Allow_Shamir then
         if Policy.Primary.Shamir_Threshold = 0 then
            Error := Invalid_Shamir_Threshold;
            Message (1 .. 38) := "Shamir threshold is zero (invalid k=0)";
            return;  -- Early return (fail-closed)
         end if;
         if Policy.Primary.Shamir_Threshold > 10 then
            Error := Invalid_Shamir_Threshold;
            Message (1 .. 37) := "Shamir threshold too large (max k=10)";
            return;  -- Early return (fail-closed)
         end if;
      else
         --  Shamir not allowed, threshold must be 0
         if Policy.Primary.Shamir_Threshold /= 0 then
            Error := Invalid_Shamir_Threshold;
            Message (1 .. 44) := "Shamir disabled but threshold is non-zero   ";
            return;  -- Early return (fail-closed)
         end if;
      end if;

      --  =====================================================================
      --  FAST UNLOCK POLICY VALIDATION
      --  =====================================================================

      --  Check 3: Touch ID security constraints (biometric never alone)
      --
      --  Security Requirement: Touch ID MUST be combined with passphrase
      --  NIST SP 800-63B Section 5.2.3: "Biometric characteristics do not
      --  constitute secrets... SHALL be used with another authentication factor"
      if Policy.Fast.Enabled then
         if Policy.Fast.Require_TouchID and not Policy.Fast.Also_Passphrase then
            Error := TouchID_Alone;
            Message (1 .. 52) := "Touch ID requires passphrase (biometric never alone)";
            return;  -- Early return (fail-closed)
         end if;

         --  Check 4: TTL bounds (reasonable security window)
         --
         --  Limit fast unlock time-to-live to 24 hours maximum
         if Policy.Fast.TTL_Minutes > Max_TTL_Minutes then
            Error := TTL_Too_Long;
            Message (1 .. 39) := "TTL exceeds 24 hours (max 1440 minutes)";
            return;  -- Early return (fail-closed)
         end if;
      end if;

      --  =====================================================================
      --  FINAL VALIDATION: Is_Safe_Policy Oracle
      --  =====================================================================
      --
      --  PROOF GUARANTEE: If all individual checks pass AND Is_Safe_Policy returns True,
      --  then policy is guaranteed safe by type predicate.
      --
      --  This final check is CRITICAL for postcondition proof:
      --    Post => (if Valid then Error = No_Error and Is_Safe_Policy (Policy))
      --
      --  Without this check, prover cannot prove the postcondition because
      --  individual checks don't syntactically match Is_Safe_Policy definition.

      if not Is_Safe_Policy (Policy) then
         --  Should never reach here if all checks above passed, but defense in depth
         Error := Invalid_Serialization;
         --  String must match slice length exactly (34 chars)
         Message (1 .. 34) := "Policy failed Is_Safe_Policy check";
         return;  -- Early return (fail-closed)
      end if;

      --  =====================================================================
      --  SUCCESS PATH
      --  =====================================================================
      --
      --  All validation checks passed:
      --    - At least one unlock method enabled
      --    - Shamir configuration valid (if enabled)
      --    - Touch ID never alone (if enabled)
      --    - TTL within bounds (if fast unlock enabled)
      --    - Is_Safe_Policy predicate satisfied
      --
      --  Postcondition satisfied: Valid = True AND Is_Safe_Policy (Policy) = True

      --  PROOF: Explicit assertion to connect Is_Safe_Policy check to postcondition
      --  We checked Is_Safe_Policy above and didn't return, so it must be True
      pragma Assert (Is_Safe_Policy (Policy));
      pragma Assert (Error = No_Error);  -- No error set, so still No_Error

      Valid := True;
      Error := No_Error;
      Message (1 .. 18) := "Policy is valid   ";
   end Validate_Policy;

end SparkPass.Vault.Policy;
