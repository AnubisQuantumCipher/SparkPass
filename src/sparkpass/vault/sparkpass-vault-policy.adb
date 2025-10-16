pragma SPARK_Mode (On);
with Interfaces; use Interfaces;

package body SparkPass.Vault.Policy is

   --  =========================================================================
   --  Policy Validation Functions
   --  =========================================================================

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
      --  Check primary policy requirements
      --
      --  Logic:
      --    If Require_Passphrase=True, passphrase MUST be present
      --    If Require_Recovery=True, recovery MUST be present
      --    If Allow_Shamir=True, Shamir shares with threshold can unlock
      --    If all Require_* are False, at least one factor must be present

      if Policy.Primary.Require_Passphrase then
         --  Passphrase is mandatory
         Primary_Satisfied := Has_Passphrase;
      elsif Policy.Primary.Require_Recovery then
         --  Recovery is mandatory
         Primary_Satisfied := Has_Recovery;
      elsif Policy.Primary.Allow_Shamir then
         --  Shamir shares can unlock (check threshold)
         if Has_Shamir and Shamir_Count >= Policy.Primary.Shamir_Threshold then
            Primary_Satisfied := True;
         end if;
      else
         --  Flexible policy: at least one software factor required
         --  (Key-Arena enforces Wrap A presence, so this is always safe)
         Primary_Satisfied := Has_Passphrase or Has_Recovery;
      end if;

      --  Check if both required factors are present (if both flags set)
      if Policy.Primary.Require_Passphrase and Policy.Primary.Require_Recovery then
         Primary_Satisfied := Has_Passphrase and Has_Recovery;
      end if;

      --  Check fast unlock policy (if enabled)
      if Policy.Fast.Enabled then
         if Policy.Fast.Require_TouchID then
            --  Touch ID must be present WITH passphrase (never biometric alone)
            --  This is enforced by type predicate, but double-check at runtime
            if Policy.Fast.Also_Passphrase then
               Fast_Satisfied := Has_TouchID and Has_Passphrase;
            else
               --  Invalid policy (should never happen due to predicate)
               Fast_Satisfied := False;
            end if;
         else
            --  Fast unlock doesn't require Touch ID (unused configuration)
            Fast_Satisfied := False;
         end if;
      end if;

      --  Final decision: Primary satisfied OR Fast satisfied
      --  (Fast unlock is an acceleration path, not a replacement)
      return Primary_Satisfied or Fast_Satisfied;
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
      procedure Append (S : String) is
      begin
         if Pos + S'Length - 1 <= Result'Last then
            Result (Pos .. Pos + S'Length - 1) := S;
            Pos := Pos + S'Length;
         end if;
      end Append;

      --  Helper: Append Natural as decimal string
      procedure Append_Natural (N : Natural) is
         Str : String (1 .. 10) := (others => ' ');
         Val : Natural := N;
         Idx : Positive := Str'Last;
      begin
         if Val = 0 then
            Append ("0");
            return;
         end if;

         while Val > 0 loop
            Str (Idx) := Character'Val (48 + (Val mod 10));
            Val := Val / 10;
            Idx := Idx - 1;
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

   procedure Validate_Policy
     (Policy  : in     Combined_Policy;
      Valid   : out    Boolean;
      Error   : out    Policy_Error;
      Message : out    Error_Message)
   is
   begin
      --  Initialize outputs
      Valid := False;
      Error := No_Error;
      Message := (others => ' ');

      --  Check primary policy: at least one unlock method
      if not Policy.Primary.Require_Passphrase and
         not Policy.Primary.Require_Recovery and
         not Policy.Primary.Allow_Shamir
      then
         --  No unlock method enabled (but should be caught by predicate)
         Error := No_Unlock_Method;
         Message (1 .. 47) := "No unlock method enabled (software-only failed)";
         return;
      end if;

      --  Check Shamir configuration
      if Policy.Primary.Allow_Shamir then
         if Policy.Primary.Shamir_Threshold = 0 then
            Error := Invalid_Shamir_Threshold;
            Message (1 .. 38) := "Shamir threshold is zero (invalid k=0)";
            return;
         end if;
         if Policy.Primary.Shamir_Threshold > 10 then
            Error := Invalid_Shamir_Threshold;
            Message (1 .. 37) := "Shamir threshold too large (max k=10)";
            return;
         end if;
      end if;

      --  Check fast unlock constraints
      if Policy.Fast.Enabled then
         if Policy.Fast.Require_TouchID and not Policy.Fast.Also_Passphrase then
            Error := TouchID_Alone;
            Message (1 .. 52) := "Touch ID requires passphrase (biometric never alone)";
            return;
         end if;

         if Policy.Fast.TTL_Minutes > Max_TTL_Minutes then
            Error := TTL_Too_Long;
            Message (1 .. 39) := "TTL exceeds 24 hours (max 1440 minutes)";
            return;
         end if;
      end if;

      --  All checks passed
      Valid := True;
      Error := No_Error;
      Message (1 .. 18) := "Policy is valid   ";
   end Validate_Policy;

end SparkPass.Vault.Policy;
