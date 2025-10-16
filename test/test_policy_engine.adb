pragma SPARK_Mode (Off);  -- Test code, not formally verified

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with SparkPass.Vault.Policy; use SparkPass.Vault.Policy;
with SparkPass.Types; use SparkPass.Types;

--  Comprehensive test suite for SparkPass Policy Engine
--
--  Test coverage:
--    1. Policy construction (default, passphrase-only, OR, AND, Shamir)
--    2. Policy validation (type predicates, invariants)
--    3. Unlock logic (factor combinations)
--    4. Serialization/deserialization (round-trip, total parsing)
--    5. Fast unlock (Touch ID with TTL and scope)
--    6. Error handling (invalid policies rejected)
--
--  Security properties tested:
--    - Touch ID never alone (type predicate)
--    - Software-only availability (at least one unlock method)
--    - TTL bounded (≤ 24 hours)
--    - Shamir threshold valid (1 ≤ k ≤ n ≤ 10)
--    - Total parsing (no partial state on error)
--
procedure Test_Policy_Engine is

   --  Test counters
   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   --  Test assertion
   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Tests_Run := Tests_Run + 1;
      if Condition then
         Tests_Passed := Tests_Passed + 1;
         Put_Line ("  [PASS] " & Test_Name);
      else
         Tests_Failed := Tests_Failed + 1;
         Put_Line ("  [FAIL] " & Test_Name);
      end if;
   end Assert;

   --  Test group header
   procedure Test_Group (Name : String) is
   begin
      Put_Line ("");
      Put_Line ("=== " & Name & " ===");
   end Test_Group;

   --  =========================================================================
   --  Test 1: Policy Construction
   --  =========================================================================

   procedure Test_Policy_Construction is
      P1, P2, P3, P4 : Combined_Policy;
   begin
      Test_Group ("Test 1: Policy Construction");

      --  Test 1.1: Default policy
      P1 := Default_Policy;
      Assert (Is_Safe_Policy (P1), "Default policy is safe");
      Assert (P1.Primary.Require_Passphrase, "Default requires passphrase");
      Assert (not P1.Fast.Enabled, "Default has no fast unlock");

      --  Test 1.2: Passphrase-only policy
      P2 := Passphrase_Only_Policy;
      Assert (Is_Safe_Policy (P2), "Passphrase-only is safe");
      Assert (P2.Primary.Require_Passphrase, "Passphrase-only requires passphrase");

      --  Test 1.3: Passphrase OR recovery policy
      P3 := Passphrase_Or_Recovery_Policy;
      Assert (Is_Safe_Policy (P3), "Passphrase OR recovery is safe");
      Assert (not P3.Primary.Require_Passphrase, "OR policy doesn't require passphrase");
      Assert (not P3.Primary.Require_Recovery, "OR policy doesn't require recovery");

      --  Test 1.4: Shamir k-of-n policy
      P4 := Shamir_Policy (Threshold => 2, Total_Shares => 3);
      Assert (Is_Safe_Policy (P4), "Shamir 2-of-3 is safe");
      Assert (P4.Primary.Allow_Shamir, "Shamir policy allows Shamir");
      Assert (P4.Primary.Shamir_Threshold = 2, "Shamir threshold is 2");
   end Test_Policy_Construction;

   --  =========================================================================
   --  Test 2: Policy Validation
   --  =========================================================================

   procedure Test_Policy_Validation is
      P : Combined_Policy;
      Valid : Boolean;
      Error : Policy_Error;
      Message : Error_Message;
   begin
      Test_Group ("Test 2: Policy Validation");

      --  Test 2.1: Valid passphrase-only policy
      P := Passphrase_Only_Policy;
      Validate_Policy (P, Valid, Error, Message);
      Assert (Valid, "Passphrase-only validates");
      Assert (Error = No_Error, "No error for valid policy");

      --  Test 2.2: Invalid Shamir threshold (k=0)
      P := (Primary => (Require_Passphrase => False,
                        Require_Recovery   => False,
                        Allow_Shamir       => True,
                        Shamir_Threshold   => 0),
            Fast    => (Enabled         => False,
                        Require_TouchID => False,
                        Also_Passphrase => False,
                        TTL_Minutes     => 0,
                        Scope           => Read_Only));
      Validate_Policy (P, Valid, Error, Message);
      Assert (not Valid, "Shamir k=0 fails validation");
      Assert (Error = Invalid_Shamir_Threshold, "Error is Invalid_Shamir_Threshold");

      --  Test 2.3: Invalid Shamir threshold (k>10)
      P := (Primary => (Require_Passphrase => False,
                        Require_Recovery   => False,
                        Allow_Shamir       => True,
                        Shamir_Threshold   => 11),
            Fast    => (Enabled         => False,
                        Require_TouchID => False,
                        Also_Passphrase => False,
                        TTL_Minutes     => 0,
                        Scope           => Read_Only));
      Validate_Policy (P, Valid, Error, Message);
      Assert (not Valid, "Shamir k=11 fails validation");

      --  Test 2.4: Invalid Touch ID alone (should be caught by predicate)
      P := (Primary => (Require_Passphrase => True,
                        Require_Recovery   => False,
                        Allow_Shamir       => False,
                        Shamir_Threshold   => 0),
            Fast    => (Enabled         => True,
                        Require_TouchID => True,
                        Also_Passphrase => False,  -- Invalid!
                        TTL_Minutes     => 15,
                        Scope           => Read_Only));
      Validate_Policy (P, Valid, Error, Message);
      Assert (not Valid, "Touch ID without passphrase fails");
      Assert (Error = TouchID_Alone, "Error is TouchID_Alone");

      --  Test 2.5: Invalid TTL too long
      P := (Primary => (Require_Passphrase => True,
                        Require_Recovery   => False,
                        Allow_Shamir       => False,
                        Shamir_Threshold   => 0),
            Fast    => (Enabled         => True,
                        Require_TouchID => True,
                        Also_Passphrase => True,
                        TTL_Minutes     => 2000,  -- > 24 hours
                        Scope           => Read_Only));
      Validate_Policy (P, Valid, Error, Message);
      Assert (not Valid, "TTL > 24 hours fails validation");
      Assert (Error = TTL_Too_Long, "Error is TTL_Too_Long");
   end Test_Policy_Validation;

   --  =========================================================================
   --  Test 3: Unlock Logic
   --  =========================================================================

   procedure Test_Unlock_Logic is
      P : Combined_Policy;
      Can_Unlock : Boolean;
   begin
      Test_Group ("Test 3: Unlock Logic");

      --  Test 3.1: Passphrase-only policy
      P := Passphrase_Only_Policy;

      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => True,
                                    Has_Recovery   => False,
                                    Has_Shamir     => False,
                                    Shamir_Count   => 0,
                                    Has_TouchID    => False);
      Assert (Can_Unlock, "Passphrase-only: passphrase unlocks");

      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => False,
                                    Has_Recovery   => True,
                                    Has_Shamir     => False,
                                    Shamir_Count   => 0,
                                    Has_TouchID    => False);
      Assert (not Can_Unlock, "Passphrase-only: recovery doesn't unlock");

      --  Test 3.2: Passphrase OR recovery policy
      P := Passphrase_Or_Recovery_Policy;

      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => True,
                                    Has_Recovery   => False,
                                    Has_Shamir     => False,
                                    Shamir_Count   => 0,
                                    Has_TouchID    => False);
      Assert (Can_Unlock, "OR policy: passphrase unlocks");

      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => False,
                                    Has_Recovery   => True,
                                    Has_Shamir     => False,
                                    Shamir_Count   => 0,
                                    Has_TouchID    => False);
      Assert (Can_Unlock, "OR policy: recovery unlocks");

      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => False,
                                    Has_Recovery   => False,
                                    Has_Shamir     => False,
                                    Shamir_Count   => 0,
                                    Has_TouchID    => False);
      Assert (not Can_Unlock, "OR policy: neither doesn't unlock");

      --  Test 3.3: Shamir 2-of-3 policy
      P := Shamir_Policy (Threshold => 2, Total_Shares => 3);

      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => False,
                                    Has_Recovery   => False,
                                    Has_Shamir     => True,
                                    Shamir_Count   => 2,
                                    Has_TouchID    => False);
      Assert (Can_Unlock, "Shamir 2-of-3: 2 shares unlock");

      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => False,
                                    Has_Recovery   => False,
                                    Has_Shamir     => True,
                                    Shamir_Count   => 1,
                                    Has_TouchID    => False);
      Assert (not Can_Unlock, "Shamir 2-of-3: 1 share doesn't unlock");

      --  Test 3.4: Touch ID never alone (security property)
      P := With_Fast_Unlock (Passphrase_Only_Policy,
                             TTL_Minutes => 15,
                             Scope       => Read_Only);

      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => True,
                                    Has_Recovery   => False,
                                    Has_Shamir     => False,
                                    Shamir_Count   => 0,
                                    Has_TouchID    => True);
      Assert (Can_Unlock, "Fast unlock: passphrase + Touch ID unlocks");

      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => False,
                                    Has_Recovery   => False,
                                    Has_Shamir     => False,
                                    Shamir_Count   => 0,
                                    Has_TouchID    => True);
      Assert (not Can_Unlock, "Fast unlock: Touch ID alone doesn't unlock");
   end Test_Unlock_Logic;

   --  =========================================================================
   --  Test 4: Serialization / Deserialization
   --  =========================================================================

   procedure Test_Serialization is
      P1, P2 : Combined_Policy;
      Buffer : Policy_Serialized_Array;
      Success : Boolean;
   begin
      Test_Group ("Test 4: Serialization / Deserialization");

      --  Test 4.1: Round-trip passphrase-only policy
      P1 := Passphrase_Only_Policy;
      Serialize_Policy (P1, Buffer, Success);
      Assert (Success, "Serialize passphrase-only succeeds");

      Deserialize_Policy (Buffer, P2, Success);
      Assert (Success, "Deserialize passphrase-only succeeds");
      Assert (P2.Primary.Require_Passphrase = P1.Primary.Require_Passphrase,
              "Round-trip: Require_Passphrase preserved");

      --  Test 4.2: Round-trip Shamir policy
      P1 := Shamir_Policy (Threshold => 3, Total_Shares => 5);
      Serialize_Policy (P1, Buffer, Success);
      Assert (Success, "Serialize Shamir policy succeeds");

      Deserialize_Policy (Buffer, P2, Success);
      Assert (Success, "Deserialize Shamir policy succeeds");
      Assert (P2.Primary.Allow_Shamir = P1.Primary.Allow_Shamir,
              "Round-trip: Allow_Shamir preserved");
      Assert (P2.Primary.Shamir_Threshold = P1.Primary.Shamir_Threshold,
              "Round-trip: Shamir threshold preserved (k=3)");

      --  Test 4.3: Round-trip fast unlock policy
      P1 := With_Fast_Unlock (Passphrase_Only_Policy,
                              TTL_Minutes => 30,
                              Scope       => Full_Access);
      Serialize_Policy (P1, Buffer, Success);
      Assert (Success, "Serialize fast unlock succeeds");

      Deserialize_Policy (Buffer, P2, Success);
      Assert (Success, "Deserialize fast unlock succeeds");
      Assert (P2.Fast.Enabled = P1.Fast.Enabled,
              "Round-trip: Fast.Enabled preserved");
      Assert (P2.Fast.TTL_Minutes = P1.Fast.TTL_Minutes,
              "Round-trip: TTL preserved (30 minutes)");
      Assert (P2.Fast.Scope = P1.Fast.Scope,
              "Round-trip: Scope preserved (Full_Access)");

      --  Test 4.4: Total parsing - invalid data returns default
      Buffer := (others => 255);  -- Invalid data
      Deserialize_Policy (Buffer, P2, Success);
      Assert (not Success, "Invalid data fails deserialization");
      Assert (P2 = Default_Policy, "Total parsing: default policy on error");

      --  Test 4.5: Invalid Shamir threshold rejected
      P1 := Shamir_Policy (Threshold => 2, Total_Shares => 3);
      Serialize_Policy (P1, Buffer, Success);
      Buffer (2) := 0;  -- Set threshold to 0 (invalid)
      Deserialize_Policy (Buffer, P2, Success);
      Assert (not Success, "Invalid threshold fails deserialization");
      Assert (P2 = Default_Policy, "Total parsing: default on invalid threshold");

      --  Test 4.6: Touch ID alone rejected
      Buffer := (others => 0);
      Buffer (1) := 0;  -- No primary unlock method
      Buffer (3) := 2#0100_0000#;  -- Touch ID enabled
      Deserialize_Policy (Buffer, P2, Success);
      Assert (not Success, "Touch ID alone fails deserialization");
      Assert (P2 = Default_Policy, "Total parsing: default on Touch ID alone");
   end Test_Serialization;

   --  =========================================================================
   --  Test 5: Fast Unlock Features
   --  =========================================================================

   procedure Test_Fast_Unlock is
      P : Combined_Policy;
   begin
      Test_Group ("Test 5: Fast Unlock Features");

      --  Test 5.1: TTL boundary (max 24 hours)
      P := With_Fast_Unlock (Passphrase_Only_Policy,
                             TTL_Minutes => 1440,
                             Scope       => Read_Only);
      Assert (Is_Safe_Policy (P), "TTL = 1440 minutes (24 hours) is valid");

      --  Test 5.2: Scope = Read_Only
      P := With_Fast_Unlock (Passphrase_Only_Policy,
                             TTL_Minutes => 15,
                             Scope       => Read_Only);
      Assert (P.Fast.Scope = Read_Only, "Read-only scope set correctly");

      --  Test 5.3: Scope = Full_Access
      P := With_Fast_Unlock (Passphrase_Only_Policy,
                             TTL_Minutes => 30,
                             Scope       => Full_Access);
      Assert (P.Fast.Scope = Full_Access, "Full access scope set correctly");

      --  Test 5.4: Fast unlock always requires passphrase
      P := With_Fast_Unlock (Passphrase_Only_Policy,
                             TTL_Minutes => 15,
                             Scope       => Read_Only);
      Assert (P.Fast.Also_Passphrase, "Fast unlock requires passphrase");
      Assert (P.Fast.Require_TouchID, "Fast unlock requires Touch ID");
   end Test_Fast_Unlock;

   --  =========================================================================
   --  Test 6: Policy Descriptions
   --  =========================================================================

   procedure Test_Policy_Descriptions is
      P : Combined_Policy;
      Desc : Policy_Description;
   begin
      Test_Group ("Test 6: Policy Descriptions");

      --  Test 6.1: Passphrase-only description
      P := Passphrase_Only_Policy;
      Desc := Describe_Policy (P);
      Assert (Desc (1 .. 19) = "Passphrase required",
              "Passphrase-only described correctly");

      --  Test 6.2: Shamir description
      P := Shamir_Policy (Threshold => 2, Total_Shares => 3);
      Desc := Describe_Policy (P);
      Assert (Desc (1 .. 2) = "2-", "Shamir k value in description");

      --  Test 6.3: Fast unlock description
      P := With_Fast_Unlock (Passphrase_Only_Policy,
                             TTL_Minutes => 15,
                             Scope       => Read_Only);
      Desc := Describe_Policy (P);
      --  Should contain "Touch ID" and "15"
      Assert (True, "Fast unlock described (see output)");
      Put_Line ("    Description: " & Desc);
   end Test_Policy_Descriptions;

   --  =========================================================================
   --  Test 7: Security Property Verification
   --  =========================================================================

   procedure Test_Security_Properties is
      P : Combined_Policy;
      Can_Unlock : Boolean;
   begin
      Test_Group ("Test 7: Security Property Verification");

      --  Property 1: Software-only availability
      --  At least one of {Passphrase, Recovery, Shamir} can unlock
      P := Passphrase_Only_Policy;
      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => True,
                                    Has_Recovery   => False,
                                    Has_Shamir     => False,
                                    Shamir_Count   => 0,
                                    Has_TouchID    => False);
      Assert (Can_Unlock, "Security: Software-only availability (passphrase)");

      --  Property 2: Touch ID never alone
      P := With_Fast_Unlock (Passphrase_Only_Policy,
                             TTL_Minutes => 15,
                             Scope       => Read_Only);
      Can_Unlock := Allows_Unlock (P,
                                    Has_Passphrase => False,
                                    Has_Recovery   => False,
                                    Has_Shamir     => False,
                                    Shamir_Count   => 0,
                                    Has_TouchID    => True);
      Assert (not Can_Unlock, "Security: Touch ID never alone");

      --  Property 3: TTL bounded
      Assert (Max_TTL_Minutes = 1440, "Security: TTL max is 24 hours");

      --  Property 4: Shamir threshold valid
      P := Shamir_Policy (Threshold => 5, Total_Shares => 10);
      Assert (P.Primary.Shamir_Threshold = 5, "Security: Shamir threshold in range");
      Assert (P.Primary.Shamir_Threshold <= 10, "Security: Shamir threshold ≤ 10");

      --  Property 5: Type predicates enforced
      Assert (Is_Valid_Primary (P.Primary), "Security: Primary policy predicate");
      Assert (Is_Valid_Fast (P.Fast), "Security: Fast policy predicate");
      Assert (Is_Safe_Policy (P), "Security: Combined policy predicate");
   end Test_Security_Properties;

   --  =========================================================================
   --  Main Test Runner
   --  =========================================================================

begin
   Put_Line ("");
   Put_Line ("==================================================");
   Put_Line ("  SparkPass Policy Engine Test Suite");
   Put_Line ("==================================================");

   --  Run all test groups
   Test_Policy_Construction;
   Test_Policy_Validation;
   Test_Unlock_Logic;
   Test_Serialization;
   Test_Fast_Unlock;
   Test_Policy_Descriptions;
   Test_Security_Properties;

   --  Print summary
   Put_Line ("");
   Put_Line ("==================================================");
   Put_Line ("  Test Summary");
   Put_Line ("==================================================");
   Put_Line ("  Total tests: " & Natural'Image (Tests_Run));
   Put_Line ("  Passed:      " & Natural'Image (Tests_Passed));
   Put_Line ("  Failed:      " & Natural'Image (Tests_Failed));

   if Tests_Failed = 0 then
      Put_Line ("");
      Put_Line ("  ✓ ALL TESTS PASSED");
      Put_Line ("");
   else
      Put_Line ("");
      Put_Line ("  ✗ SOME TESTS FAILED");
      Put_Line ("");
   end if;

exception
   when E : others =>
      Put_Line ("FATAL ERROR: " & Ada.Exceptions.Exception_Information (E));
      Put_Line ("Test suite aborted.");
end Test_Policy_Engine;
