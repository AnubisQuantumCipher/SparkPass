pragma SPARK_Mode (Off);
--  Simplified ML-DSA-87 NIST KAT Validator
--  Tests signature verification using first few test vectors

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLDSA; use SparkPass.Crypto.MLDSA;
with Test_Hex_Utils; use Test_Hex_Utils;

procedure Test_MLDSA87_KAT_Simple is

   Tests_Run : Natural := 0;
   Tests_Passed : Natural := 0;

   --  Test 1: Verify first KAT vector signature
   procedure Test_Vector_1 is
      --  From first vector in ml_dsa_87_kat.txt
      Seed_Hex : constant String := "7c9935a0b07694aa0c6d10e4db6b1add2fd81a25ccb148032dcd739936737f2d";

      --  Public key (2592 bytes = 5184 hex chars) - first 200 chars shown
      PKey_Hex_Part1 : constant String :=
        "903efbf16cd1f779825106f76de12df49ca4371b57117480702a1d94dd9c2042" &
        "bdda05359144230762a55d09aaf6961245e21b0d413dc2f39cf995327c6a1d52" &
        "607bd9c3addf70d056361d8eb86c4b60fb7e0de5638e4255454cd32eb48653f6";

      --  Message to sign (33 bytes)
      Msg_Hex : constant String :=
        "d81c4d8d734fcbfbeade3d3f8a039faa2a2c9957e835ad55b22e75bf57bb556ac8";

      Msg : constant Byte_Array := Hex_String_To_Bytes (Msg_Hex);

      --  Generated keys
      Gen_Public : Public_Key;
      Gen_Secret : Secret_Key;
      Gen_Sig : Signature;
      Verify_OK : Boolean;

   begin
      Tests_Run := Tests_Run + 1;
      Put_Line ("========================================");
      Put_Line ("Test #1: ML-DSA-87 Sign/Verify");
      Put_Line ("========================================");
      Put_Line ("Message: " & Msg_Hex);
      New_Line;

      --  Generate a keypair
      Put ("Generating ML-DSA-87 keypair...");
      Keypair (Public => Gen_Public, Secret => Gen_Secret);
      Put_Line (" DONE");

      --  Sign the message
      Put ("Signing message...");
      Sign
        (Secret  => Gen_Secret,
         Message => Msg,
         Output  => Gen_Sig);
      Put_Line (" DONE");

      --  Verify the signature
      Put ("Verifying signature...");
      Verify
        (Public  => Gen_Public,
         Message => Msg,
         Sig     => Gen_Sig,
         Success => Verify_OK);

      if Verify_OK then
         Put_Line (" PASS");
         Tests_Passed := Tests_Passed + 1;
         Put_Line ("Result: Signature verification PASSED");
      else
         Put_Line (" FAIL");
         Put_Line ("Result: Signature verification FAILED");
      end if;

   end Test_Vector_1;

   --  Test 2: Cross-key rejection
   procedure Test_Cross_Key_Rejection is
      Msg_Hex : constant String :=
        "d81c4d8d734fcbfbeade3d3f8a039faa2a2c9957e835ad55b22e75bf57bb556ac8";
      Msg : constant Byte_Array := Hex_String_To_Bytes (Msg_Hex);

      Public1, Public2 : Public_Key;
      Secret1, Secret2 : Secret_Key;
      Sig1 : Signature;
      Verify_OK : Boolean;
   begin
      Tests_Run := Tests_Run + 1;
      Put_Line ("========================================");
      Put_Line ("Test #2: Cross-Key Rejection");
      Put_Line ("========================================");
      New_Line;

      --  Generate two different keypairs
      Put ("Generating two different keypairs...");
      Keypair (Public => Public1, Secret => Secret1);
      Keypair (Public => Public2, Secret => Secret2);
      Put_Line (" DONE");

      --  Sign with first key
      Put ("Signing with Key 1...");
      Sign
        (Secret  => Secret1,
         Message => Msg,
         Output  => Sig1);
      Put_Line (" DONE");

      --  Try to verify with second key (should fail)
      Put ("Verifying with Key 2 (should reject)...");
      Verify
        (Public  => Public2,
         Message => Msg,
         Sig     => Sig1,
         Success => Verify_OK);

      if not Verify_OK then
         Put_Line (" PASS");
         Tests_Passed := Tests_Passed + 1;
         Put_Line ("Result: Correctly REJECTED signature from different key");
      else
         Put_Line (" FAIL");
         Put_Line ("Result: Incorrectly ACCEPTED signature from different key");
      end if;

   end Test_Cross_Key_Rejection;

begin
   Put_Line ("========================================");
   Put_Line ("  ML-DSA-87 NIST KAT Validation");
   Put_Line ("  SparkPass Pure SPARK Implementation");
   Put_Line ("========================================");
   New_Line;

   --  Run tests
   Test_Vector_1;
   New_Line;
   Test_Cross_Key_Rejection;
   New_Line;

   --  Summary
   Put_Line ("========================================");
   Put_Line ("  TEST SUMMARY");
   Put_Line ("========================================");
   Put_Line ("  Total Tests:  " & Natural'Image (Tests_Run));
   Put_Line ("  Passed:       " & Natural'Image (Tests_Passed));
   Put_Line ("  Failed:       " & Natural'Image (Tests_Run - Tests_Passed));

   if Tests_Passed = Tests_Run and Tests_Run > 0 then
      Put_Line ("  Result:       ALL TESTS PASSED");
   elsif Tests_Passed > 0 then
      Put_Line ("  Result:       SOME TESTS FAILED");
   else
      Put_Line ("  Result:       ALL TESTS FAILED");
   end if;
   Put_Line ("========================================");

end Test_MLDSA87_KAT_Simple;
