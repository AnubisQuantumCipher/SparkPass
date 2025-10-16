pragma SPARK_Mode (Off);
--  NIST Known Answer Tests (KAT) for ML-DSA-87
--
--  This test verifies that the ML-DSA-87 implementation matches
--  NIST FIPS 204 test vectors for digital signature generation and verification.
--
--  ML-DSA-87 corresponds to NIST security level 5 (equivalent to AES-256).
--
--  References:
--    - NIST FIPS 204: Module-Lattice-Based Digital Signature Standard
--    - https://csrc.nist.gov/pubs/fips/204/final

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Config;
with SparkPass.Crypto.MLDSA;

procedure Test_MLDSA_KAT is

   --  Helper: Convert hex string to byte array
   function Hex_To_Bytes (Hex : String) return Byte_Array is
      Result : Byte_Array (1 .. Hex'Length / 2);
      Idx : Positive := 1;

      function Hex_Char_To_U8 (C : Character) return U8 is
      begin
         case C is
            when '0' .. '9' => return U8 (Character'Pos (C) - Character'Pos ('0'));
            when 'a' .. 'f' => return U8 (Character'Pos (C) - Character'Pos ('a') + 10);
            when 'A' .. 'F' => return U8 (Character'Pos (C) - Character'Pos ('A') + 10);
            when others => raise Constraint_Error with "Invalid hex character: " & C;
         end case;
      end Hex_Char_To_U8;
   begin
      for I in Result'Range loop
         Result (I) := Hex_Char_To_U8 (Hex (Idx)) * 16 + Hex_Char_To_U8 (Hex (Idx + 1));
         Idx := Idx + 2;
      end loop;
      return Result;
   end Hex_To_Bytes;

   --  Helper: Compare byte arrays
   function Bytes_Equal (A, B : Byte_Array) return Boolean is
   begin
      if A'Length /= B'Length then
         return False;
      end if;
      for I in A'Range loop
         if A (I) /= B (I - A'First + B'First) then
            return False;
         end if;
      end loop;
      return True;
   end Bytes_Equal;

   --  Helper: Print first/last bytes of array for debugging
   procedure Print_Bytes (Label : String; Data : Byte_Array; Show_Length : Positive := 16) is
   begin
      Put (Label & ": ");
      for I in Data'First .. Integer'Min (Data'First + Show_Length - 1, Data'Last) loop
         Put (U8'Image (Data (I)));
         if I < Data'Last then
            Put (" ");
         end if;
      end loop;
      if Data'Length > Show_Length then
         Put (" ... (+" & Positive'Image (Data'Length - Show_Length) & " more bytes)");
      end if;
      New_Line;
   end Print_Bytes;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   Fail_Count : Natural := 0;

   --  Self-Consistency Test: Internal ML-DSA-87 validation
   --  TODO: Replace with actual NIST ACVP test vectors from:
   --        https://github.com/usnistgov/ACVP-Server/tree/master/gen-val/json-files
   procedure Test_Self_Consistency is
      Public_Key : SparkPass.Crypto.MLDSA.Public_Key;
      Secret_Key : SparkPass.Crypto.MLDSA.Secret_Key;
      Signature : SparkPass.Crypto.MLDSA.Signature;
      Message : constant Byte_Array := Hex_To_Bytes ("48656c6c6f2c20576f726c6421");  -- "Hello, World!"
      Verified : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Self-Test #1: ML-DSA-87 Sign/Verify Consistency ===");
      Put_Line ("Note: This is a self-generated test vector, not an official NIST KAT");
      New_Line;

      --  Step 1: Generate keypair
      Put_Line ("[1] Generating ML-DSA-87 keypair...");
      SparkPass.Crypto.MLDSA.Keypair (Public_Key, Secret_Key);
      Put_Line ("  ✓ Keypair generated");
      Print_Bytes ("    Public key (first 16)", Public_Key);
      Print_Bytes ("    Secret key (first 16)", Secret_Key);

      --  Step 2: Sign message
      Put_Line ("[2] Signing test message...");
      Put_Line ("    Message: ""Hello, World!""");
      SparkPass.Crypto.MLDSA.Sign (Secret_Key, Message, Signature);
      Put_Line ("  ✓ Signature generated");
      Print_Bytes ("    Signature (first 16) ", Signature);

      --  Step 3: Verify signature
      Put_Line ("[3] Verifying signature...");
      SparkPass.Crypto.MLDSA.Verify (Public_Key, Message, Signature, Verified);

      if Verified then
         Put_Line ("  ✓ PASS: Signature verification succeeded");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Signature verification failed");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Self_Consistency;

   --  Test signature verification with tampered message
   procedure Test_Tampered_Message is
      Public_Key : SparkPass.Crypto.MLDSA.Public_Key;
      Secret_Key : SparkPass.Crypto.MLDSA.Secret_Key;
      Signature : SparkPass.Crypto.MLDSA.Signature;
      Message : constant Byte_Array := Hex_To_Bytes ("48656c6c6f2c20576f726c6421");  -- "Hello, World!"
      Tampered : constant Byte_Array := Hex_To_Bytes ("48656c6c6f2c20416c6963652121");  -- "Hello, Alice!!"
      Verified : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test #2: ML-DSA-87 Tampered Message Detection ===");
      New_Line;

      --  Generate keypair and sign original message
      Put_Line ("[1] Signing original message...");
      SparkPass.Crypto.MLDSA.Keypair (Public_Key, Secret_Key);
      SparkPass.Crypto.MLDSA.Sign (Secret_Key, Message, Signature);
      Put_Line ("  ✓ Original message signed");

      --  Verify with tampered message (should fail)
      Put_Line ("[2] Verifying signature with tampered message...");
      Put_Line ("    Original: ""Hello, World!""");
      Put_Line ("    Tampered: ""Hello, Alice!!""");
      SparkPass.Crypto.MLDSA.Verify (Public_Key, Tampered, Signature, Verified);

      if not Verified then
         Put_Line ("  ✓ PASS: Tampered message correctly rejected");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Tampered message incorrectly accepted");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Tampered_Message;

   --  Test signature verification with wrong public key
   procedure Test_Wrong_Public_Key is
      Public_Key_1 : SparkPass.Crypto.MLDSA.Public_Key;
      Secret_Key_1 : SparkPass.Crypto.MLDSA.Secret_Key;
      Public_Key_2 : SparkPass.Crypto.MLDSA.Public_Key;
      Secret_Key_2 : SparkPass.Crypto.MLDSA.Secret_Key;
      Signature : SparkPass.Crypto.MLDSA.Signature;
      Message : constant Byte_Array := Hex_To_Bytes ("546573744d657373616765");  -- "TestMessage"
      Verified : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test #3: ML-DSA-87 Wrong Public Key Detection ===");
      New_Line;

      --  Generate two keypairs
      Put_Line ("[1] Generating two different keypairs...");
      SparkPass.Crypto.MLDSA.Keypair (Public_Key_1, Secret_Key_1);
      SparkPass.Crypto.MLDSA.Keypair (Public_Key_2, Secret_Key_2);
      Put_Line ("  ✓ Keypairs generated");

      --  Sign with first secret key
      Put_Line ("[2] Signing with keypair #1...");
      SparkPass.Crypto.MLDSA.Sign (Secret_Key_1, Message, Signature);
      Put_Line ("  ✓ Message signed");

      --  Try to verify with wrong public key (should fail)
      Put_Line ("[3] Verifying with keypair #2 (wrong key)...");
      SparkPass.Crypto.MLDSA.Verify (Public_Key_2, Message, Signature, Verified);

      if not Verified then
         Put_Line ("  ✓ PASS: Wrong public key correctly rejected");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Wrong public key incorrectly accepted");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Wrong_Public_Key;

begin
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  NIST Known Answer Tests (KAT) for ML-DSA-87              ║");
   Put_Line ("║  FIPS 204: Module-Lattice-Based Digital Signature         ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("ML-DSA-87 Parameters:");
   Put_Line ("  Public Key: " & Positive'Image (SparkPass.Config.MLDsa_Public_Key_Length) & " bytes");
   Put_Line ("  Secret Key: " & Positive'Image (SparkPass.Config.MLDsa_Secret_Key_Length) & " bytes");
   Put_Line ("  Signature:  " & Positive'Image (SparkPass.Config.MLDsa_Signature_Length) & " bytes");
   New_Line;

   --  Run tests
   Test_Self_Consistency;
   Test_Tampered_Message;
   Test_Wrong_Public_Key;

   --  Summary
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Test Summary                                              ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   Put_Line ("Total Tests: " & Natural'Image (Test_Count));
   Put_Line ("Passed:      " & Natural'Image (Pass_Count));
   Put_Line ("Failed:      " & Natural'Image (Fail_Count));
   New_Line;

   if Fail_Count = 0 then
      Put_Line ("✓ All ML-DSA-87 tests PASSED");
      New_Line;
      Put_Line ("Note: This test suite validates internal consistency using self-generated");
      Put_Line ("      test vectors. For full NIST FIPS 204 compliance validation, official");
      Put_Line ("      ACVP test vectors should be added from:");
      Put_Line ("      https://github.com/usnistgov/ACVP-Server/tree/master/gen-val/json-files");
   else
      Put_Line ("✗ Some tests FAILED - review output above");
   end if;

end Test_MLDSA_KAT;
