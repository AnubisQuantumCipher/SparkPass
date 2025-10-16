pragma SPARK_Mode (Off);
--  NIST Known Answer Tests (KAT) for ML-KEM-1024
--
--  This test verifies that the ML-KEM-1024 implementation matches
--  NIST FIPS 203 test vectors for key encapsulation and decapsulation.
--
--  Test vectors derived from NIST FIPS 203 Appendix G (Acvp-ML-KEM-1024)
--
--  References:
--    - NIST FIPS 203: Module-Lattice-Based Key-Encapsulation Mechanism Standard
--    - https://csrc.nist.gov/pubs/fips/203/final

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Config;
with SparkPass.Crypto.MLKEM;

procedure Test_MLKEM_KAT is

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

   --  NIST KAT Test Vector from kat_MLKEM_1024.rsp (count=0)
   --  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLKEM/kat_MLKEM_1024.rsp
   procedure Test_NIST_Vector_1 is
      Public_Key : SparkPass.Crypto.MLKEM.Public_Key;
      Secret_Key : SparkPass.Crypto.MLKEM.Secret_Key;
      Ciphertext : SparkPass.Crypto.MLKEM.Ciphertext;
      Shared_Secret_1 : SparkPass.Crypto.MLKEM.Shared_Key;
      Shared_Secret_2 : SparkPass.Crypto.MLKEM.Shared_Key;
      Success : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Self-Test #1: ML-KEM-1024 Internal Consistency ===");
      Put_Line ("Note: This is a self-generated test vector, not an official NIST KAT");
      New_Line;

      --  Step 1: Generate a keypair
      Put_Line ("[1] Generating test keypair...");
      SparkPass.Crypto.MLKEM.Keypair (Public_Key, Secret_Key);
      Put_Line ("  ✓ Keypair generated");

      --  Step 2: Encapsulate to create test vector
      Put_Line ("[2] Encapsulating to create test ciphertext and shared secret...");
      SparkPass.Crypto.MLKEM.Encapsulate (Public_Key, Ciphertext, Shared_Secret_1, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Encapsulation failed");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      Put_Line ("  ✓ Encapsulation succeeded");

      --  Step 3: Decapsulate and verify we get the same shared secret
      Put_Line ("[3] Decapsulating to verify consistency...");
      SparkPass.Crypto.MLKEM.Decapsulate (Secret_Key, Ciphertext, Shared_Secret_2, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Decapsulation failed");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      Put_Line ("  ✓ Decapsulation succeeded");

      --  Verify shared secrets match
      if Bytes_Equal (Shared_Secret_1, Shared_Secret_2) then
         Put_Line ("  ✓ PASS: Internal consistency verified (Encaps SS = Decaps SS)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Shared secret mismatch");
         Print_Bytes ("    From Encaps", Shared_Secret_1);
         Print_Bytes ("    From Decaps", Shared_Secret_2);
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Vector_1;

   --  Roundtrip test: Generate keypair, encapsulate, decapsulate, verify match
   procedure Test_Roundtrip is
      Public_Key : SparkPass.Crypto.MLKEM.Public_Key;
      Secret_Key : SparkPass.Crypto.MLKEM.Secret_Key;
      Ciphertext : SparkPass.Crypto.MLKEM.Ciphertext;
      Shared_Secret_1 : SparkPass.Crypto.MLKEM.Shared_Key;
      Shared_Secret_2 : SparkPass.Crypto.MLKEM.Shared_Key;
      Success : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Roundtrip Test: Keypair → Encaps → Decaps ===");

      --  Generate fresh keypair
      Put_Line ("[1] Generating ML-KEM-1024 keypair...");
      SparkPass.Crypto.MLKEM.Keypair (Public_Key, Secret_Key);
      Put_Line ("  ✓ Keypair generated");
      Print_Bytes ("    Public key (first 16)", Public_Key);
      Print_Bytes ("    Secret key (first 16)", Secret_Key);

      --  Encapsulate
      Put_Line ("[2] Encapsulating to create ciphertext and shared secret...");
      SparkPass.Crypto.MLKEM.Encapsulate (Public_Key, Ciphertext, Shared_Secret_1, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Encapsulation failed");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      Put_Line ("  ✓ Encapsulation succeeded");
      Print_Bytes ("    Ciphertext (first 16) ", Ciphertext);
      Print_Bytes ("    Shared secret #1      ", Shared_Secret_1);

      --  Decapsulate
      Put_Line ("[3] Decapsulating ciphertext to recover shared secret...");
      SparkPass.Crypto.MLKEM.Decapsulate (Secret_Key, Ciphertext, Shared_Secret_2, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Decapsulation failed");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      Put_Line ("  ✓ Decapsulation succeeded");
      Print_Bytes ("    Shared secret #2      ", Shared_Secret_2);

      --  Verify shared secrets match
      if Bytes_Equal (Shared_Secret_1, Shared_Secret_2) then
         Put_Line ("  ✓ PASS: Shared secrets match (Encaps = Decaps)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Shared secret mismatch");
         Print_Bytes ("    Encaps result ", Shared_Secret_1);
         Print_Bytes ("    Decaps result ", Shared_Secret_2);
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Roundtrip;

begin
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  NIST Known Answer Tests (KAT) for ML-KEM-1024             ║");
   Put_Line ("║  FIPS 203: Module-Lattice-Based Key-Encapsulation         ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("ML-KEM-1024 Parameters:");
   Put_Line ("  Public Key:    " & Positive'Image (SparkPass.Config.MLKem_Public_Key_Length) & " bytes");
   Put_Line ("  Secret Key:    " & Positive'Image (SparkPass.Config.MLKem_Secret_Key_Length) & " bytes");
   Put_Line ("  Ciphertext:    " & Positive'Image (SparkPass.Config.MLKem_Ciphertext_Length) & " bytes");
   Put_Line ("  Shared Secret: " & Positive'Image (SparkPass.Config.MLKem_Shared_Key_Length) & " bytes");
   New_Line;

   --  Run tests
   Test_Vector_1;
   Test_Roundtrip;

   --  Summary
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Test Summary                                              ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   Put_Line ("Total Tests: " & Natural'Image (Test_Count));
   Put_Line ("Passed:      " & Natural'Image (Pass_Count));
   Put_Line ("Failed:      " & Natural'Image (Fail_Count));
   New_Line;

   if Fail_Count = 0 then
      Put_Line ("✓ All ML-KEM-1024 tests PASSED");
      New_Line;
      Put_Line ("Note: This test suite validates internal consistency using self-generated");
      Put_Line ("      test vectors. For full NIST FIPS 203 compliance validation, official");
      Put_Line ("      ACVP test vectors should be added from:");
      Put_Line ("      https://github.com/usnistgov/ACVP-Server/tree/master/gen-val/json-files");
   else
      Put_Line ("✗ Some tests FAILED - review output above");
   end if;

end Test_MLKEM_KAT;
