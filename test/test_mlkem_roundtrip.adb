--  ========================================================================
--  ML-KEM Roundtrip Test: KeyGen → Encaps → Decaps
--  ========================================================================
--
--  **Purpose**: Verify pure SPARK ML-KEM implementation works correctly
--               Tests that shared secrets match after encapsulation/decapsulation
--
--  **Test Flow**:
--    1. Generate key pair (PK, SK)
--    2. Encapsulate shared secret SS1 using PK → ciphertext CT
--    3. Decapsulate ciphertext CT using SK → shared secret SS2
--    4. Verify SS1 = SS2
--
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Pure;

procedure Test_MLKEM_Roundtrip is

   --  Key pair
   PK : SparkPass.Crypto.MLKEM.Pure.Public_Key;
   SK : SparkPass.Crypto.MLKEM.Pure.Secret_Key;

   --  Encapsulation results
   CT  : SparkPass.Crypto.MLKEM.Pure.Ciphertext;
   SS1 : SparkPass.Crypto.MLKEM.Pure.Shared_Key;

   --  Decapsulation results
   SS2 : SparkPass.Crypto.MLKEM.Pure.Shared_Key;

   --  Success flags
   Encaps_Success : Boolean;
   Decaps_Success : Boolean;

   --  Match flag
   Secrets_Match : Boolean := True;

begin
   Put_Line("========================================");
   Put_Line("ML-KEM-1024 Roundtrip Test");
   Put_Line("========================================");
   Put_Line("");

   --  Step 1: Generate key pair
   Put_Line("[1] Generating ML-KEM-1024 key pair...");
   SparkPass.Crypto.MLKEM.Pure.Keypair(PK, SK);
   Put_Line("    ✓ Public key: 1568 bytes");
   Put_Line("    ✓ Secret key: 3168 bytes");
   Put_Line("");

   --  Step 2: Encapsulate
   Put_Line("[2] Encapsulating shared secret...");
   SparkPass.Crypto.MLKEM.Pure.Encapsulate(PK, CT, SS1, Encaps_Success);

   if Encaps_Success then
      Put_Line("    ✓ Encapsulation succeeded");
      Put_Line("    ✓ Ciphertext: 1568 bytes");
      Put_Line("    ✓ Shared secret 1: 32 bytes");
   else
      Put_Line("    ✗ Encapsulation FAILED");
      return;
   end if;
   Put_Line("");

   --  Step 3: Decapsulate
   Put_Line("[3] Decapsulating ciphertext...");
   SparkPass.Crypto.MLKEM.Pure.Decapsulate(SK, CT, SS2, Decaps_Success);

   if Decaps_Success then
      Put_Line("    ✓ Decapsulation succeeded");
      Put_Line("    ✓ Shared secret 2: 32 bytes");
   else
      Put_Line("    ✗ Decapsulation FAILED");
      return;
   end if;
   Put_Line("");

   --  Step 4: Verify shared secrets match
   Put_Line("[4] Verifying shared secrets match...");
   for I in SS1'Range loop
      if SS1(I) /= SS2(I) then
         Secrets_Match := False;
         exit;
      end if;
   end loop;

   if Secrets_Match then
      Put_Line("    ✓ PASSED: Shared secrets match!");
      Put_Line("");
      Put_Line("========================================");
      Put_Line("TEST RESULT: SUCCESS");
      Put_Line("========================================");
      Put_Line("");
      Put_Line("ML-KEM-1024 implementation is functional!");
      Put_Line("KeyGen → Encaps → Decaps roundtrip works correctly.");
   else
      Put_Line("    ✗ FAILED: Shared secrets DO NOT match!");
      Put_Line("");
      Put_Line("========================================");
      Put_Line("TEST RESULT: FAILURE");
      Put_Line("========================================");

      --  Print first 16 bytes of each for debugging
      Put("    SS1 (first 16 bytes): ");
      for I in 1 .. 16 loop
         Put(U8'Image(SS1(I)));
         if I < 16 then
            Put(" ");
         end if;
      end loop;
      New_Line;

      Put("    SS2 (first 16 bytes): ");
      for I in 1 .. 16 loop
         Put(U8'Image(SS2(I)));
         if I < 16 then
            Put(" ");
         end if;
      end loop;
      New_Line;
   end if;

end Test_MLKEM_Roundtrip;
