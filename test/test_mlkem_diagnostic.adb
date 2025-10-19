--  ========================================================================
--  Diagnostic Test: ML-KEM-1024 with detailed output
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.KeyGen;
with SparkPass.Crypto.MLKEM.Encaps;
with SparkPass.Crypto.MLKEM.Decaps;
with SparkPass.Crypto.Random;

procedure Test_MLKEM_Diagnostic is

   PK : Public_Key_Array;
   SK : Secret_Key_Array;
   CT : Ciphertext_Array;
   CT_Prime : Ciphertext_Array;
   SS_Enc : Shared_Secret_Array;
   SS_Dec : Shared_Secret_Array;
   Recovered_Msg : Seed_Array;
   Valid : Boolean;
   U_Vec : Polynomial_Vector;
   V_Poly : Polynomial;
   Random_Msg : Seed_Array;

   Match : Boolean := True;

begin
   Put_Line("========================================");
   Put_Line("ML-KEM-1024 Diagnostic Test");
   Put_Line("========================================");
   Put_Line("");

   --  Step 1: Generate key pair
   Put_Line("[1] Generating key pair...");
   declare
      Seed : Seed_Array;
   begin
      SparkPass.Crypto.Random.Fill(Seed);
      SparkPass.Crypto.MLKEM.KeyGen.KeyGen(Seed, PK, SK);
   end;
   Put_Line("    ✓ KeyGen completed");
   Put_Line("");

   --  Step 2: Encapsulate
   Put_Line("[2] Encapsulating with random message...");
   SparkPass.Crypto.Random.Fill(Random_Msg);

   SparkPass.Crypto.MLKEM.Encaps.Encapsulate_Expanded(
      PK, Random_Msg, CT, SS_Enc, U_Vec, V_Poly
   );

   Put_Line("    ✓ Encapsulation completed");
   Put("    Message (first 8 bytes): ");
   for I in 1 .. 8 loop
      Put(U8'Image(Random_Msg(I)) & " ");
   end loop;
   Put_Line("");
   Put("    Shared secret (first 8 bytes): ");
   for I in 1 .. 8 loop
      Put(U8'Image(SS_Enc(I)) & " ");
   end loop;
   Put_Line("");
   Put_Line("");

   --  Step 3: Decapsulate
   Put_Line("[3] Decapsulating...");

   SparkPass.Crypto.MLKEM.Decaps.Decapsulate_Expanded(
      SK, CT, SS_Dec, Recovered_Msg, Valid
   );

   Put_Line("    ✓ Decapsulation completed");
   Put("    Recovered message (first 8 bytes): ");
   for I in 1 .. 8 loop
      Put(U8'Image(Recovered_Msg(I)) & " ");
   end loop;
   Put_Line("");
   Put_Line("    Validation status: " & Boolean'Image(Valid));
   Put("    Shared secret (first 8 bytes): ");
   for I in 1 .. 8 loop
      Put(U8'Image(SS_Dec(I)) & " ");
   end loop;
   Put_Line("");
   Put_Line("");

   --  Step 4: Check if messages match
   Put_Line("[4] Checking message recovery...");
   declare
      Msg_Match : Boolean := True;
   begin
      for I in Random_Msg'Range loop
         if Random_Msg(I) /= Recovered_Msg(I) then
            Msg_Match := False;
            exit;
         end if;
      end loop;

      if Msg_Match then
         Put_Line("    ✓ Messages match!");
      else
         Put_Line("    ✗ Messages DO NOT match!");
      end if;
   end;
   Put_Line("");

   --  Step 5: Re-encrypt to check validation
   Put_Line("[5] Re-encrypting recovered message...");
   SparkPass.Crypto.MLKEM.Encaps.Encapsulate_Expanded(
      PK, Recovered_Msg, CT_Prime, SS_Enc, U_Vec, V_Poly
   );

   declare
      CT_Match : Boolean := True;
   begin
      for I in CT'Range loop
         if CT(I) /= CT_Prime(I) then
            CT_Match := False;
            Put_Line("    ✗ Mismatch at byte" & Positive'Image(I));
            Put_Line("      Original:    " & U8'Image(CT(I)));
            Put_Line("      Re-encrypted: " & U8'Image(CT_Prime(I)));
            exit;
         end if;
      end loop;

      if CT_Match then
         Put_Line("    ✓ Ciphertexts match!");
      else
         Put_Line("    ✗ Ciphertexts DO NOT match!");
      end if;
   end;
   Put_Line("");

   --  Step 6: Check shared secrets
   Put_Line("[6] Checking shared secrets...");
   for I in SS_Enc'Range loop
      if SS_Enc(I) /= SS_Dec(I) then
         Match := False;
      end if;
   end loop;

   if Match then
      Put_Line("    ✓ Shared secrets match!");
   else
      Put_Line("    ✗ Shared secrets DO NOT match!");
   end if;

   Put_Line("");
   Put_Line("========================================");
   if Match then
      Put_Line("TEST RESULT: SUCCESS");
   else
      Put_Line("TEST RESULT: FAILURE");
   end if;
   Put_Line("========================================");

end Test_MLKEM_Diagnostic;
