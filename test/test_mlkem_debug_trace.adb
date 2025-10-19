--  ========================================================================
--  ML-KEM Debug Trace: Detailed Encryption/Decryption Analysis
--  ========================================================================
--
--  **Purpose**: Trace all intermediate values in K-PKE.Encrypt and K-PKE.Decrypt
--               to identify where the roundtrip fails
--
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.KeyGen;
with SparkPass.Crypto.MLKEM.NTT;
with SparkPass.Crypto.MLKEM.Matrix;
with SparkPass.Crypto.MLKEM.Sampling;
with SparkPass.Crypto.MLKEM.Hash;
with SparkPass.Crypto.MLKEM.PRF;
with SparkPass.Crypto.MLKEM.XOF;
with SparkPass.Crypto.MLKEM.Encoding;
with SparkPass.Crypto.MLKEM.Poly;

procedure Test_MLKEM_Debug_Trace is

   procedure Print_Poly_Summary (Label : String; P : Polynomial) is
   begin
      Put (Label & " [0..3]: ");
      for I in 0 .. 3 loop
         Put (Coefficient'Image(P(I)));
         if I < 3 then Put (" "); end if;
      end loop;
      Put (" ... [252..255]: ");
      for I in 252 .. 255 loop
         Put (Coefficient'Image(P(I)));
         if I < 255 then Put (" "); end if;
      end loop;
      New_Line;
   end Print_Poly_Summary;

   procedure Print_Bytes (Label : String; Data : Byte_Array; Count : Natural := 16) is
   begin
      Put (Label & ": ");
      for I in Data'First .. Natural'Min(Data'First + Count - 1, Data'Last) loop
         Put (U8'Image(Data(I)));
         if I < Natural'Min(Data'First + Count - 1, Data'Last) then Put (" "); end if;
      end loop;
      New_Line;
   end Print_Bytes;

   --  Keys
   Seed : Seed_Array;
   PK   : Public_Key_Array;
   SK   : Secret_Key_Array;

   --  Extracted from SK
   S_Vector : Polynomial_Vector;

   --  Encryption inputs/outputs
   Message       : Seed_Array;
   Randomness    : Seed_Array;
   Ciphertext    : Ciphertext_Array;

   --  Encryption intermediates
   T_Vector_NTT  : Polynomial_Vector;
   Rho_Seed      : Seed_Array;
   A_Matrix      : Polynomial_Matrix;
   R_Vector      : Polynomial_Vector;
   R_Vector_NTT  : Polynomial_Vector;
   E1_Vector     : Polynomial_Vector;
   E2_Poly       : Polynomial;
   Message_Poly  : Polynomial;
   U_Vector_Enc  : Polynomial_Vector;
   V_Poly_Enc    : Polynomial;
   Temp_Vector   : Polynomial_Vector;
   N : Natural;

   --  Decryption intermediates
   U_Vector_Dec  : Polynomial_Vector;
   V_Poly_Dec    : Polynomial;
   S_NTT         : Polynomial_Vector;
   U_NTT         : Polynomial_Vector;
   Dot_Prod      : Polynomial;
   W_Poly        : Polynomial;
   Recovered_Msg : Seed_Array;

   Match : Boolean;

begin
   Put_Line("========================================");
   Put_Line("ML-KEM K-PKE Debug Trace");
   Put_Line("========================================");
   New_Line;

   --  ========================================================================
   --  STEP 1: Generate Keys
   --  ========================================================================

   Put_Line("[1] Generating key pair with fixed seed...");
   Seed := (others => 42);

   SparkPass.Crypto.MLKEM.KeyGen.KeyGen(Seed, PK, SK);

   --  Extract secret vector s from SK
   declare
      S_Bytes : Byte_Array(1 .. 1536);
      use SparkPass.Crypto.MLKEM.Encoding;
   begin
      S_Bytes := SK(1 .. 1536);
      Decode_Vector_12(S_Bytes, S_Vector);
   end;

   Print_Poly_Summary("   s[0]", S_Vector(0));
   New_Line;

   --  ========================================================================
   --  STEP 2: ENCRYPTION - K-PKE.Encrypt
   --  ========================================================================

   Put_Line("[2] K-PKE.Encrypt - Detailed Trace");
   Put_Line("    =====================================");
   New_Line;

   --  Fixed inputs
   Message := (others => 123);
   Randomness := (others => 99);

   Print_Bytes("   Input message m", Message);
   Print_Bytes("   Input randomness r", Randomness);
   New_Line;

   --  Step 2.1: Decode public key
   declare
      T_Bytes : Byte_Array(1 .. 1536);
      use SparkPass.Crypto.MLKEM.Encoding;
   begin
      T_Bytes := PK(1 .. 1536);
      Decode_Vector_12(T_Bytes, T_Vector_NTT);

      --  Transform to NTT domain
      for I in 0 .. K - 1 loop
         SparkPass.Crypto.MLKEM.NTT.NTT(T_Vector_NTT(I));
      end loop;

      Rho_Seed := PK(1537 .. 1568);
   end;

   Print_Poly_Summary("   t̂[0] (NTT)", T_Vector_NTT(0));
   New_Line;

   --  Step 2.2: Generate matrix A
   for I in 0 .. K - 1 loop
      for J in 0 .. K - 1 loop
         declare
            XOF_Out    : SparkPass.Crypto.MLKEM.XOF.XOF_Output;
            Bytes_Used : Natural;
            use SparkPass.Crypto.MLKEM.XOF;
            use SparkPass.Crypto.MLKEM.Sampling;
         begin
            XOF_Uniform(Rho_Seed, U8(I), U8(J), XOF_Out);
            SampleNTT(XOF_Out, A_Matrix(I, J), Bytes_Used);
         end;
      end loop;
   end loop;

   Print_Poly_Summary("   Â[0,0]", A_Matrix(0, 0));
   New_Line;

   --  Step 2.3: Generate r vector
   N := 0;
   for I in 0 .. K - 1 loop
      declare
         PRF_Out : SparkPass.Crypto.MLKEM.PRF.PRF_Output;
         use SparkPass.Crypto.MLKEM.PRF;
         use SparkPass.Crypto.MLKEM.Sampling;
      begin
         PRF_CBD(Randomness, U8(N), PRF_Out);
         N := N + 1;
         SamplePolyCBD(PRF_Out, Eta => 2, Poly => R_Vector(I));
         R_Vector_NTT(I) := R_Vector(I);
         SparkPass.Crypto.MLKEM.NTT.NTT(R_Vector_NTT(I));
      end;
   end loop;

   Print_Poly_Summary("   r[0] (coeff)", R_Vector(0));
   Print_Poly_Summary("   r̂[0] (NTT)", R_Vector_NTT(0));
   New_Line;

   --  Step 2.4: Generate e1 vector
   for I in 0 .. K - 1 loop
      declare
         PRF_Out : SparkPass.Crypto.MLKEM.PRF.PRF_Output;
         use SparkPass.Crypto.MLKEM.PRF;
         use SparkPass.Crypto.MLKEM.Sampling;
      begin
         PRF_CBD(Randomness, U8(N), PRF_Out);
         N := N + 1;
         SamplePolyCBD(PRF_Out, Eta => 2, Poly => E1_Vector(I));
      end;
   end loop;

   Print_Poly_Summary("   e₁[0]", E1_Vector(0));
   New_Line;

   --  Step 2.5: Generate e2
   declare
      PRF_Out : SparkPass.Crypto.MLKEM.PRF.PRF_Output;
      use SparkPass.Crypto.MLKEM.PRF;
      use SparkPass.Crypto.MLKEM.Sampling;
   begin
      PRF_CBD(Randomness, U8(N), PRF_Out);
      SamplePolyCBD(PRF_Out, Eta => 2, Poly => E2_Poly);
   end;

   Print_Poly_Summary("   e₂", E2_Poly);
   New_Line;

   --  Step 2.6: Compute u = INTT(Âᵀ·r̂) + e₁
   SparkPass.Crypto.MLKEM.Matrix.Matrix_Transpose_Vector_Mul(
      A_Matrix, R_Vector_NTT, Temp_Vector
   );

   Put_Line("   Computing u = INTT(Âᵀ·r̂) + e₁:");
   Print_Poly_Summary("     Âᵀ·r̂[0] (NTT, before INTT)", Temp_Vector(0));

   for I in 0 .. K - 1 loop
      U_Vector_Enc(I) := Temp_Vector(I);
      SparkPass.Crypto.MLKEM.NTT.INTT(U_Vector_Enc(I));
   end loop;

   Print_Poly_Summary("     INTT(Âᵀ·r̂)[0] (after INTT)", U_Vector_Enc(0));

   SparkPass.Crypto.MLKEM.Matrix.Vector_Add(
      U_Vector_Enc, E1_Vector, U_Vector_Enc
   );

   Print_Poly_Summary("     u[0] = INTT(Âᵀ·r̂) + e₁", U_Vector_Enc(0));
   New_Line;

   --  Step 2.7: Compute v = INTT(t̂ᵀ·r̂) + e₂ + μ
   declare
      Dot : Polynomial;
   begin
      SparkPass.Crypto.MLKEM.Matrix.Dot_Product(T_Vector_NTT, R_Vector_NTT, Dot);

      Put_Line("   Computing v = INTT(t̂ᵀ·r̂) + e₂ + μ:");
      Print_Poly_Summary("     t̂ᵀ·r̂ (NTT, before INTT)", Dot);

      V_Poly_Enc := Dot;
      SparkPass.Crypto.MLKEM.NTT.INTT(V_Poly_Enc);

      Print_Poly_Summary("     INTT(t̂ᵀ·r̂) (after INTT)", V_Poly_Enc);
   end;

   SparkPass.Crypto.MLKEM.Poly.Add(V_Poly_Enc, E2_Poly, V_Poly_Enc);
   Print_Poly_Summary("     INTT(t̂ᵀ·r̂) + e₂", V_Poly_Enc);

   --  Decompress message
   for I in 0 .. 255 loop
      declare
         Byte_Index : constant Natural := I / 8;
         Bit_Index  : constant Natural := I mod 8;
         Bit_Value  : constant U8 := (Message(Byte_Index + 1) / (2 ** Bit_Index)) and 1;
      begin
         if Bit_Value = 0 then
            Message_Poly(I) := 0;
         else
            Message_Poly(I) := 1665;  -- ⌈3329/2⌋
         end if;
      end;
   end loop;

   Print_Poly_Summary("     μ (decompressed message)", Message_Poly);

   SparkPass.Crypto.MLKEM.Poly.Add(V_Poly_Enc, Message_Poly, V_Poly_Enc);
   Print_Poly_Summary("     v = INTT(t̂ᵀ·r̂) + e₂ + μ", V_Poly_Enc);
   New_Line;

   --  Step 2.8: Encode ciphertext (simplified - just store for decryption)
   Put_Line("   Encryption complete.");
   New_Line;

   --  ========================================================================
   --  STEP 3: DECRYPTION - K-PKE.Decrypt
   --  ========================================================================

   Put_Line("[3] K-PKE.Decrypt - Detailed Trace");
   Put_Line("    =====================================");
   New_Line;

   --  Use the same u and v from encryption (no compression/decompression yet)
   U_Vector_Dec := U_Vector_Enc;
   V_Poly_Dec := V_Poly_Enc;

   Print_Poly_Summary("   Input u[0]", U_Vector_Dec(0));
   Print_Poly_Summary("   Input v", V_Poly_Dec);
   New_Line;

   --  Step 3.1: Transform s and u to NTT
   Put_Line("   Transforming s and u to NTT domain:");
   for I in 0 .. K - 1 loop
      S_NTT(I) := S_Vector(I);
      SparkPass.Crypto.MLKEM.NTT.NTT(S_NTT(I));

      U_NTT(I) := U_Vector_Dec(I);
      SparkPass.Crypto.MLKEM.NTT.NTT(U_NTT(I));
   end loop;

   Print_Poly_Summary("     ŝ[0] (NTT)", S_NTT(0));
   Print_Poly_Summary("     û[0] (NTT)", U_NTT(0));
   New_Line;

   --  Step 3.2: Compute sᵀ·u
   Put_Line("   Computing sᵀ·u:");
   SparkPass.Crypto.MLKEM.Matrix.Dot_Product(S_NTT, U_NTT, Dot_Prod);

   Print_Poly_Summary("     ŝᵀ·û (NTT, before INTT)", Dot_Prod);

   W_Poly := Dot_Prod;
   SparkPass.Crypto.MLKEM.NTT.INTT(W_Poly);

   Print_Poly_Summary("     sᵀ·u = INTT(ŝᵀ·û) (after INTT)", W_Poly);
   New_Line;

   --  Step 3.3: Compute w = v - sᵀ·u
   Put_Line("   Computing w = v - sᵀ·u:");
   Print_Poly_Summary("     v", V_Poly_Dec);
   Print_Poly_Summary("     sᵀ·u", W_Poly);

   SparkPass.Crypto.MLKEM.Poly.Sub(V_Poly_Dec, W_Poly, W_Poly);

   Print_Poly_Summary("     w = v - sᵀ·u", W_Poly);
   New_Line;

   --  Step 3.4: Compress to message
   Put_Line("   Extracting message from w:");
   for I in 0 .. 255 loop
      declare
         Byte_Index : constant Natural := I / 8;
         Bit_Index  : constant Natural := I mod 8;
         Bit_Value  : constant U8 := (if W_Poly(I) >= 1665 then 1 else 0);
      begin
         if I mod 8 = 0 then
            Recovered_Msg(Byte_Index + 1) := 0;
         end if;
         Recovered_Msg(Byte_Index + 1) :=
            Recovered_Msg(Byte_Index + 1) or (Bit_Value * (2 ** Bit_Index));
      end;
   end loop;

   Print_Bytes("   Recovered message m'", Recovered_Msg);
   New_Line;

   --  ========================================================================
   --  STEP 4: VERIFICATION
   --  ========================================================================

   Put_Line("[4] Verification");
   Put_Line("    =====================================");
   New_Line;

   Match := True;
   for I in Message'Range loop
      if Message(I) /= Recovered_Msg(I) then
         Match := False;
         exit;
      end if;
   end loop;

   if Match then
      Put_Line("    ✓ SUCCESS: Message recovered correctly!");
      Put_Line("    ✓ m = m' (all bytes match)");
   else
      Put_Line("    ✗ FAILURE: Message recovery failed!");
      Print_Bytes("      Original m", Message);
      Print_Bytes("      Recovered m'", Recovered_Msg);

      --  Find first diverging byte
      for I in Message'Range loop
         if Message(I) /= Recovered_Msg(I) then
            Put_Line("      First divergence at byte" & Natural'Image(I) &
                     ": expected" & U8'Image(Message(I)) &
                     " got" & U8'Image(Recovered_Msg(I)));
            exit;
         end if;
      end loop;
   end if;

   New_Line;
   Put_Line("========================================");
   if Match then
      Put_Line("TEST RESULT: SUCCESS");
   else
      Put_Line("TEST RESULT: FAILURE");
   end if;
   Put_Line("========================================");

end Test_MLKEM_Debug_Trace;
