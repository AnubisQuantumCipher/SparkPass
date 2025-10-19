--  Test if t is being encoded/decoded correctly

with Ada.Text_IO; use Ada.Text_IO;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.KeyGen;
with SparkPass.Crypto.MLKEM.Encoding;
with SparkPass.Crypto.MLKEM.NTT;

procedure Test_T_Encoding_Roundtrip is

   Seed : Seed_Array;
   PK   : Public_Key_Array;
   SK   : Secret_Key_Array;

   T_Original    : Polynomial_Vector;
   T_Decoded     : Polynomial_Vector;
   T_Decoded_NTT : Polynomial_Vector;
   T_After_INTT  : Polynomial_Vector;

   Match : Boolean := True;

begin
   Put_Line("Testing t encoding/decoding roundtrip");
   Put_Line("========================================");
   New_Line;

   --  Generate keys
   Seed := (others => 42);
   SparkPass.Crypto.MLKEM.KeyGen.KeyGen(Seed, PK, SK);

   Put_Line("Step 1: Extract original t from KeyGen internals");
   --  (We can't actually do this without modifying KeyGen)
   --  Instead, decode t from PK
   declare
      T_Bytes : Byte_Array(1 .. 1536);
      use SparkPass.Crypto.MLKEM.Encoding;
   begin
      T_Bytes := PK(1 .. 1536);
      Decode_Vector_12(T_Bytes, T_Original);
   end;

   Put_Line("  t[0][0] = " & Coefficient'Image(T_Original(0)(0)));
   Put_Line("  t[0][1] = " & Coefficient'Image(T_Original(0)(1)));
   Put_Line("  t[0][2] = " & Coefficient'Image(T_Original(0)(2)));
   New_Line;

   Put_Line("Step 2: Encode t and decode it back");
   declare
      T_Encoded : Byte_Array(1 .. 1536);
      use SparkPass.Crypto.MLKEM.Encoding;
   begin
      Encode_Vector_12(T_Original, T_Encoded);
      Decode_Vector_12(T_Encoded, T_Decoded);
   end;

   Put_Line("  t_decoded[0][0] = " & Coefficient'Image(T_Decoded(0)(0)));
   Put_Line("  t_decoded[0][1] = " & Coefficient'Image(T_Decoded(0)(1)));
   Put_Line("  t_decoded[0][2] = " & Coefficient'Image(T_Decoded(0)(2)));
   New_Line;

   --  Check if encoding/decoding is lossless
   for I in 0 .. K - 1 loop
      for J in 0 .. 255 loop
         if T_Original(I)(J) /= T_Decoded(I)(J) then
            Put_Line("  MISMATCH at t[" & Natural'Image(I) & "][" & Natural'Image(J) & "]");
            Put_Line("    Original: " & Coefficient'Image(T_Original(I)(J)));
            Put_Line("    Decoded:  " & Coefficient'Image(T_Decoded(I)(J)));
            Match := False;
            exit;
         end if;
      end loop;
      exit when not Match;
   end loop;

   if Match then
      Put_Line("  ✓ Encoding/decoding is lossless!");
   else
      Put_Line("  ✗ Encoding/decoding has errors!");
      return;
   end if;
   New_Line;

   Put_Line("Step 3: Transform to NTT and back");
   for I in 0 .. K - 1 loop
      T_Decoded_NTT(I) := T_Decoded(I);
      SparkPass.Crypto.MLKEM.NTT.NTT(T_Decoded_NTT(I));
   end loop;

   Put_Line("  t_ntt[0][0] = " & Coefficient'Image(T_Decoded_NTT(0)(0)));
   Put_Line("  t_ntt[0][1] = " & Coefficient'Image(T_Decoded_NTT(0)(1)));
   Put_Line("  t_ntt[0][2] = " & Coefficient'Image(T_Decoded_NTT(0)(2)));
   New_Line;

   for I in 0 .. K - 1 loop
      T_After_INTT(I) := T_Decoded_NTT(I);
      SparkPass.Crypto.MLKEM.NTT.INTT(T_After_INTT(I));
   end loop;

   Put_Line("  t_after_intt[0][0] = " & Coefficient'Image(T_After_INTT(0)(0)));
   Put_Line("  t_after_intt[0][1] = " & Coefficient'Image(T_After_INTT(0)(1)));
   Put_Line("  t_after_intt[0][2] = " & Coefficient'Image(T_After_INTT(0)(2)));
   New_Line;

   --  Check if NTT/INTT roundtrip works
   Match := True;
   for I in 0 .. K - 1 loop
      for J in 0 .. 255 loop
         if T_Decoded(I)(J) /= T_After_INTT(I)(J) then
            Put_Line("  MISMATCH at t[" & Natural'Image(I) & "][" & Natural'Image(J) & "]");
            Put_Line("    Before NTT: " & Coefficient'Image(T_Decoded(I)(J)));
            Put_Line("    After INTT: " & Coefficient'Image(T_After_INTT(I)(J)));
            Match := False;
            exit;
         end if;
      end loop;
      exit when not Match;
   end loop;

   if Match then
      Put_Line("  ✓ NTT/INTT roundtrip is perfect!");
   else
      Put_Line("  ✗ NTT/INTT roundtrip has errors!");
   end if;

   New_Line;
   Put_Line("========================================");
   if Match then
      Put_Line("TEST RESULT: SUCCESS");
      Put_Line("t encoding and NTT operations are correct");
   else
      Put_Line("TEST RESULT: FAILURE");
   end if;
   Put_Line("========================================");

end Test_T_Encoding_Roundtrip;
