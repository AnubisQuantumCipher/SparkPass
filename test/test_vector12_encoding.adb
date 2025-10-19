--  ========================================================================
--  Test 12-bit Vector Encoding Roundtrip
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.Encoding; use SparkPass.Crypto.MLKEM.Encoding;
with SparkPass.Crypto.Random;

procedure Test_Vector12_Encoding is

   Test_Vec : Polynomial_Vector;
   Encoded : Byte_Array(1 .. Bytes_Per_Vector_12);
   Decoded_Vec : Polynomial_Vector;
   Match : Boolean := True;

begin
   Put_Line("========================================");
   Put_Line("12-bit Vector Encoding Test");
   Put_Line("========================================");
   Put_Line("");

   --  Generate random test vector
   Put_Line("[1] Generating random test vector...");
   for I in 0 .. K - 1 loop
      declare
         Random_Bytes : Byte_Array(1 .. 768);
      begin
         SparkPass.Crypto.Random.Fill(Random_Bytes);
         for J in 0 .. 255 loop
            declare
               Val : constant Unsigned_32 :=
                  Unsigned_32(Random_Bytes(3*J + 1)) or
                  (Unsigned_32(Random_Bytes(3*J + 2)) * 256) or
                  (Unsigned_32(Random_Bytes(3*J + 3)) * 65536);
            begin
               Test_Vec(I)(J) := Coefficient(Val mod Unsigned_32(Q));
            end;
         end loop;
      end;
   end loop;
   Put_Line("    ✓ Generated 4 polynomials with 256 coefficients each");
   Put_Line("");

   --  Test encoding/decoding roundtrip
   Put_Line("[2] Testing encode → decode roundtrip...");
   Encode_Vector_12(Test_Vec, Encoded);
   Put_Line("    ✓ Encoded to 1536 bytes");

   Decode_Vector_12(Encoded, Decoded_Vec);
   Put_Line("    ✓ Decoded back to vector");

   --  Verify all coefficients match
   for I in 0 .. K - 1 loop
      for J in 0 .. 255 loop
         if Test_Vec(I)(J) /= Decoded_Vec(I)(J) then
            Put_Line("    ✗ Mismatch at polynomial" & Natural'Image(I) &
                     ", coefficient" & Natural'Image(J));
            Put_Line("      Original: " & Coefficient'Image(Test_Vec(I)(J)));
            Put_Line("      Decoded:  " & Coefficient'Image(Decoded_Vec(I)(J)));
            Match := False;
            goto Done;
         end if;
      end loop;
   end loop;

   <<Done>>

   if Match then
      Put_Line("    ✓ All coefficients match!");
   end if;
   Put_Line("");

   Put_Line("========================================");
   if Match then
      Put_Line("TEST RESULT: SUCCESS");
   else
      Put_Line("TEST RESULT: FAILURE");
   end if;
   Put_Line("========================================");

end Test_Vector12_Encoding;
