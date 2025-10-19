--  ========================================================================
--  Test Encoding Roundtrip: Verify encode → decode is identity
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.Encoding; use SparkPass.Crypto.MLKEM.Encoding;
with SparkPass.Crypto.Random;

procedure Test_Encoding_Roundtrip is

   Test_Poly : Polynomial;
   Encoded_11 : Byte_Array(1 .. Bytes_Per_Poly_11);
   Decoded_11 : Polynomial;

   Encoded_5 : Byte_Array(1 .. Bytes_Per_Poly_5);
   Decoded_5 : Polynomial;

   Match_11 : Boolean := True;
   Match_5 : Boolean := True;

begin
   Put_Line("========================================");
   Put_Line("Encoding Roundtrip Test");
   Put_Line("========================================");
   Put_Line("");

   --  Generate random test polynomial
   Put_Line("[1] Generating random test polynomial...");
   declare
      Random_Bytes : Byte_Array(1 .. 768);
   begin
      SparkPass.Crypto.Random.Fill(Random_Bytes);

      --  Create polynomial with coefficients < q
      for I in 0 .. 255 loop
         declare
            Val : constant Unsigned_32 :=
               Unsigned_32(Random_Bytes(3*I + 1)) or
               (Unsigned_32(Random_Bytes(3*I + 2)) * 256) or
               (Unsigned_32(Random_Bytes(3*I + 3)) * 65536);
         begin
            Test_Poly(I) := Coefficient(Val mod Unsigned_32(Q));
         end;
      end loop;
   end;
   Put_Line("    ✓ Generated 256 random coefficients (all < 3329)");
   Put_Line("");

   --  Test 11-bit encoding roundtrip
   Put_Line("[2] Testing 11-bit encoding roundtrip...");

   --  Create compressed polynomial (all values < 2048)
   for I in 0 .. 255 loop
      Test_Poly(I) := Compress_11(Test_Poly(I));
   end loop;

   ByteEncode_11(Test_Poly, Encoded_11);
   Put_Line("    ✓ Encoded to 352 bytes");

   ByteDecode_11(Encoded_11, Decoded_11);
   Put_Line("    ✓ Decoded back to polynomial");

   --  Verify all coefficients match
   for I in 0 .. 255 loop
      if Test_Poly(I) /= Decoded_11(I) then
         Put_Line("    ✗ Mismatch at coefficient" & Integer'Image(I));
         Put_Line("      Original: " & Coefficient'Image(Test_Poly(I)));
         Put_Line("      Decoded:  " & Coefficient'Image(Decoded_11(I)));
         Match_11 := False;
         exit;
      end if;
   end loop;

   if Match_11 then
      Put_Line("    ✓ All 256 coefficients match!");
   end if;
   Put_Line("");

   --  Test 5-bit encoding with compressed values
   Put_Line("[3] Testing 5-bit encoding roundtrip...");

   --  Regenerate random polynomial for 5-bit test
   declare
      Random_Bytes : Byte_Array(1 .. 768);
   begin
      SparkPass.Crypto.Random.Fill(Random_Bytes);
      for I in 0 .. 255 loop
         declare
            Val : constant Unsigned_32 :=
               Unsigned_32(Random_Bytes(3*I + 1)) or
               (Unsigned_32(Random_Bytes(3*I + 2)) * 256) or
               (Unsigned_32(Random_Bytes(3*I + 3)) * 65536);
         begin
            Test_Poly(I) := Coefficient(Val mod Unsigned_32(Q));
         end;
      end loop;
   end;

   --  Create compressed polynomial (all values < 32)
   for I in 0 .. 255 loop
      Test_Poly(I) := Compress_5(Test_Poly(I));
   end loop;

   ByteEncode_5(Test_Poly, Encoded_5);
   Put_Line("    ✓ Encoded to 160 bytes");

   ByteDecode_5(Encoded_5, Decoded_5);
   Put_Line("    ✓ Decoded back to polynomial");

   --  Verify all coefficients match
   for I in 0 .. 255 loop
      if Test_Poly(I) /= Decoded_5(I) then
         Put_Line("    ✗ Mismatch at coefficient" & Integer'Image(I));
         Put_Line("      Original: " & Coefficient'Image(Test_Poly(I)));
         Put_Line("      Decoded:  " & Coefficient'Image(Decoded_5(I)));
         Match_5 := False;
         exit;
      end if;
   end loop;

   if Match_5 then
      Put_Line("    ✓ All 256 coefficients match!");
   end if;
   Put_Line("");

   --  Final result
   Put_Line("========================================");
   if Match_11 and Match_5 then
      Put_Line("TEST RESULT: SUCCESS");
      Put_Line("========================================");
      Put_Line("");
      Put_Line("Both 11-bit and 5-bit encoding work correctly!");
   else
      Put_Line("TEST RESULT: FAILURE");
      Put_Line("========================================");
      if not Match_11 then
         Put_Line("✗ 11-bit encoding has bugs");
      end if;
      if not Match_5 then
         Put_Line("✗ 5-bit encoding has bugs");
      end if;
   end if;

end Test_Encoding_Roundtrip;
