--  ========================================================================
--  Test NTT/INTT are inverses
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.NTT;
with SparkPass.Crypto.Random;

procedure Test_NTT_Inverse is

   Test_Poly : Polynomial;
   NTT_Poly : Polynomial;
   Recovered : Polynomial;
   Match : Boolean := True;

begin
   Put_Line("========================================");
   Put_Line("NTT/INTT Inverse Test");
   Put_Line("========================================");
   Put_Line("");

   --  Generate random test polynomial
   Put_Line("[1] Generating random polynomial...");
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
   Put_Line("    ✓ Generated 256 coefficients");
   Put("    First 4 coefficients: ");
   for I in 0 .. 3 loop
      Put(Coefficient'Image(Test_Poly(I)) & " ");
   end loop;
   Put_Line("");
   Put_Line("");

   --  Test NTT → INTT roundtrip
   Put_Line("[2] Testing NTT → INTT roundtrip...");
   NTT_Poly := Test_Poly;
   SparkPass.Crypto.MLKEM.NTT.NTT(NTT_Poly);
   Put_Line("    ✓ Applied NTT");
   Put("    First 4 NTT coefficients: ");
   for I in 0 .. 3 loop
      Put(Coefficient'Image(NTT_Poly(I)) & " ");
   end loop;
   Put_Line("");

   Recovered := NTT_Poly;
   SparkPass.Crypto.MLKEM.NTT.INTT(Recovered);
   Put_Line("    ✓ Applied INTT");
   Put("    First 4 recovered coefficients: ");
   for I in 0 .. 3 loop
      Put(Coefficient'Image(Recovered(I)) & " ");
   end loop;
   Put_Line("");

   --  Verify all coefficients match
   for I in 0 .. 255 loop
      if Test_Poly(I) /= Recovered(I) then
         Put_Line("    ✗ Mismatch at coefficient" & Natural'Image(I));
         Put_Line("      Original:  " & Coefficient'Image(Test_Poly(I)));
         Put_Line("      Recovered: " & Coefficient'Image(Recovered(I)));
         Match := False;
         exit;
      end if;
   end loop;

   if Match then
      Put_Line("    ✓ All 256 coefficients match!");
   end if;
   Put_Line("");

   Put_Line("========================================");
   if Match then
      Put_Line("TEST RESULT: SUCCESS");
      Put_Line("NTT and INTT are proper inverses!");
   else
      Put_Line("TEST RESULT: FAILURE");
   end if;
   Put_Line("========================================");

end Test_NTT_Inverse;
