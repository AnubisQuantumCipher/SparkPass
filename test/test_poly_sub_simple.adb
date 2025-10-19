--  Test if polynomial subtraction works correctly

with Ada.Text_IO; use Ada.Text_IO;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.Poly;

procedure Test_Poly_Sub_Simple is

   A, B, C : Polynomial;
   Expected : Polynomial;

begin
   Put_Line("Testing polynomial subtraction");
   Put_Line("==============================");
   New_Line;

   --  Test 1: Simple subtraction
   A := (others => 1000);
   B := (others => 500);
   SparkPass.Crypto.MLKEM.Poly.Sub(A, B, C);

   Put_Line("Test 1: A - B where A=1000, B=500");
   Put_Line("  Expected: C = 500");
   Put_Line("  Got: C[0] = " & Coefficient'Image(C(0)));

   if C(0) = 500 then
      Put_Line("  ✓ PASS");
   else
      Put_Line("  ✗ FAIL");
   end if;
   New_Line;

   --  Test 2: Subtraction with borrow (result should wrap modulo q)
   A := (others => 500);
   B := (others => 1000);
   SparkPass.Crypto.MLKEM.Poly.Sub(A, B, C);

   Put_Line("Test 2: A - B where A=500, B=1000");
   Put_Line("  Expected: C = (500 - 1000 + 3329) mod 3329 = 2829");
   Put_Line("  Got: C[0] = " & Coefficient'Image(C(0)));

   if C(0) = 2829 then
      Put_Line("  ✓ PASS");
   else
      Put_Line("  ✗ FAIL");
   end if;
   New_Line;

   --  Test 3: Mix of values
   for I in 0 .. 255 loop
      A(I) := Coefficient(I mod 100);
      B(I) := Coefficient((I mod 50) * 10);
   end loop;

   SparkPass.Crypto.MLKEM.Poly.Sub(A, B, C);

   Put_Line("Test 3: Mixed values");
   Put_Line("  A[0] = " & Coefficient'Image(A(0)) & ", B[0] = " & Coefficient'Image(B(0)));
   Expected(0) := Coefficient((Integer(A(0)) - Integer(B(0)) + 3329) mod 3329);
   Put_Line("  Expected C[0] = " & Coefficient'Image(Expected(0)));
   Put_Line("  Got C[0] = " & Coefficient'Image(C(0)));

   if C(0) = Expected(0) then
      Put_Line("  ✓ PASS");
   else
      Put_Line("  ✗ FAIL");
   end if;

   Put_Line("  A[10] = " & Coefficient'Image(A(10)) & ", B[10] = " & Coefficient'Image(B(10)));
   Expected(10) := Coefficient((Integer(A(10)) - Integer(B(10)) + 3329) mod 3329);
   Put_Line("  Expected C[10] = " & Coefficient'Image(Expected(10)));
   Put_Line("  Got C[10] = " & Coefficient'Image(C(10)));

   if C(10) = Expected(10) then
      Put_Line("  ✓ PASS");
   else
      Put_Line("  ✗ FAIL");
   end if;

   New_Line;
   Put_Line("==============================");
   Put_Line("Polynomial subtraction test complete");
   Put_Line("==============================");

end Test_Poly_Sub_Simple;
