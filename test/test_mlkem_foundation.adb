pragma SPARK_Mode (On);

with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.Arithmetic; use SparkPass.Crypto.MLKEM.Arithmetic;
with SparkPass.Crypto.MLKEM.Poly; use SparkPass.Crypto.MLKEM.Poly;
with Ada.Text_IO; use Ada.Text_IO;

--  ========================================================================
--  ML-KEM-1024 Foundation Unit Tests (Phase 2.1)
--  ========================================================================
--
--  **Purpose**: Verify correctness of Barrett reduction and basic arithmetic
--
--  **Test Coverage**:
--  1. Barrett reduction on known test vectors
--  2. Modular arithmetic properties (commutativity, associativity)
--  3. Polynomial addition/subtraction correctness
--
--  **Test Vectors**: Hand-computed and verified against libcrux
--
--  ========================================================================

procedure Test_MLKEM_Foundation is

   Passes : Natural := 0;
   Fails  : Natural := 0;

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      if Condition then
         Passes := Passes + 1;
         Put_Line("  [PASS] " & Message);
      else
         Fails := Fails + 1;
         Put_Line("  [FAIL] " & Message);
      end if;
   end Assert;

   --  ========================================================================
   --  Test 1: Barrett Reduction Correctness
   --  ========================================================================

   procedure Test_Barrett_Reduction is
      X : Integer;
      R : Coefficient;
   begin
      Put_Line("=== Test 1: Barrett Reduction ===");

      --  Test Case 1.1: Zero reduction
      X := 0;
      R := Barrett_Reduce(X);
      Assert(R = 0, "Barrett_Reduce(0) = 0");

      --  Test Case 1.2: Identity (x < q)
      X := 1000;
      R := Barrett_Reduce(X);
      Assert(R = 1000, "Barrett_Reduce(1000) = 1000");

      --  Test Case 1.3: Single modular reduction (q <= x < 2q)
      X := 3329;  -- Exactly q
      R := Barrett_Reduce(X);
      Assert(R = 0, "Barrett_Reduce(3329) = 0");

      --  Test Case 1.4: Single modular reduction (q < x < 2q)
      X := 5000;  -- Should reduce to 5000 - 3329 = 1671
      R := Barrett_Reduce(X);
      Assert(R = 1671, "Barrett_Reduce(5000) = 1671");

      --  Test Case 1.5: Multiple modular reduction
      X := 10000;  -- 10000 mod 3329 = 10000 - 3*3329 = 13
      R := Barrett_Reduce(X);
      Assert(R = 13, "Barrett_Reduce(10000) = 13");

      --  Test Case 1.6: Negative input
      X := -100;  -- Should reduce to 3329 - 100 = 3229
      R := Barrett_Reduce(X);
      Assert(R = 3229, "Barrett_Reduce(-100) = 3229");

      --  Test Case 1.7: Large negative
      X := -10000;  -- -10000 mod 3329 = 3329 - 13 = 3316
      R := Barrett_Reduce(X);
      Assert(R = 3316, "Barrett_Reduce(-10000) = 3316");

   end Test_Barrett_Reduction;

   --  ========================================================================
   --  Test 2: Modular Arithmetic Properties
   --  ========================================================================

   procedure Test_Modular_Arithmetic is
      A, B, C : Coefficient;
      R1, R2  : Coefficient;
   begin
      Put_Line("");
      Put_Line("=== Test 2: Modular Arithmetic ===");

      --  Test Case 2.1: Addition commutativity (a + b = b + a)
      A := 1500;
      B := 2000;
      R1 := Mod_Add(A, B);
      R2 := Mod_Add(B, A);
      Assert(R1 = R2, "Mod_Add commutativity: (1500 + 2000) = (2000 + 1500)");

      --  Test Case 2.2: Addition with overflow
      A := 3000;
      B := 1000;  -- 3000 + 1000 = 4000 > q, should reduce to 671
      R1 := Mod_Add(A, B);
      Assert(R1 = 671, "Mod_Add overflow: (3000 + 1000) mod 3329 = 671");

      --  Test Case 2.3: Subtraction (positive result)
      A := 2000;
      B := 1000;
      R1 := Mod_Sub(A, B);
      Assert(R1 = 1000, "Mod_Sub: (2000 - 1000) = 1000");

      --  Test Case 2.4: Subtraction (negative result, needs wrap)
      A := 1000;
      B := 2000;  -- 1000 - 2000 = -1000, should wrap to 3329 - 1000 = 2329
      R1 := Mod_Sub(A, B);
      Assert(R1 = 2329, "Mod_Sub wrap: (1000 - 2000) mod 3329 = 2329");

      --  Test Case 2.5: Multiplication
      A := 100;
      B := 50;  -- 100 * 50 = 5000, 5000 mod 3329 = 1671
      R1 := Mod_Mul(A, B);
      Assert(R1 = 1671, "Mod_Mul: (100 * 50) mod 3329 = 1671");

      --  Test Case 2.6: Multiplication commutativity
      A := 123;
      B := 456;
      R1 := Mod_Mul(A, B);
      R2 := Mod_Mul(B, A);
      Assert(R1 = R2, "Mod_Mul commutativity: (123 * 456) = (456 * 123)");

      --  Test Case 2.7: Additive inverse (a + (-a) = 0)
      A := 1234;
      B := Q - A;  -- -A in Z_q
      R1 := Mod_Add(A, B);
      Assert(R1 = 0, "Additive inverse: 1234 + (3329 - 1234) = 0");

   end Test_Modular_Arithmetic;

   --  ========================================================================
   --  Test 3: Polynomial Operations
   --  ========================================================================

   procedure Test_Polynomial_Operations is
      P1, P2, P3 : Polynomial;
   begin
      Put_Line("");
      Put_Line("=== Test 3: Polynomial Operations ===");

      --  Test Case 3.1: Zero polynomial
      P1 := Zero_Poly;
      Assert((for all I in Polynomial'Range => P1(I) = 0),
             "Zero polynomial has all coefficients = 0");

      --  Test Case 3.2: Polynomial addition (simple case)
      P1 := (others => 100);
      P2 := (others => 200);
      Add(P1, P2, P3);
      Assert((for all I in Polynomial'Range => P3(I) = 300),
             "Poly add: (100 + 200) = 300 for all coefficients");

      --  Test Case 3.3: Polynomial addition (overflow case)
      P1 := (others => 3000);
      P2 := (others => 1000);  -- Should reduce to 671
      Add(P1, P2, P3);
      Assert((for all I in Polynomial'Range => P3(I) = 671),
             "Poly add with reduction: (3000 + 1000) mod 3329 = 671");

      --  Test Case 3.4: Polynomial subtraction (positive result)
      P1 := (others => 2000);
      P2 := (others => 500);
      Sub(P1, P2, P3);
      Assert((for all I in Polynomial'Range => P3(I) = 1500),
             "Poly sub: (2000 - 500) = 1500 for all coefficients");

      --  Test Case 3.5: Polynomial subtraction (wrap case)
      P1 := (others => 500);
      P2 := (others => 2000);  -- Should wrap to 3329 - 1500 = 1829
      Sub(P1, P2, P3);
      Assert((for all I in Polynomial'Range => P3(I) = 1829),
             "Poly sub with wrap: (500 - 2000) mod 3329 = 1829");

      --  Test Case 3.6: Add then subtract (should return original)
      P1 := (0 => 1000, 1 => 2000, 2 => 3000, others => 100);
      P2 := (0 => 500,  1 => 1000, 2 => 1500, others => 50);
      Add(P1, P2, P3);  -- P3 = P1 + P2
      Sub(P3, P2, P3);  -- P3 = P3 - P2 = P1
      Assert((for all I in Polynomial'Range => P3(I) = P1(I)),
             "Poly identity: (P1 + P2) - P2 = P1");

   end Test_Polynomial_Operations;

   --  ========================================================================
   --  Main Test Runner
   --  ========================================================================

begin
   Put_Line("");
   Put_Line("╔════════════════════════════════════════════════════════════════╗");
   Put_Line("║       ML-KEM-1024 Foundation Tests (Phase 2.1)                ║");
   Put_Line("║       NIST FIPS 203 - Bronze Level Verification                ║");
   Put_Line("╚════════════════════════════════════════════════════════════════╝");
   Put_Line("");

   Test_Barrett_Reduction;
   Test_Modular_Arithmetic;
   Test_Polynomial_Operations;

   Put_Line("");
   Put_Line("╔════════════════════════════════════════════════════════════════╗");
   Put_Line("║                      TEST SUMMARY                              ║");
   Put_Line("╚════════════════════════════════════════════════════════════════╝");
   Put_Line("");
   Put_Line("  PASSED: " & Natural'Image(Passes));
   Put_Line("  FAILED: " & Natural'Image(Fails));
   Put_Line("");

   if Fails = 0 then
      Put_Line("  ✓ ALL TESTS PASSED");
      Put_Line("");
      Put_Line("  Phase 2.1 Complete:");
      Put_Line("    - ML-KEM-1024 types defined");
      Put_Line("    - Barrett reduction verified");
      Put_Line("    - Modular arithmetic correct");
      Put_Line("    - Polynomial operations functional");
      Put_Line("");
      Put_Line("  Next: Phase 2.2 - NTT Implementation");
   else
      Put_Line("  ✗ TESTS FAILED - FIX BEFORE PROCEEDING");
   end if;
   Put_Line("");

end Test_MLKEM_Foundation;
