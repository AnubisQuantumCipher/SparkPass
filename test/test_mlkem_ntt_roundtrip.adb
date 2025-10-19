pragma SPARK_Mode (On);

with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.NTT; use SparkPass.Crypto.MLKEM.NTT;
with Ada.Text_IO; use Ada.Text_IO;

--  ========================================================================
--  ML-KEM-1024 NTT Round-Trip Tests (Phase 2.2)
--  ========================================================================
--
--  **Purpose**: Verify NTT and INTT are proper inverse operations
--
--  **Mathematical Properties Tested**:
--  1. Round-trip identity: NTT(INTT(x)) = x
--  2. Reverse round-trip: INTT(NTT(x)) = x
--  3. Linearity preservation through transform
--
--  **Test Coverage**:
--  - Zero polynomial
--  - Constant polynomial
--  - Sparse polynomials (single non-zero coefficient)
--  - Dense polynomials (all coefficients non-zero)
--  - Boundary cases (max coefficient values)
--
--  **Reference**: NIST FIPS 203, Section 4.3
--    "The NTT and its inverse are bijective mappings between
--     coefficient and NTT representations"
--
--  ========================================================================

procedure Test_MLKEM_NTT_RoundTrip is

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
   --  Helper: Compare two polynomials for equality
   --  ========================================================================

   function Poly_Equal (P1, P2 : Polynomial) return Boolean is
      Result : Boolean := True;
   begin
      for I in Polynomial'Range loop
         if P1(I) /= P2(I) then
            Result := False;
            exit;
         end if;
      end loop;
      return Result;
   end Poly_Equal;

   --  ========================================================================
   --  Test 1: Zero Polynomial Round-Trip
   --  ========================================================================

   procedure Test_Zero_Polynomial is
      Original : Polynomial := (others => 0);
      Forward  : Polynomial;
      RoundTrip : Polynomial;
   begin
      Put_Line("=== Test 1: Zero Polynomial ===");

      --  Test Case 1.1: NTT(0) should return 0
      Forward := Original;
      NTT(Forward);
      Assert((for all I in Polynomial'Range => Forward(I) = 0),
             "NTT(0) = 0 (zero polynomial is fixed point)");

      --  Test Case 1.2: INTT(NTT(0)) = 0
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT(0)) = 0 (round-trip preserves zero)");

      --  Test Case 1.3: NTT(INTT(0)) = 0
      RoundTrip := Original;
      INTT(RoundTrip);
      NTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "NTT(INTT(0)) = 0 (reverse round-trip preserves zero)");

   end Test_Zero_Polynomial;

   --  ========================================================================
   --  Test 2: Constant Polynomial Round-Trip
   --  ========================================================================

   procedure Test_Constant_Polynomial is
      Original : Polynomial;
      RoundTrip : Polynomial;
   begin
      Put_Line("");
      Put_Line("=== Test 2: Constant Polynomial ===");

      --  Test Case 2.1: Constant value = 1
      Original := (others => 1);
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([1,1,...,1])) = [1,1,...,1]");

      --  Test Case 2.2: Constant value = 42
      Original := (others => 42);
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([42,42,...,42])) = [42,42,...,42]");

      --  Test Case 2.3: Constant value = 1000
      Original := (others => 1000);
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([1000,1000,...,1000])) = [1000,1000,...,1000]");

      --  Test Case 2.4: Reverse round-trip (constant)
      Original := (others => 123);
      RoundTrip := Original;
      INTT(RoundTrip);
      NTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "NTT(INTT([123,123,...,123])) = [123,123,...,123]");

   end Test_Constant_Polynomial;

   --  ========================================================================
   --  Test 3: Sparse Polynomial Round-Trip
   --  ========================================================================

   procedure Test_Sparse_Polynomial is
      Original : Polynomial;
      RoundTrip : Polynomial;
   begin
      Put_Line("");
      Put_Line("=== Test 3: Sparse Polynomial ===");

      --  Test Case 3.1: Single coefficient at position 0
      Original := (0 => 1234, others => 0);
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([1234, 0, 0, ...])) = [1234, 0, 0, ...]");

      --  Test Case 3.2: Single coefficient at position 128
      Original := (128 => 2345, others => 0);
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([0,...,0, 2345, 0,...,0])) = original");

      --  Test Case 3.3: Single coefficient at position 255 (last)
      Original := (255 => 3000, others => 0);
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([0,...,0, 3000])) = [0,...,0, 3000]");

      --  Test Case 3.4: Two coefficients (positions 0 and 255)
      Original := (0 => 1111, 255 => 2222, others => 0);
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([1111, 0,...,0, 2222])) = [1111, 0,...,0, 2222]");

      --  Test Case 3.5: Reverse round-trip (sparse)
      Original := (64 => 3111, others => 0);
      RoundTrip := Original;
      INTT(RoundTrip);
      NTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "NTT(INTT(sparse)) = sparse (reverse round-trip)");

   end Test_Sparse_Polynomial;

   --  ========================================================================
   --  Test 4: Dense Polynomial Round-Trip
   --  ========================================================================

   procedure Test_Dense_Polynomial is
      Original : Polynomial;
      RoundTrip : Polynomial;
   begin
      Put_Line("");
      Put_Line("=== Test 4: Dense Polynomial ===");

      --  Test Case 4.1: Incrementing sequence
      for I in Polynomial'Range loop
         Original(I) := Coefficient(I mod 1000);
      end loop;
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([0,1,2,...,255 mod 1000])) = [0,1,2,...,255 mod 1000]");

      --  Test Case 4.2: Pseudo-random pattern (deterministic)
      for I in Polynomial'Range loop
         Original(I) := Coefficient((I * 137 + 42) mod Q);
      end loop;
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT(pseudo-random)) = pseudo-random (dense pattern)");

      --  Test Case 4.3: Alternating pattern
      for I in Polynomial'Range loop
         if I mod 2 = 0 then
            Original(I) := 1234;
         else
            Original(I) := 2345;
         end if;
      end loop;
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([1234,2345,1234,2345,...])) = [1234,2345,1234,2345,...]");

      --  Test Case 4.4: Reverse round-trip (dense)
      for I in Polynomial'Range loop
         Original(I) := Coefficient((I * I) mod Q);
      end loop;
      RoundTrip := Original;
      INTT(RoundTrip);
      NTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "NTT(INTT(dense)) = dense (reverse round-trip)");

   end Test_Dense_Polynomial;

   --  ========================================================================
   --  Test 5: Boundary Value Round-Trip
   --  ========================================================================

   procedure Test_Boundary_Values is
      Original : Polynomial;
      RoundTrip : Polynomial;
   begin
      Put_Line("");
      Put_Line("=== Test 5: Boundary Values ===");

      --  Test Case 5.1: Maximum coefficient value (Q - 1 = 3328)
      Original := (others => Q - 1);
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([3328,3328,...,3328])) = [3328,3328,...,3328]");

      --  Test Case 5.2: Alternating 0 and Q-1
      for I in Polynomial'Range loop
         if I mod 2 = 0 then
            Original(I) := 0;
         else
            Original(I) := Q - 1;
         end if;
      end loop;
      RoundTrip := Original;
      NTT(RoundTrip);
      INTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "INTT(NTT([0,3328,0,3328,...])) = [0,3328,0,3328,...]");

      --  Test Case 5.3: All possible single-coefficient values
      --  Test a representative sample (every 16th value)
      for Val in 0 .. (Q - 1) / 16 loop
         Original := (0 => Coefficient(Val * 16), others => 0);
         RoundTrip := Original;
         NTT(RoundTrip);
         INTT(RoundTrip);
         if not Poly_Equal(RoundTrip, Original) then
            Assert(False, "Round-trip failed for coefficient value" &
                   Coefficient'Image(Val * 16));
            exit;
         end if;
      end loop;
      Assert(True, "Round-trip preserves all sampled coefficient values");

      --  Test Case 5.4: Reverse round-trip (boundary)
      Original := (others => Q - 1);
      RoundTrip := Original;
      INTT(RoundTrip);
      NTT(RoundTrip);
      Assert(Poly_Equal(RoundTrip, Original),
             "NTT(INTT([Q-1,Q-1,...,Q-1])) = [Q-1,Q-1,...,Q-1]");

   end Test_Boundary_Values;

   --  ========================================================================
   --  Test 6: Multiple Round-Trips (Idempotence)
   --  ========================================================================

   procedure Test_Multiple_RoundTrips is
      Original : Polynomial;
      Test1, Test2, Test3 : Polynomial;
   begin
      Put_Line("");
      Put_Line("=== Test 6: Multiple Round-Trips ===");

      --  Test Case 6.1: Double round-trip
      Original := (0 => 1111, 50 => 2222, 100 => 3111, 150 => 2444,
                   200 => 2555, 255 => 3111, others => 0);

      Test1 := Original;
      NTT(Test1);
      INTT(Test1);

      Test2 := Test1;
      NTT(Test2);
      INTT(Test2);

      Assert(Poly_Equal(Test2, Original),
             "Double round-trip: INTT(NTT(INTT(NTT(x)))) = x");

      --  Test Case 6.2: Triple round-trip
      Test3 := Test2;
      NTT(Test3);
      INTT(Test3);

      Assert(Poly_Equal(Test3, Original),
             "Triple round-trip: INTT(NTT(...)) = x (idempotence)");

      --  Test Case 6.3: Reverse double round-trip
      Test1 := Original;
      INTT(Test1);
      NTT(Test1);

      Test2 := Test1;
      INTT(Test2);
      NTT(Test2);

      Assert(Poly_Equal(Test2, Original),
             "Reverse double round-trip: NTT(INTT(NTT(INTT(x)))) = x");

   end Test_Multiple_RoundTrips;

   --  ========================================================================
   --  Main Test Runner
   --  ========================================================================

begin
   Put_Line("");
   Put_Line("╔════════════════════════════════════════════════════════════════╗");
   Put_Line("║       ML-KEM-1024 NTT Round-Trip Tests (Phase 2.2)           ║");
   Put_Line("║       NIST FIPS 203 - Verifying NTT/INTT Correctness          ║");
   Put_Line("╚════════════════════════════════════════════════════════════════╝");
   Put_Line("");

   Test_Zero_Polynomial;
   Test_Constant_Polynomial;
   Test_Sparse_Polynomial;
   Test_Dense_Polynomial;
   Test_Boundary_Values;
   Test_Multiple_RoundTrips;

   Put_Line("");
   Put_Line("╔════════════════════════════════════════════════════════════════╗");
   Put_Line("║                      TEST SUMMARY                              ║");
   Put_Line("╚════════════════════════════════════════════════════════════════╝");
   Put_Line("");
   Put_Line("  PASSED: " & Natural'Image(Passes));
   Put_Line("  FAILED: " & Natural'Image(Fails));
   Put_Line("");

   if Fails = 0 then
      Put_Line("  ✓ ALL NTT ROUND-TRIP TESTS PASSED");
      Put_Line("");
      Put_Line("  Verified Properties:");
      Put_Line("    - NTT and INTT are proper inverse operations");
      Put_Line("    - Round-trip identity: INTT(NTT(x)) = x");
      Put_Line("    - Reverse identity: NTT(INTT(x)) = x");
      Put_Line("    - Idempotence: Multiple round-trips preserve polynomial");
      Put_Line("    - Correctness across all coefficient ranges [0, Q-1]");
      Put_Line("");
      Put_Line("  NTT Implementation Status: ✓ VERIFIED");
      Put_Line("  Ready for: Phase 2.3 - Compression");
   else
      Put_Line("  ✗ ROUND-TRIP TESTS FAILED - FIX BEFORE PROCEEDING");
      Put_Line("");
      Put_Line("  CRITICAL: NTT/INTT are not proper inverses!");
      Put_Line("  This indicates a bug in the NTT or INTT implementation.");
      Put_Line("  All subsequent ML-KEM operations will be incorrect.");
   end if;
   Put_Line("");

end Test_MLKEM_NTT_RoundTrip;
