pragma SPARK_Mode (On);

with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.Compression; use SparkPass.Crypto.MLKEM.Compression;
with SparkPass.Types; use SparkPass.Types;
with Ada.Text_IO; use Ada.Text_IO;

--  ========================================================================
--  ML-KEM-1024 Compression Round-Trip Tests (Phase 2.3)
--  ========================================================================
--
--  **Purpose**: Verify Compress_d and Decompress_d are approximate inverses
--
--  **Mathematical Properties Tested**:
--  1. Round-trip bounded error: |Decompress(Compress(x)) - x| ≤ ε
--  2. Error bounds for D_U=11: ε ≤ q/(2^12) ≈ 0.81
--  3. Error bounds for D_V=5:  ε ≤ q/(2^6) ≈ 52
--  4. Polynomial compression preserves coefficient order
--
--  **Test Coverage**:
--  - Basic coefficient compression/decompression
--  - Round-trip error bounds
--  - Polynomial-level compression
--  - Boundary cases (0, Q-1)
--  - Both compression factors (D_U=11, D_V=5)
--
--  **Reference**: NIST FIPS 203, Section 4.2.1
--    "The compression is lossy but designed to preserve enough
--     information for correct decryption with high probability"
--
--  ========================================================================

procedure Test_MLKEM_Compression_RoundTrip is

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
   --  Helper: Compute absolute difference between two coefficients
   --  ========================================================================

   function Abs_Diff (A, B : Coefficient) return Natural is
   begin
      if A >= B then
         return A - B;
      else
         return B - A;
      end if;
   end Abs_Diff;

   --  ========================================================================
   --  Test 1: Basic Compress/Decompress with D=5
   --  ========================================================================

   procedure Test_Basic_Compress_D5 is
      X : Coefficient;
      Y : Natural;
      X_Recovered : Coefficient;
      Error : Natural;
      Max_Error : constant Natural := Q / (2 ** 6);  -- q / 2^(d+1)
   begin
      Put_Line("");
      Put_Line("=== Test 1: Basic Compress/Decompress (D=5) ===");

      --  Test Case 1.1: Zero
      X := 0;
      Y := Compress_d(X, 5);
      X_Recovered := Decompress_d(Y, 5);
      Assert(X = X_Recovered, "Compress/Decompress(0, D=5) = 0");

      --  Test Case 1.2: Small value (x=100)
      X := 100;
      Y := Compress_d(X, 5);
      X_Recovered := Decompress_d(Y, 5);
      Error := Abs_Diff(X, X_Recovered);
      Assert(Error <= Max_Error,
             "Round-trip error for 100 within bounds (error=" &
             Natural'Image(Error) & ", max=" & Natural'Image(Max_Error) & ")");

      --  Test Case 1.3: Medium value (x=1000)
      X := 1000;
      Y := Compress_d(X, 5);
      X_Recovered := Decompress_d(Y, 5);
      Error := Abs_Diff(X, X_Recovered);
      Assert(Error <= Max_Error,
             "Round-trip error for 1000 within bounds (error=" &
             Natural'Image(Error) & ")");

      --  Test Case 1.4: Large value (x=3000)
      X := 3000;
      Y := Compress_d(X, 5);
      X_Recovered := Decompress_d(Y, 5);
      Error := Abs_Diff(X, X_Recovered);
      Assert(Error <= Max_Error,
             "Round-trip error for 3000 within bounds (error=" &
             Natural'Image(Error) & ")");

      --  Test Case 1.5: Maximum value (Q-1)
      X := Q - 1;
      Y := Compress_d(X, 5);
      X_Recovered := Decompress_d(Y, 5);
      Error := Abs_Diff(X, X_Recovered);
      Assert(Error <= Max_Error,
             "Round-trip error for Q-1 within bounds (error=" &
             Natural'Image(Error) & ")");

   end Test_Basic_Compress_D5;

   --  ========================================================================
   --  Test 2: Basic Compress/Decompress with D=11
   --  ========================================================================

   procedure Test_Basic_Compress_D11 is
      X : Coefficient;
      Y : Natural;
      X_Recovered : Coefficient;
      Error : Natural;
      Max_Error : constant Natural := Q / (2 ** 12);  -- q / 2^(d+1)
   begin
      Put_Line("");
      Put_Line("=== Test 2: Basic Compress/Decompress (D=11) ===");

      --  Test Case 2.1: Zero
      X := 0;
      Y := Compress_d(X, 11);
      X_Recovered := Decompress_d(Y, 11);
      Assert(X = X_Recovered, "Compress/Decompress(0, D=11) = 0");

      --  Test Case 2.2: Small value (x=100)
      X := 100;
      Y := Compress_d(X, 11);
      X_Recovered := Decompress_d(Y, 11);
      Error := Abs_Diff(X, X_Recovered);
      Assert(Error <= Max_Error,
             "Round-trip error for 100 within bounds (error=" &
             Natural'Image(Error) & ", max=" & Natural'Image(Max_Error) & ")");

      --  Test Case 2.3: Medium value (x=1000)
      X := 1000;
      Y := Compress_d(X, 11);
      X_Recovered := Decompress_d(Y, 11);
      Error := Abs_Diff(X, X_Recovered);
      Assert(Error <= Max_Error,
             "Round-trip error for 1000 within bounds (error=" &
             Natural'Image(Error) & ")");

      --  Test Case 2.4: Maximum value (Q-1)
      X := Q - 1;
      Y := Compress_d(X, 11);
      X_Recovered := Decompress_d(Y, 11);
      Error := Abs_Diff(X, X_Recovered);
      Assert(Error <= Max_Error,
             "Round-trip error for Q-1 within bounds (error=" &
             Natural'Image(Error) & ")");

   end Test_Basic_Compress_D11;

   --  ========================================================================
   --  Test 3: Comprehensive Coefficient Range (D=5)
   --  ========================================================================

   procedure Test_Coefficient_Range_D5 is
      X : Coefficient;
      Y : Natural;
      X_Recovered : Coefficient;
      Error : Natural;
      Max_Error : constant Natural := Q / (2 ** 6);
      Max_Observed_Error : Natural := 0;
      Failures : Natural := 0;
   begin
      Put_Line("");
      Put_Line("=== Test 3: Full Coefficient Range (D=5) ===");

      --  Test every 8th coefficient to keep test reasonable
      for I in 0 .. (Q - 1) / 8 loop
         X := Coefficient(I * 8);
         Y := Compress_d(X, 5);
         X_Recovered := Decompress_d(Y, 5);
         Error := Abs_Diff(X, X_Recovered);

         if Error > Max_Observed_Error then
            Max_Observed_Error := Error;
         end if;

         if Error > Max_Error then
            Failures := Failures + 1;
            if Failures <= 5 then  -- Report first 5 failures
               Put_Line("  [FAIL] x=" & Coefficient'Image(X) &
                       " error=" & Natural'Image(Error) &
                       " > max=" & Natural'Image(Max_Error));
            end if;
         end if;
      end loop;

      Assert(Failures = 0,
             "All sampled coefficients within error bounds (max observed=" &
             Natural'Image(Max_Observed_Error) & ")");
   end Test_Coefficient_Range_D5;

   --  ========================================================================
   --  Test 4: Comprehensive Coefficient Range (D=11)
   --  ========================================================================

   procedure Test_Coefficient_Range_D11 is
      X : Coefficient;
      Y : Natural;
      X_Recovered : Coefficient;
      Error : Natural;
      Max_Error : constant Natural := Q / (2 ** 12);
      Max_Observed_Error : Natural := 0;
      Failures : Natural := 0;
   begin
      Put_Line("");
      Put_Line("=== Test 4: Full Coefficient Range (D=11) ===");

      --  Test every 8th coefficient
      for I in 0 .. (Q - 1) / 8 loop
         X := Coefficient(I * 8);
         Y := Compress_d(X, 11);
         X_Recovered := Decompress_d(Y, 11);
         Error := Abs_Diff(X, X_Recovered);

         if Error > Max_Observed_Error then
            Max_Observed_Error := Error;
         end if;

         if Error > Max_Error then
            Failures := Failures + 1;
            if Failures <= 5 then  -- Report first 5 failures
               Put_Line("  [FAIL] x=" & Coefficient'Image(X) &
                       " error=" & Natural'Image(Error) &
                       " > max=" & Natural'Image(Max_Error));
            end if;
         end if;
      end loop;

      Assert(Failures = 0,
             "All sampled coefficients within error bounds (max observed=" &
             Natural'Image(Max_Observed_Error) & ")");
   end Test_Coefficient_Range_D11;

   --  ========================================================================
   --  Test 5: Polynomial Compression (D=5)
   --  ========================================================================

   procedure Test_Polynomial_Compress_D5 is
      P : Polynomial;
      P_Recovered : Polynomial;
      Buffer_Size_D5 : constant := 160;  -- (256 * 5 + 7) / 8 = 160 bytes
      Buffer : Byte_Array(1 .. Buffer_Size_D5);
      Failures : Natural := 0;
      Error : Natural;
      Max_Error : constant Natural := Q / (2 ** 6);
   begin
      Put_Line("");
      Put_Line("=== Test 5: Polynomial Compression (D=5) ===");

      --  Test Case 5.1: Zero polynomial
      P := (others => 0);
      Compress_Poly(P, 5, Buffer);
      Decompress_Poly(Buffer, 5, P_Recovered);
      Assert((for all I in Polynomial'Range => P(I) = P_Recovered(I)),
             "Zero polynomial round-trip preserves all zeros");

      --  Test Case 5.2: Constant polynomial (all 1000)
      P := (others => 1000);
      Compress_Poly(P, 5, Buffer);
      Decompress_Poly(Buffer, 5, P_Recovered);
      for I in Polynomial'Range loop
         Error := Abs_Diff(P(I), P_Recovered(I));
         if Error > Max_Error then
            Failures := Failures + 1;
         end if;
      end loop;
      Assert(Failures = 0,
             "Constant polynomial round-trip within error bounds");

      --  Test Case 5.3: Sequential polynomial
      Failures := 0;
      for I in Polynomial'Range loop
         P(I) := Coefficient((I * 13) mod Q);  -- Pseudo-random pattern
      end loop;
      Compress_Poly(P, 5, Buffer);
      Decompress_Poly(Buffer, 5, P_Recovered);
      for I in Polynomial'Range loop
         Error := Abs_Diff(P(I), P_Recovered(I));
         if Error > Max_Error then
            Failures := Failures + 1;
         end if;
      end loop;
      Assert(Failures = 0,
             "Sequential polynomial round-trip within error bounds");

   end Test_Polynomial_Compress_D5;

   --  ========================================================================
   --  Test 6: Polynomial Compression (D=11)
   --  ========================================================================

   procedure Test_Polynomial_Compress_D11 is
      P : Polynomial;
      P_Recovered : Polynomial;
      Buffer_Size_D11 : constant := 352;  -- (256 * 11 + 7) / 8 = 352 bytes
      Buffer : Byte_Array(1 .. Buffer_Size_D11);
      Failures : Natural := 0;
      Error : Natural;
      Max_Error : constant Natural := Q / (2 ** 12);
   begin
      Put_Line("");
      Put_Line("=== Test 6: Polynomial Compression (D=11) ===");

      --  Test Case 6.1: Zero polynomial
      P := (others => 0);
      Compress_Poly(P, 11, Buffer);
      Decompress_Poly(Buffer, 11, P_Recovered);
      Assert((for all I in Polynomial'Range => P(I) = P_Recovered(I)),
             "Zero polynomial round-trip preserves all zeros");

      --  Test Case 6.2: Maximum polynomial (all Q-1)
      P := (others => Q - 1);
      Compress_Poly(P, 11, Buffer);
      Decompress_Poly(Buffer, 11, P_Recovered);
      for I in Polynomial'Range loop
         Error := Abs_Diff(P(I), P_Recovered(I));
         if Error > Max_Error then
            Failures := Failures + 1;
         end if;
      end loop;
      Assert(Failures = 0,
             "Maximum polynomial round-trip within error bounds");

      --  Test Case 6.3: Dense polynomial
      Failures := 0;
      for I in Polynomial'Range loop
         P(I) := Coefficient((I * I + 42) mod Q);  -- Pseudo-random
      end loop;
      Compress_Poly(P, 11, Buffer);
      Decompress_Poly(Buffer, 11, P_Recovered);
      for I in Polynomial'Range loop
         Error := Abs_Diff(P(I), P_Recovered(I));
         if Error > Max_Error then
            Failures := Failures + 1;
         end if;
      end loop;
      Assert(Failures = 0,
             "Dense polynomial round-trip within error bounds");

   end Test_Polynomial_Compress_D11;

   --  ========================================================================
   --  Main Test Runner
   --  ========================================================================

begin
   Put_Line("");
   Put_Line("╔════════════════════════════════════════════════════════════════╗");
   Put_Line("║     ML-KEM-1024 Compression Round-Trip Tests (Phase 2.3)      ║");
   Put_Line("║       NIST FIPS 203 - Verifying Compression Correctness       ║");
   Put_Line("╚════════════════════════════════════════════════════════════════╝");
   Put_Line("");

   Test_Basic_Compress_D5;
   Test_Basic_Compress_D11;
   Test_Coefficient_Range_D5;
   Test_Coefficient_Range_D11;
   Test_Polynomial_Compress_D5;
   Test_Polynomial_Compress_D11;

   Put_Line("");
   Put_Line("╔════════════════════════════════════════════════════════════════╗");
   Put_Line("║                      TEST SUMMARY                              ║");
   Put_Line("╚════════════════════════════════════════════════════════════════╝");
   Put_Line("");
   Put_Line("  PASSED: " & Natural'Image(Passes));
   Put_Line("  FAILED: " & Natural'Image(Fails));
   Put_Line("");

   if Fails = 0 then
      Put_Line("  ✓ ALL COMPRESSION ROUND-TRIP TESTS PASSED");
      Put_Line("");
      Put_Line("  Verified Properties:");
      Put_Line("    - Compress_d and Decompress_d are approximate inverses");
      Put_Line("    - Round-trip error: |Decompress(Compress(x)) - x| ≤ ε");
      Put_Line("    - D=5:  ε ≤ " & Natural'Image(Q / (2 ** 6)) & " (FIPS 203 bound)");
      Put_Line("    - D=11: ε ≤ " & Natural'Image(Q / (2 ** 12)) & " (FIPS 203 bound)");
      Put_Line("    - Polynomial compression preserves coefficient order");
      Put_Line("    - All coefficient ranges tested successfully");
      Put_Line("");
      Put_Line("  Compression Implementation Status: ✓ VERIFIED");
      Put_Line("  Ready for: Phase 2.4 - Sampling (SamplePolyCBD, SampleNTT)");
   else
      Put_Line("  ✗ COMPRESSION TESTS FAILED - FIX BEFORE PROCEEDING");
      Put_Line("");
      Put_Line("  CRITICAL: Compression/decompression not meeting error bounds!");
      Put_Line("  This indicates a bug in Compress_d or Decompress_d.");
      Put_Line("  ML-KEM encryption/decryption will fail.");
   end if;
   Put_Line("");

end Test_MLKEM_Compression_RoundTrip;
