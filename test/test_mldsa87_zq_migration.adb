--  ============================================================================
--  ML-DSA-87 Zq Migration Unit Tests
--  ============================================================================
--
--  Verifies that the Zq [0, Q-1] migration works correctly:
--    1. Domain conversion round-trips
--    2. NTT/NTT_Inv round-trips
--    3. Packing/unpacking round-trips
--    4. Modular arithmetic correctness
--
--  ============================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with SparkPass.Crypto.MLDSA87.Params;  use SparkPass.Crypto.MLDSA87.Params;
with SparkPass.Crypto.MLDSA87.ZQ_Ops;
with SparkPass.Crypto.MLDSA87.Poly;    use SparkPass.Crypto.MLDSA87.Poly;
with SparkPass.Crypto.MLDSA87.Packing; use SparkPass.Crypto.MLDSA87.Packing;
with SparkPass.Types; use SparkPass.Types;

procedure Test_MLDSA87_Zq_Migration is

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Put_Line ("[PASS] " & Test_Name);
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("[FAIL] " & Test_Name);
      end if;
   end Assert;

   --  ==========================================================================
   --  Test 1: Domain Conversion Round-Trips
   --  ==========================================================================

   procedure Test_Domain_Conversions is
      use SparkPass.Crypto.MLDSA87.ZQ_Ops;
      C : Coeff_Centered;
      Z : Zq;
   begin
      Put_Line ("=== Test 1: Domain Conversion Round-Trips ===");

      --  Test positive values
      C := 42;
      Z := To_Zq (C);
      Assert (To_Centered (Z) = C, "To_Zq / To_Centered round-trip (positive)");

      --  Test negative values
      C := -42;
      Z := To_Zq (C);
      Assert (To_Centered (Z) = C, "To_Zq / To_Centered round-trip (negative)");

      --  Test zero
      C := 0;
      Z := To_Zq (C);
      Assert (To_Centered (Z) = C, "To_Zq / To_Centered round-trip (zero)");

      --  Test boundary values
      C := (Q - 1) / 2;  -- Maximum positive centered value
      Z := To_Zq (C);
      Assert (To_Centered (Z) = C, "To_Zq / To_Centered round-trip (max positive)");

      C := -((Q - 1) / 2);  -- Minimum negative centered value
      Z := To_Zq (C);
      Assert (To_Centered (Z) = C, "To_Zq / To_Centered round-trip (max negative)");

      Put_Line ("");
   end Test_Domain_Conversions;

   --  ==========================================================================
   --  Test 2: Modular Arithmetic
   --  ==========================================================================

   procedure Test_Modular_Arithmetic is
      use SparkPass.Crypto.MLDSA87.ZQ_Ops;
      A, B, C : Zq;
   begin
      Put_Line ("=== Test 2: Modular Arithmetic ===");

      --  Test AddQ
      A := 100;
      B := 200;
      C := AddQ (A, B);
      Assert (C = 300, "AddQ (100, 200) = 300");

      --  Test AddQ with wraparound
      A := Q - 10;
      B := 20;
      C := AddQ (A, B);
      Assert (C = 10, "AddQ with modular reduction");

      --  Test SubQ
      A := 300;
      B := 100;
      C := SubQ (A, B);
      Assert (C = 200, "SubQ (300, 100) = 200");

      --  Test SubQ with wraparound
      A := 10;
      B := 20;
      C := SubQ (A, B);
      Assert (C = Q - 10, "SubQ with modular wraparound");

      --  Test MontMul identity (assuming zetas are in Montgomery form)
      A := 1753;  -- First zeta value
      B := Mont_R;  -- Montgomery R
      C := MontMul (A, B);
      --  MontMul(A, R) should give A in Montgomery domain
      Assert (C > 0 and C < Q, "MontMul produces valid Zq");

      Put_Line ("");
   end Test_Modular_Arithmetic;

   --  ==========================================================================
   --  Test 3: NTT Round-Trip
   --  ==========================================================================

   procedure Test_NTT_Round_Trip is
      P_Original : Polynomial;
      P_Mont     : Polynomial;
      P_Result   : Polynomial;
      use SparkPass.Crypto.MLDSA87.ZQ_Ops;
   begin
      Put_Line ("=== Test 3: NTT Round-Trip ===");

      --  Initialize polynomial with test pattern (plain domain)
      for I in Poly_Index loop
         P_Original (I) := Zq (I mod Q);
      end loop;

      --  Encode to Montgomery domain before NTT
      Encode_Poly (P_Mont, P_Original);

      --  Forward NTT (operates in Montgomery domain)
      NTT (P_Mont);

      --  Inverse NTT (stays in Montgomery domain)
      NTT_Inv (P_Mont);

      --  Decode from Montgomery domain back to plain
      Decode_Poly (P_Result, P_Mont);

      --  Verify round-trip
      declare
         All_Match : Boolean := True;
      begin
         for I in Poly_Index loop
            if P_Result (I) /= P_Original (I) then
               All_Match := False;
               exit;
            end if;
         end loop;
         Assert (All_Match, "NTT_Inv(NTT(p)) = p");
      end;

      Put_Line ("");
   end Test_NTT_Round_Trip;

   --  ==========================================================================
   --  Test 4: Packing Round-Trips
   --  ==========================================================================

   procedure Test_Packing_Round_Trips is
      P_Original : Polynomial;
      P_Unpacked : Polynomial;
      Packed_t1  : Byte_Array (1 .. Poly_T1_Bytes);
      Packed_t0  : Byte_Array (1 .. Poly_T0_Bytes);
      Packed_s   : Byte_Array (1 .. Poly_S_Bytes);
      Packed_z   : Byte_Array (1 .. Poly_Z_Bytes);
      use SparkPass.Crypto.MLDSA87.ZQ_Ops;
   begin
      Put_Line ("=== Test 4: Packing Round-Trips ===");

      --  Test t1 packing (10 bits, direct Zq)
      for I in Poly_Index loop
         P_Original (I) := Zq (I mod 1024);  -- 10-bit range
      end loop;
      Pack_t1 (P_Original, Packed_t1);
      Unpack_t1 (Packed_t1, P_Unpacked);

      declare
         All_Match : Boolean := True;
      begin
         for I in Poly_Index loop
            if P_Unpacked (I) /= P_Original (I) then
               All_Match := False;
               exit;
            end if;
         end loop;
         Assert (All_Match, "Pack_t1 / Unpack_t1 round-trip");
      end;

      --  Test t0 packing (13 bits, centered)
      for I in Poly_Index loop
         --  Generate centered value in range [-2^12, 2^12)
         declare
            C : Coeff_Centered := Coeff_Centered ((I mod 4096) - 2048);
         begin
            P_Original (I) := To_Zq (C);
         end;
      end loop;
      Pack_t0 (P_Original, Packed_t0);
      Unpack_t0 (Packed_t0, P_Unpacked);

      declare
         All_Match : Boolean := True;
      begin
         for I in Poly_Index loop
            if P_Unpacked (I) /= P_Original (I) then
               All_Match := False;
               exit;
            end if;
         end loop;
         Assert (All_Match, "Pack_t0 / Unpack_t0 round-trip");
      end;

      --  Test s packing (3 bits, small centered)
      for I in Poly_Index loop
         declare
            C : Coeff_Centered := Coeff_Centered ((I mod 5) - 2);  -- [-2, 2]
         begin
            P_Original (I) := To_Zq (C);
         end;
      end loop;
      Pack_s (P_Original, Packed_s);
      Unpack_s (Packed_s, P_Unpacked);

      declare
         All_Match : Boolean := True;
      begin
         for I in Poly_Index loop
            if P_Unpacked (I) /= P_Original (I) then
               All_Match := False;
               exit;
            end if;
         end loop;
         Assert (All_Match, "Pack_s / Unpack_s round-trip");
      end;

      --  Test z packing (20 bits, centered around γ1)
      for I in Poly_Index loop
         declare
            C : Coeff_Centered := Coeff_Centered ((I mod 1000) - 500);
         begin
            P_Original (I) := To_Zq (C);
         end;
      end loop;
      Pack_z (P_Original, Packed_z);
      Unpack_z (Packed_z, P_Unpacked);

      declare
         All_Match : Boolean := True;
      begin
         for I in Poly_Index loop
            if P_Unpacked (I) /= P_Original (I) then
               All_Match := False;
               exit;
            end if;
         end loop;
         Assert (All_Match, "Pack_z / Unpack_z round-trip");
      end;

      Put_Line ("");
   end Test_Packing_Round_Trips;

   --  ==========================================================================
   --  Test 5: Polynomial Arithmetic
   --  ==========================================================================

   procedure Test_Polynomial_Arithmetic is
      A, B, C : Polynomial;
      use SparkPass.Crypto.MLDSA87.ZQ_Ops;
   begin
      Put_Line ("=== Test 5: Polynomial Arithmetic ===");

      --  Initialize test polynomials
      for I in Poly_Index loop
         A (I) := Zq (I mod 100);
         B (I) := Zq ((I + 50) mod 100);
      end loop;

      --  Test polynomial addition
      Add (C, A, B);
      declare
         Correct : Boolean := True;
      begin
         for I in Poly_Index loop
            if C (I) /= AddQ (A (I), B (I)) then
               Correct := False;
               exit;
            end if;
         end loop;
         Assert (Correct, "Polynomial Add uses AddQ correctly");
      end;

      --  Test polynomial subtraction
      Sub (C, A, B);
      declare
         Correct : Boolean := True;
      begin
         for I in Poly_Index loop
            if C (I) /= SubQ (A (I), B (I)) then
               Correct := False;
               exit;
            end if;
         end loop;
         Assert (Correct, "Polynomial Sub uses SubQ correctly");
      end;

      Put_Line ("");
   end Test_Polynomial_Arithmetic;

   --  ==========================================================================
   --  Main Test Execution
   --  ==========================================================================

begin
   Put_Line ("============================================================================");
   Put_Line ("ML-DSA-87 Zq Migration Unit Tests");
   Put_Line ("============================================================================");
   Put_Line ("");

   Test_Domain_Conversions;
   Test_Modular_Arithmetic;
   Test_NTT_Round_Trip;
   Test_Packing_Round_Trips;
   Test_Polynomial_Arithmetic;

   Put_Line ("============================================================================");
   Put_Line ("Test Results: " & Natural'Image (Pass_Count) & " / " & Natural'Image (Test_Count) & " tests passed");
   Put_Line ("============================================================================");

   if Pass_Count = Test_Count then
      Put_Line ("✓ ALL TESTS PASSED");
   else
      Put_Line ("✗ SOME TESTS FAILED");
   end if;

end Test_MLDSA87_Zq_Migration;
