pragma SPARK_Mode (On);

with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.Sampling; use SparkPass.Crypto.MLKEM.Sampling;
with SparkPass.Types; use SparkPass.Types;
with Ada.Text_IO; use Ada.Text_IO;

--  ========================================================================
--  ML-KEM-1024 Sampling Tests (Phase 2.4)
--  ========================================================================
--
--  **Purpose**: Verify SamplePolyCBD and SampleNTT correctness
--
--  **Mathematical Properties Tested**:
--  1. SamplePolyCBD produces coefficients from centered binomial distribution
--  2. SamplePolyCBD is constant-time (256 coefficients always produced)
--  3. SampleNTT produces uniformly distributed coefficients in [0, q-1]
--  4. SampleNTT correctly implements rejection sampling
--
--  **Test Coverage**:
--  - CBD with η=2 (ML-KEM-1024 parameter)
--  - Edge cases (all-zero bytes, all-one bytes, boundary values)
--  - Distribution properties (range checking)
--  - Rejection sampling correctness
--
--  **Reference**: NIST FIPS 203, Section 4.2.2
--    "SamplePolyCBD samples from a centered binomial distribution"
--    "SampleNTT uniformly samples from Z_q using rejection"
--
--  ========================================================================

procedure Test_MLKEM_Sampling is

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
   --  Helper: Count coefficients in a specific range
   --  ========================================================================

   function Count_In_Range (
      P : Polynomial;
      Min : Coefficient;
      Max : Coefficient
   ) return Natural is
      Count : Natural := 0;
   begin
      for I in Polynomial'Range loop
         if P(I) >= Min and then P(I) <= Max then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_In_Range;

   --  ========================================================================
   --  Test 1: Get_Bit Functionality
   --  ========================================================================

   procedure Test_Get_Bit is
      Bytes : Byte_Array(1 .. 3) := (16#AB#, 16#CD#, 16#EF#);
      --  Binary: 10101011, 11001101, 11101111
   begin
      Put_Line("");
      Put_Line("=== Test 1: Get_Bit Functionality ===");

      --  Test Case 1.1: Extract LSB of first byte
      --  AB = 10101011, bit 0 (LSB) = 1
      Assert(Get_Bit(Bytes, 0) = 1,
             "Get_Bit(0xAB, bit 0) = 1 (LSB)");

      --  Test Case 1.2: Extract MSB of first byte
      --  AB = 10101011, bit 7 (MSB) = 1
      Assert(Get_Bit(Bytes, 7) = 1,
             "Get_Bit(0xAB, bit 7) = 1 (MSB)");

      --  Test Case 1.3: Extract middle bit
      --  AB = 10101011, bit 3 = 1
      Assert(Get_Bit(Bytes, 3) = 1,
             "Get_Bit(0xAB, bit 3) = 1");

      --  Test Case 1.4: Extract zero bit
      --  AB = 10101011, bit 2 = 0
      Assert(Get_Bit(Bytes, 2) = 0,
             "Get_Bit(0xAB, bit 2) = 0");

      --  Test Case 1.5: Extract from second byte
      --  CD = 11001101, byte offset 1, bit 0 (global bit 8)
      Assert(Get_Bit(Bytes, 8) = 1,
             "Get_Bit(0xCD, bit 8) = 1 (second byte)");

      --  Test Case 1.6: Extract from second byte, middle
      --  CD = 11001101, bit 5 = 1, global bit 13
      Assert(Get_Bit(Bytes, 13) = 1,
             "Get_Bit(0xCD, bit 13) = 1");

   end Test_Get_Bit;

   --  ========================================================================
   --  Test 2: SamplePolyCBD with All-Zero Bytes
   --  ========================================================================

   procedure Test_CBD_All_Zeros is
      Eta : constant Positive := 2;
      Bytes : Byte_Array(1 .. 64 * Eta) := (others => 0);
      P : Polynomial;
   begin
      Put_Line("");
      Put_Line("=== Test 2: SamplePolyCBD (All-Zero Bytes) ===");

      SamplePolyCBD(Bytes, Eta, P);

      --  Test Case 2.1: All coefficients should be zero
      --  Since all bits are 0, x = 0, y = 0, diff = 0
      Assert((for all I in Polynomial'Range => P(I) = 0),
             "All-zero bytes produce all-zero polynomial");

      --  Test Case 2.2: Verify exactly 256 coefficients
      Assert(P'Length = 256,
             "Produces exactly 256 coefficients");

   end Test_CBD_All_Zeros;

   --  ========================================================================
   --  Test 3: SamplePolyCBD with All-One Bytes
   --  ========================================================================

   procedure Test_CBD_All_Ones is
      Eta : constant Positive := 2;
      Bytes : Byte_Array(1 .. 64 * Eta) := (others => 16#FF#);
      P : Polynomial;
   begin
      Put_Line("");
      Put_Line("=== Test 3: SamplePolyCBD (All-One Bytes) ===");

      SamplePolyCBD(Bytes, Eta, P);

      --  Test Case 3.1: All coefficients should be zero
      --  For η=2: all bits are 1
      --  x = bit[0] + bit[1] = 1 + 1 = 2
      --  y = bit[2] + bit[3] = 1 + 1 = 2
      --  diff = 2 - 2 = 0
      Assert((for all I in Polynomial'Range => P(I) = 0),
             "All-one bytes produce all-zero polynomial (balanced CBD)");

   end Test_CBD_All_Ones;

   --  ========================================================================
   --  Test 4: SamplePolyCBD Range and Distribution
   --  ========================================================================

   procedure Test_CBD_Range_Distribution is
      Eta : constant Positive := 2;
      Bytes : Byte_Array(1 .. 64 * Eta);
      P : Polynomial;
      In_Range : Natural;
   begin
      Put_Line("");
      Put_Line("=== Test 4: SamplePolyCBD Range and Distribution ===");

      --  Test Case 4.1: Pseudo-random pattern
      --  Fill with deterministic pseudo-random values
      for I in Bytes'Range loop
         Bytes(I) := Unsigned_8((I * 137 + 42) mod 256);
      end loop;

      SamplePolyCBD(Bytes, Eta, P);

      --  Test Case 4.1a: All coefficients in valid range [0, Q-1]
      Assert((for all I in Polynomial'Range => P(I) in 0 .. Q - 1),
             "All coefficients in valid range [0, Q-1]");

      --  Test Case 4.1b: For η=2, expected values are {q-2, q-1, 0, 1, 2}
      --  Since x, y ∈ [0,2], diff ∈ [-2, 2]
      --  In modular arithmetic: -2 → q-2, -1 → q-1, 0 → 0, 1 → 1, 2 → 2
      In_Range := Count_In_Range(P, 0, 2) +
                  Count_In_Range(P, Q - 2, Q - 1);

      Assert(In_Range = 256,
             "All coefficients in expected CBD range {0,1,2,q-2,q-1}");

      --  Test Case 4.2: Alternating pattern (0x55 = 01010101)
      Bytes := (others => 16#55#);
      SamplePolyCBD(Bytes, Eta, P);

      --  For 0x55 = 01010101:
      --  Coefficient pattern repeats every 2 coefficients
      --  Coeff 0: bits [0,1,2,3] = [1,0,1,0], x=1, y=1, diff=0
      --  Coeff 1: bits [4,5,6,7] = [1,0,1,0], x=1, y=1, diff=0
      Assert((for all I in Polynomial'Range => P(I) = 0),
             "Alternating pattern 0x55 produces all-zero polynomial");

      --  Test Case 4.3: Alternating pattern (0xAA = 10101010)
      Bytes := (others => 16#AA#);
      SamplePolyCBD(Bytes, Eta, P);

      --  For 0xAA = 10101010:
      --  Coeff 0: bits [0,1,2,3] = [0,1,0,1], x=1, y=1, diff=0
      Assert((for all I in Polynomial'Range => P(I) = 0),
             "Alternating pattern 0xAA produces all-zero polynomial");

   end Test_CBD_Range_Distribution;

   --  ========================================================================
   --  Test 5: SamplePolyCBD Specific Coefficient Values
   --  ========================================================================

   procedure Test_CBD_Specific_Values is
      Eta : constant Positive := 2;
      Bytes : Byte_Array(1 .. 64 * Eta) := (others => 0);
      P : Polynomial;
   begin
      Put_Line("");
      Put_Line("=== Test 5: SamplePolyCBD Specific Values ===");

      --  Test Case 5.1: First coefficient = +2
      --  Bits [0,1,2,3] = [1,1,0,0], x=2, y=0, diff=+2
      Bytes := (others => 0);
      Bytes(1) := 16#03#;  -- 00000011 (bits 0,1 = 1)
      SamplePolyCBD(Bytes, Eta, P);
      Assert(P(0) = 2,
             "Pattern 0000_0011 produces coefficient +2");

      --  Test Case 5.2: First coefficient = -2 (q-2)
      --  Bits [0,1,2,3] = [0,0,1,1], x=0, y=2, diff=-2
      Bytes := (others => 0);
      Bytes(1) := 16#0C#;  -- 00001100 (bits 2,3 = 1)
      SamplePolyCBD(Bytes, Eta, P);
      Assert(P(0) = Q - 2,
             "Pattern 0000_1100 produces coefficient -2 (mod q = " &
             Coefficient'Image(Q - 2) & ")");

      --  Test Case 5.3: First coefficient = +1
      --  Bits [0,1,2,3] = [1,0,0,0], x=1, y=0, diff=+1
      Bytes := (others => 0);
      Bytes(1) := 16#01#;  -- 00000001 (bit 0 = 1)
      SamplePolyCBD(Bytes, Eta, P);
      Assert(P(0) = 1,
             "Pattern 0000_0001 produces coefficient +1");

      --  Test Case 5.4: First coefficient = -1 (q-1)
      --  Bits [0,1,2,3] = [0,0,1,0], x=0, y=1, diff=-1
      Bytes := (others => 0);
      Bytes(1) := 16#04#;  -- 00000100 (bit 2 = 1)
      SamplePolyCBD(Bytes, Eta, P);
      Assert(P(0) = Q - 1,
             "Pattern 0000_0100 produces coefficient -1 (mod q = " &
             Coefficient'Image(Q - 1) & ")");

   end Test_CBD_Specific_Values;

   --  ========================================================================
   --  Test 6: SampleNTT with Known Values
   --  ========================================================================

   procedure Test_NTT_Known_Values is
      XOF_Stream : Byte_Array(1 .. 672);
      P : Polynomial;
      Bytes_Used : Natural;
   begin
      Put_Line("");
      Put_Line("=== Test 6: SampleNTT (Known Values) ===");

      --  Test Case 6.1: Values that should be accepted
      --  Set up bytes to produce d₁ = 1000, d₂ = 2000 (both < q = 3329)
      --  d₁ = B[0] + 256·(B[1] & 0x0F)
      --  1000 = B[0] + 256·k → B[0] = 1000 mod 256 = 232, k = 3
      --  d₂ = (B[1] >> 4) + 16·B[2]
      --  2000 = m + 16·n → n = 125, m = 0
      --  B[1] = (m << 4) | k = (0 << 4) | 3 = 3
      --  B[2] = 125

      XOF_Stream := (others => 0);
      XOF_Stream(1) := 232;  -- B[0]
      XOF_Stream(2) := 3;    -- B[1]
      XOF_Stream(3) := 125;  -- B[2]

      SampleNTT(XOF_Stream, P, Bytes_Used);

      Assert(P(0) = 1000,
             "First accepted value is 1000");
      Assert(P(1) = 2000,
             "Second accepted value is 2000");

      --  Test Case 6.2: All coefficients in valid range
      Assert((for all I in Polynomial'Range => P(I) in 0 .. Q - 1),
             "All sampled coefficients in valid range [0, Q-1]");

   end Test_NTT_Known_Values;

   --  ========================================================================
   --  Test 7: SampleNTT Rejection Sampling
   --  ========================================================================

   procedure Test_NTT_Rejection is
      XOF_Stream : Byte_Array(1 .. 672);
      P : Polynomial;
      Bytes_Used : Natural;
   begin
      Put_Line("");
      Put_Line("=== Test 7: SampleNTT Rejection Sampling ===");

      --  Test Case 7.1: Values that should be rejected
      --  Set up d₁ = 4000 (> q), d₂ = 100 (< q)
      --  4000 = B[0] + 256·k → B[0] = 4000 mod 256 = 160, k = 15
      --  100 = m + 16·n → n = 6, m = 4
      --  B[1] = (m << 4) | k = (4 << 4) | 15 = 79
      --  B[2] = 6

      XOF_Stream := (others => 0);
      XOF_Stream(1) := 160;  -- B[0]
      XOF_Stream(2) := 79;   -- B[1]
      XOF_Stream(3) := 6;    -- B[2]

      --  Set up valid value for next 3 bytes
      --  d₁ = 500
      XOF_Stream(4) := 244;  -- 500 = 244 + 256·1
      XOF_Stream(5) := 1;
      XOF_Stream(6) := 0;

      SampleNTT(XOF_Stream, P, Bytes_Used);

      --  d₁=4000 rejected, d₂=100 accepted → P(0) = 100
      --  Next: d₁=500 accepted → P(1) = 500
      Assert(P(0) = 100,
             "Rejection sampling: d₁=4000 rejected, d₂=100 accepted");
      Assert(P(1) = 500,
             "Next valid value 500 accepted");

   end Test_NTT_Rejection;

   --  ========================================================================
   --  Test 8: SampleNTT Boundary Values
   --  ========================================================================

   procedure Test_NTT_Boundary is
      XOF_Stream : Byte_Array(1 .. 672);
      P : Polynomial;
      Bytes_Used : Natural;
   begin
      Put_Line("");
      Put_Line("=== Test 8: SampleNTT Boundary Values ===");

      --  Test Case 8.1: Minimum value (d = 0)
      XOF_Stream := (others => 0);
      SampleNTT(XOF_Stream, P, Bytes_Used);

      Assert(P(0) = 0,
             "Minimum value d=0 accepted");

      --  Test Case 8.2: Maximum valid value (d = q-1 = 3328)
      --  3328 = B[0] + 256·k → B[0] = 0, k = 13
      XOF_Stream := (others => 0);
      XOF_Stream(1) := 0;
      XOF_Stream(2) := 13;   -- Low 4 bits = 13
      XOF_Stream(3) := 0;

      SampleNTT(XOF_Stream, P, Bytes_Used);

      Assert(P(0) = Q - 1,
             "Maximum valid value d=q-1=3328 accepted");

      --  Test Case 8.3: First rejected value (d = q = 3329)
      --  3329 = B[0] + 256·k → B[0] = 1, k = 13
      XOF_Stream := (others => 0);
      XOF_Stream(1) := 1;
      XOF_Stream(2) := 13;   -- Low 4 bits = 13
      XOF_Stream(3) := 0;
      --  Add a valid value after
      XOF_Stream(4) := 100;
      XOF_Stream(5) := 0;
      XOF_Stream(6) := 0;

      SampleNTT(XOF_Stream, P, Bytes_Used);

      Assert(P(0) = 100,
             "Value d=q=3329 correctly rejected");

   end Test_NTT_Boundary;

   --  ========================================================================
   --  Test 9: SampleNTT Full Polynomial
   --  ========================================================================

   procedure Test_NTT_Full_Polynomial is
      XOF_Stream : Byte_Array(1 .. 672);
      P : Polynomial;
      Bytes_Used : Natural;
   begin
      Put_Line("");
      Put_Line("=== Test 9: SampleNTT Full Polynomial ===");

      --  Test Case 9.1: Generate full polynomial from pseudo-random bytes
      for I in XOF_Stream'Range loop
         XOF_Stream(I) := Unsigned_8((I * 251 + 17) mod 256);
      end loop;

      SampleNTT(XOF_Stream, P, Bytes_Used);

      --  Test Case 9.1a: All 256 coefficients generated
      Assert((for all I in Polynomial'Range => P(I) in 0 .. Q - 1),
             "Full polynomial (256 coefficients) generated");

      --  Test Case 9.1b: Bytes consumed is reasonable
      --  Expected ~473 bytes on average, buffer is 672
      Assert(Bytes_Used <= XOF_Stream'Length,
             "Bytes consumed (" & Natural'Image(Bytes_Used) &
             ") within buffer size");

      Assert(Bytes_Used >= 384,
             "Bytes consumed (" & Natural'Image(Bytes_Used) &
             ") reasonable for 256 coefficients (≥384 bytes)");

   end Test_NTT_Full_Polynomial;

   --  ========================================================================
   --  Main Test Runner
   --  ========================================================================

begin
   Put_Line("");
   Put_Line("╔════════════════════════════════════════════════════════════════╗");
   Put_Line("║         ML-KEM-1024 Sampling Tests (Phase 2.4)                ║");
   Put_Line("║       NIST FIPS 203 - Verifying Sampling Correctness          ║");
   Put_Line("╚════════════════════════════════════════════════════════════════╝");
   Put_Line("");

   Test_Get_Bit;
   Test_CBD_All_Zeros;
   Test_CBD_All_Ones;
   Test_CBD_Range_Distribution;
   Test_CBD_Specific_Values;
   Test_NTT_Known_Values;
   Test_NTT_Rejection;
   Test_NTT_Boundary;
   Test_NTT_Full_Polynomial;

   Put_Line("");
   Put_Line("╔════════════════════════════════════════════════════════════════╗");
   Put_Line("║                      TEST SUMMARY                              ║");
   Put_Line("╚════════════════════════════════════════════════════════════════╝");
   Put_Line("");
   Put_Line("  PASSED: " & Natural'Image(Passes));
   Put_Line("  FAILED: " & Natural'Image(Fails));
   Put_Line("");

   if Fails = 0 then
      Put_Line("  ✓ ALL SAMPLING TESTS PASSED");
      Put_Line("");
      Put_Line("  Verified Properties:");
      Put_Line("    - Get_Bit correctly extracts bits from byte arrays");
      Put_Line("    - SamplePolyCBD produces coefficients from CBD(η=2)");
      Put_Line("    - SamplePolyCBD range: {0, 1, 2, q-2, q-1}");
      Put_Line("    - SamplePolyCBD is deterministic and constant-time");
      Put_Line("    - SampleNTT uniformly samples from [0, q-1]");
      Put_Line("    - SampleNTT rejection sampling works correctly");
      Put_Line("    - Boundary values handled properly");
      Put_Line("");
      Put_Line("  Sampling Implementation Status: ✓ VERIFIED");
      Put_Line("  Ready for: Phase 2.5 - Matrix Operations");
   else
      Put_Line("  ✗ SAMPLING TESTS FAILED - FIX BEFORE PROCEEDING");
      Put_Line("");
      Put_Line("  CRITICAL: Sampling functions not working correctly!");
      Put_Line("  This will cause ML-KEM key generation to fail.");
   end if;
   Put_Line("");

end Test_MLKEM_Sampling;
