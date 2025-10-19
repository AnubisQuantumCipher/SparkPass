--  ============================================================================
--  ML-DSA-87 Sampling Routines Implementation
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Crypto.Keccak;
with SparkPass.Crypto.MLDSA87.ZQ_Ops;
with Interfaces; use Interfaces;

package body SparkPass.Crypto.MLDSA87.Sampling is

   --  =========================================================================
   --  Challenge Polynomial Sampling (FIPS 204 Algorithm 44)
   --  =========================================================================

   procedure SampleInBall (
      Seed   : in  Byte_Array;
      Result : out Polynomial
   ) is
      Stream : Byte_Array (1 .. 136);  -- SHAKE-256 output
      Signs  : Unsigned_64;
      Pos    : Natural;
      B      : Unsigned_8;
      Count  : Natural := 0;
   begin
      --  Initialize all coefficients to 0
      for I in Poly_Index loop
         Result (I) := 0;
      end loop;

      --  Generate SHAKE-256 stream
      Keccak.SHAKE_256 (Seed, Stream);

      --  Extract sign bits (8 bytes = 64 bits for τ=60 signs)
      Signs := 0;
      for I in 0 .. 7 loop
         Signs := Signs or Shift_Left (Unsigned_64 (Stream (I + 1)), I * 8);
      end loop;

      --  Place τ non-zero coefficients using Fisher-Yates shuffle
      Pos := N - Tau;

      for I in Pos .. N - 1 loop
         --  Get random position
         loop
            B := Stream (8 + Count + 1);
            Count := Count + 1;
            exit when B <= Unsigned_8 (I);
         end loop;

         --  Set coefficient at position B
         Result (Natural (B)) := Result (I);

         --  Set sign based on Signs bit (±1 in centered form, then to Zq)
         if (Signs and 1) = 1 then
            Result (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (-1);
         else
            Result (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (1);
         end if;

         Signs := Shift_Right (Signs, 1);
      end loop;
   end SampleInBall;

   --  =========================================================================
   --  Uniform Gamma1 Sampling (FIPS 204 Algorithm 39)
   --  =========================================================================

   procedure UniformGamma1 (
      Seed    : in  Byte_Array;
      Counter : in  Unsigned_16;
      Result  : out Polynomial
   ) is
      Seed_Extended : Byte_Array (1 .. 66);
      Stream : Byte_Array (1 .. 768);  -- 3 bytes per coeff * 256 (read 24 bits, mask to 20)
      Idx : Natural := 1;
      Z : Unsigned_32;
   begin
      --  Prepare seed || counter
      Seed_Extended (1 .. 64) := Seed;
      Seed_Extended (65) := Unsigned_8 (Counter and 16#FF#);
      Seed_Extended (66) := Unsigned_8 (Shift_Right (Counter, 8));

      --  Generate SHAKE-256 stream
      Keccak.SHAKE_256 (Seed_Extended, Stream);

      --  Extract coefficients (20 bits each, centered around 0)
      for I in Poly_Index loop
         --  Read 3 bytes, take low 20 bits
         Z := Unsigned_32 (Stream (Idx)) +
              Unsigned_32 (Stream (Idx + 1)) * 256 +
              Unsigned_32 (Stream (Idx + 2)) * 65536;
         Z := Z and 16#FFFFF#;  -- Mask to 20 bits
         Idx := Idx + 3;

         --  Convert to signed, centered at 0: [0, 2γ1) → [-γ1, γ1)
         declare
            C : Integer_32 := Integer_32 (Z) - Gamma1;
         begin
            Result (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (C);
         end;
      end loop;
   end UniformGamma1;

   procedure UniformGamma1_Vec_L (
      Seed   : in  Byte_Array;
      Kappa  : in  Unsigned_16;
      Result : out PolyVec_L
   ) is
   begin
      for I in Vec_L_Index loop
         UniformGamma1 (Seed, Kappa + Unsigned_16 (I), Result (I));
      end loop;
   end UniformGamma1_Vec_L;

   --  =========================================================================
   --  Small Coefficient Sampling (FIPS 204 Algorithm 38)
   --  =========================================================================

   procedure UniformEta (
      Seed    : in  Byte_Array;
      Counter : in  Unsigned_16;
      Result  : out Polynomial
   ) is
      Stream : Byte_Array (1 .. 192);  -- Enough for rejection sampling
      Idx : Natural := 1;
      Count : Natural := 0;
      T0, T1 : Unsigned_8;
      C0, C1 : Integer_32;
   begin
      --  Prepare seed
      declare
         Seed_Ext : Byte_Array (1 .. 66);
      begin
         Seed_Ext (1 .. 64) := Seed (1 .. 64);
         Seed_Ext (65) := Unsigned_8 (Counter and 16#FF#);
         Seed_Ext (66) := Unsigned_8 (Shift_Right (Counter, 8));

         --  Generate SHAKE-256 stream
         Keccak.SHAKE_256 (Seed_Ext, Stream);
      end;

      --  Rejection sampling for η = 2
      --  Each byte encodes 2 coefficients (4 bits each)
      while Count < N loop
         exit when Idx > Stream'Last;

         T0 := Stream (Idx) and 16#0F#;
         T1 := Shift_Right (Stream (Idx), 4);
         Idx := Idx + 1;

         --  For η = 2: valid range is [0, 4]
         --  Maps to coefficients: 0→-2, 1→-1, 2→0, 3→1, 4→2
         if T0 <= 4 then
            C0 := Integer_32 (T0) - Eta;
            Result (Count) := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (C0);
            Count := Count + 1;
         end if;

         if Count < N and then T1 <= 4 then
            C1 := Integer_32 (T1) - Eta;
            Result (Count) := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (C1);
            Count := Count + 1;
         end if;
      end loop;
   end UniformEta;

   procedure UniformEta_Vec_L (
      Seed   : in  Byte_Array;
      Offset : in  Unsigned_16;
      Result : out PolyVec_L
   ) is
   begin
      for I in Vec_L_Index loop
         UniformEta (Seed, Offset + Unsigned_16 (I), Result (I));
      end loop;
   end UniformEta_Vec_L;

   procedure UniformEta_Vec_K (
      Seed   : in  Byte_Array;
      Offset : in  Unsigned_16;
      Result : out PolyVec_K
   ) is
   begin
      for I in Vec_K_Index loop
         UniformEta (Seed, Offset + Unsigned_16 (I), Result (I));
      end loop;
   end UniformEta_Vec_K;

end SparkPass.Crypto.MLDSA87.Sampling;
