--  ============================================================================
--  ML-DSA-87 Matrix Expansion Implementation
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Crypto.Keccak;
with SparkPass.Crypto.MLDSA87.ZQ_Ops;
with Interfaces; use Interfaces;

package body SparkPass.Crypto.MLDSA87.Matrix is

   --  =========================================================================
   --  Matrix Expansion (FIPS 204 Algorithm 28)
   --  =========================================================================

   procedure ExpandA (
      Rho : in  Byte_Array;
      A   : out Matrix_KxL
   ) is
      --  Rejection sampling state
      Seed : Byte_Array (1 .. 34);  -- ρ || i || j
      Stream : Byte_Array (1 .. 840);  -- SHAKE-128 output (enough for 256 coeffs)
      Coeff_Count : Natural;
      Idx : Natural;
      C0, C1, C2 : Unsigned_8;
      Candidate : Unsigned_32;
   begin
      --  Copy ρ into seed
      Seed (1 .. 32) := Rho;

      --  Generate each element A[i,j]
      for I in Vec_K_Index loop
         for J in Vec_L_Index loop
            --  Set i, j bytes
            Seed (33) := Unsigned_8 (J);
            Seed (34) := Unsigned_8 (I);

            --  Generate polynomial A[i,j] via rejection sampling
            --  FIPS 204: Sample uniformly from Zq using SHAKE-128

            Coeff_Count := 0;
            Idx := 1;

            --  Generate SHAKE-128 stream
            Keccak.SHAKE_128 (Seed, Stream);

            --  Rejection sampling to fill 256 coefficients
            while Coeff_Count < N loop
               exit when Idx + 2 > Stream'Last;

               C0 := Stream (Idx);
               C1 := Stream (Idx + 1);
               C2 := Stream (Idx + 2);
               Idx := Idx + 3;

               --  Parse 3 bytes as little-endian to get 23-bit value
               Candidate := Unsigned_32 (C0) +
                           Unsigned_32 (C1) * 256 +
                           Unsigned_32 (C2) * 65536;

               --  Take low 23 bits
               Candidate := Candidate and 16#7FFFFF#;

               --  Reject if >= q
               if Candidate < Q then
                  --  Store directly in Zq [0, Q-1] (no conversion needed)
                  A (I, J)(Coeff_Count) := Zq (Candidate);
                  Coeff_Count := Coeff_Count + 1;
               end if;
            end loop;

            --  If we didn't get enough coefficients, regenerate
            --  (extremely unlikely with 840 bytes)
            pragma Assert (Coeff_Count = N);
         end loop;
      end loop;
   end ExpandA;

   --  =========================================================================
   --  Matrix-Vector Multiplication
   --  =========================================================================

   procedure Matrix_Vec_Multiply (
      Result : out PolyVec_K;
      A      : in  Matrix_KxL;
      V      : in  PolyVec_L
   ) is
      Temp : Polynomial;
      Accum : Polynomial;
   begin
      --  For each row i of A
      for I in Vec_K_Index loop
         --  Initialize accumulator to zero
         for K in Poly_Index loop
            Accum (K) := 0;
         end loop;

         --  Compute dot product: A[i,:] · v
         for J in Vec_L_Index loop
            --  Pointwise multiply A[i,j] * v[j] (in NTT domain)
            Pointwise_Montgomery (Temp, A (I, J), V (J));

            --  Accumulate using modular addition
            for K in Poly_Index loop
               Accum (K) := SparkPass.Crypto.MLDSA87.ZQ_Ops.AddQ (Accum (K), Temp (K));
            end loop;
         end loop;

         --  Store result (already in Zq, no reduction needed)
         Result (I) := Accum;
      end loop;
   end Matrix_Vec_Multiply;

end SparkPass.Crypto.MLDSA87.Matrix;
