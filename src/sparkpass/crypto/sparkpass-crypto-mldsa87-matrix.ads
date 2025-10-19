--  ============================================================================
--  ML-DSA-87 Matrix Expansion (FIPS 204 Section 3.5)
--  ============================================================================
--
--  **Purpose**: Expand matrix A from seed ρ using SHAKE-128
--
--  **Algorithm**: FIPS 204 Algorithm 28 (ExpandA)
--
--  Matrix A is k×l where each element is a polynomial in NTT form.
--  Generated deterministically from 32-byte seed ρ.
--
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Crypto.MLDSA87.Params;  use SparkPass.Crypto.MLDSA87.Params;
with SparkPass.Crypto.MLDSA87.Poly;    use SparkPass.Crypto.MLDSA87.Poly;
with SparkPass.Crypto.MLDSA87.PolyVec; use SparkPass.Crypto.MLDSA87.PolyVec;
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.MLDSA87.Matrix is

   --  =========================================================================
   --  Matrix Type
   --  =========================================================================

   --  Matrix A: k×l polynomials (8×7 for ML-DSA-87)
   type Matrix_KxL is array (Vec_K_Index, Vec_L_Index) of Polynomial;

   --  =========================================================================
   --  Matrix Expansion
   --  =========================================================================

   --  **ExpandA**: Expand matrix A from seed ρ
   --
   --  FIPS 204 Algorithm 28: ExpandA(ρ)
   --
   --  Uses SHAKE-128(ρ || i || j) to generate each polynomial A[i,j]
   --  in NTT form via rejection sampling.
   --
   --  Input:  ρ (32 bytes)
   --  Output: Matrix A (k×l polynomials in NTT domain)
   --
   procedure ExpandA (
      Rho : in  Byte_Array;
      A   : out Matrix_KxL
   ) with
      Global => null,
      Pre    => Rho'Length = Seed_Bytes and Rho'First = 1;

   --  =========================================================================
   --  Matrix-Vector Multiplication
   --  =========================================================================

   --  **Matrix_Vec_Multiply**: Compute A * v (in NTT domain)
   --
   --  Computes w = A * v where:
   --    - A is k×l matrix
   --    - v is l-vector
   --    - w is k-vector
   --
   --  All in NTT domain (pointwise multiplication + accumulation)
   --
   procedure Matrix_Vec_Multiply (
      Result : out PolyVec_K;
      A      : in  Matrix_KxL;
      V      : in  PolyVec_L
   ) with
      Global => null;

end SparkPass.Crypto.MLDSA87.Matrix;
