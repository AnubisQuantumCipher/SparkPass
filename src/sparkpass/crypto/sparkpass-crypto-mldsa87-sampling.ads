--  ============================================================================
--  ML-DSA-87 Sampling Routines (FIPS 204)
--  ============================================================================
--
--  **Purpose**: Generate random polynomials from various distributions
--
--  **Distributions**:
--    - SampleInBall: τ non-zero coefficients (for challenge c)
--    - UniformGamma: Coefficients in [-γ, γ] (for y vector)
--    - UniformEta: Small coefficients in [-η, η] (for s1, s2)
--
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Crypto.MLDSA87.Params;  use SparkPass.Crypto.MLDSA87.Params;
with SparkPass.Crypto.MLDSA87.Poly;    use SparkPass.Crypto.MLDSA87.Poly;
with SparkPass.Crypto.MLDSA87.PolyVec; use SparkPass.Crypto.MLDSA87.PolyVec;
with SparkPass.Types; use SparkPass.Types;
with Interfaces; use Interfaces;

package SparkPass.Crypto.MLDSA87.Sampling is

   --  =========================================================================
   --  Challenge Polynomial Sampling
   --  =========================================================================

   --  **SampleInBall**: Generate challenge polynomial c
   --
   --  FIPS 204 Algorithm 44: SampleInBall(ρ, τ)
   --
   --  Creates polynomial with exactly τ non-zero coefficients ∈ {-1, +1}
   --  using SHAKE-256 to generate positions and signs.
   --
   --  For ML-DSA-87: τ = 60
   --
   procedure SampleInBall (
      Seed   : in  Byte_Array;
      Result : out Polynomial
   ) with
      Global => null,
      Pre    => Seed'Length = 32 and Seed'First = 1;

   --  =========================================================================
   --  Uniform Sampling
   --  =========================================================================

   --  **UniformGamma1**: Sample polynomial with coefficients in [-γ1, γ1]
   --
   --  FIPS 204 Algorithm 39: ExpandMask(ρ, κ)
   --
   --  Uses SHAKE-256 to generate coefficients uniformly in range.
   --  For ML-DSA-87: γ1 = 2^19 = 524288
   --
   procedure UniformGamma1 (
      Seed    : in  Byte_Array;
      Counter : in  Unsigned_16;
      Result  : out Polynomial
   ) with
      Global => null,
      Pre    => Seed'Length = 64 and Seed'First = 1;

   --  **UniformGamma1_Vec**: Sample vector with coefficients in [-γ1, γ1]
   procedure UniformGamma1_Vec_L (
      Seed   : in  Byte_Array;
      Kappa  : in  Unsigned_16;
      Result : out PolyVec_L
   ) with
      Global => null,
      Pre    => Seed'Length = 64 and Seed'First = 1;

   --  =========================================================================
   --  Small Coefficient Sampling
   --  =========================================================================

   --  **UniformEta**: Sample polynomial with small coefficients
   --
   --  FIPS 204 Algorithm 38: RejBoundedPoly(ρ)
   --
   --  Generates coefficients in [-η, η] using rejection sampling.
   --  For ML-DSA-87: η = 2
   --
   procedure UniformEta (
      Seed    : in  Byte_Array;
      Counter : in  Unsigned_16;
      Result  : out Polynomial
   ) with
      Global => null,
      Pre    => Seed'Length = 66 and Seed'First = 1;

   --  **UniformEta_Vec_L**: Sample L-vector of small polynomials
   procedure UniformEta_Vec_L (
      Seed   : in  Byte_Array;
      Offset : in  Unsigned_16;
      Result : out PolyVec_L
   ) with
      Global => null,
      Pre    => Seed'Length = 66 and Seed'First = 1;

   --  **UniformEta_Vec_K**: Sample K-vector of small polynomials
   procedure UniformEta_Vec_K (
      Seed   : in  Byte_Array;
      Offset : in  Unsigned_16;
      Result : out PolyVec_K
   ) with
      Global => null,
      Pre    => Seed'Length = 66 and Seed'First = 1;

end SparkPass.Crypto.MLDSA87.Sampling;
