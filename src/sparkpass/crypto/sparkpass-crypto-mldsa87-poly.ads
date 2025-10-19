--  ============================================================================
--  ML-DSA-87 Polynomial Arithmetic (FIPS 204)
--  ============================================================================
--
--  **Ring**: Zq[X]/(X^256 + 1) where q = 8380417
--
--  **Operations**:
--    - NTT/NTT^-1: Number Theoretic Transform
--    - Montgomery reduction
--    - Pointwise multiplication
--    - Rounding: Power2Round, Decompose, MakeHint, UseHint
--    - Norms: Infinity norm check
--
--  **SPARK Properties**:
--    - Memory-safe (proven bounds)
--    - Constant-time where required
--
--  ============================================================================

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with SparkPass.Crypto.MLDSA87.Params; use SparkPass.Crypto.MLDSA87.Params;
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.MLDSA87.Poly is

   --  =========================================================================
   --  Polynomial Type (imported from Params)
   --  =========================================================================

   --  Primary polynomial type: Poly_Zq (non-centered coefficients)
   --  Use Poly_C only at edges for centered operations
   subtype Polynomial is Poly_Zq;

   --  =========================================================================
   --  Note: Modular arithmetic (AddQ, SubQ, MontMul) is in ZQ package
   --  =========================================================================

   --  =========================================================================
   --  NTT Operations (FIPS 204 Section 3.7)
   --  =========================================================================

   --  **NTT**: Forward Number Theoretic Transform
   --  Transforms polynomial from coefficient to NTT representation
   procedure NTT (P : in out Polynomial) with
      Global => null;

   --  **NTT_Inv**: Inverse Number Theoretic Transform
   --  Transforms polynomial from NTT to coefficient representation
   procedure NTT_Inv (P : in out Polynomial) with
      Global => null;

   --  =========================================================================
   --  Arithmetic Operations
   --  =========================================================================

   --  **Add**: Polynomial addition (coefficient-wise mod q)
   procedure Add (
      Result : out Polynomial;
      A      : in  Polynomial;
      B      : in  Polynomial
   ) with
      Global => null;

   --  **Sub**: Polynomial subtraction (coefficient-wise mod q)
   procedure Sub (
      Result : out Polynomial;
      A      : in  Polynomial;
      B      : in  Polynomial
   ) with
      Global => null;

   --  **Pointwise_Montgomery**: Pointwise multiply in NTT domain
   --  (a_i * b_i) * R^-1 mod q for all i
   procedure Pointwise_Montgomery (
      Result : out Polynomial;
      A      : in  Polynomial;
      B      : in  Polynomial
   ) with
      Global => null;

   --  =========================================================================
   --  Rounding Operations (FIPS 204 Section 3.6)
   --  =========================================================================
   --  These take Zq inputs/outputs but may convert to centered internally

   --  **Power2Round**: Split r into (r1, r0) where r = r1*2^d + r0
   --  FIPS 204 Algorithm 27
   procedure Power2Round (
      R  : in  Zq;
      R1 : out Zq;
      R0 : out Zq
   ) with
      Global => null;

   --  **Decompose**: Split r into (r1, r0) based on γ2
   --  FIPS 204 Algorithm 30
   procedure Decompose (
      R  : in  Zq;
      R1 : out Zq;
      R0 : out Zq
   ) with
      Global => null;

   --  **HighBits**: Extract high bits (r1 from Decompose)
   --  FIPS 204 Algorithm 29
   function HighBits (R : Zq) return Zq with
      Global => null;

   --  **LowBits**: Extract low bits (r0 from Decompose)
   --  FIPS 204 Algorithm 31
   function LowBits (R : Zq) return Zq with
      Global => null;

   --  **MakeHint**: Create hint bit for rounding
   --  FIPS 204 Algorithm 33
   function MakeHint (
      Z  : Zq;
      R  : Zq
   ) return Boolean with
      Global => null;

   --  **UseHint**: Use hint bit to recover high bits
   --  FIPS 204 Algorithm 34
   function UseHint (
      H : Boolean;
      R : Zq
   ) return Zq with
      Global => null;

   --  =========================================================================
   --  Norm Operations
   --  =========================================================================

   --  **Infinity_Norm**: Maximum absolute value of coefficients
   function Infinity_Norm (P : Polynomial) return Natural with
      Global => null;

   --  **Check_Norm_Bound**: Check if ||p||∞ < bound
   function Check_Norm_Bound (
      P     : Polynomial;
      Bound : Natural
   ) return Boolean with
      Global => null;

   --  =========================================================================
   --  Utility Operations
   --  =========================================================================

   --  **Zeroize**: Securely clear polynomial
   procedure Zeroize (P : in out Polynomial) with
      Global => null,
      Post   => (for all I in Poly_Index => P (I) = 0);

end SparkPass.Crypto.MLDSA87.Poly;
