--  ============================================================================
--  ML-DSA-87 Polynomial Vector Operations
--  ============================================================================
--
--  **Purpose**: Operations on vectors of polynomials
--
--  **Vectors**:
--    - PolyVec_K: k=8 polynomials (for public key, t, w, h)
--    - PolyVec_L: l=7 polynomials (for secret key, s1, y, z)
--
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Crypto.MLDSA87.Params; use SparkPass.Crypto.MLDSA87.Params;
with SparkPass.Crypto.MLDSA87.Poly;   use SparkPass.Crypto.MLDSA87.Poly;

package SparkPass.Crypto.MLDSA87.PolyVec is

   --  =========================================================================
   --  Vector Types
   --  =========================================================================

   --  Vector of K polynomials (for t, w, h)
   subtype Vec_K_Index is Natural range 0 .. K - 1;
   type PolyVec_K is array (Vec_K_Index) of Polynomial;

   --  Vector of L polynomials (for s1, y, z)
   subtype Vec_L_Index is Natural range 0 .. L - 1;
   type PolyVec_L is array (Vec_L_Index) of Polynomial;

   --  =========================================================================
   --  NTT Operations on Vectors
   --  =========================================================================

   procedure NTT_Vec_K (V : in out PolyVec_K) with
      Global => null;

   procedure NTT_Inv_Vec_K (V : in out PolyVec_K) with
      Global => null;

   procedure NTT_Vec_L (V : in out PolyVec_L) with
      Global => null;

   procedure NTT_Inv_Vec_L (V : in out PolyVec_L) with
      Global => null;

   --  =========================================================================
   --  Vector Arithmetic
   --  =========================================================================

   procedure Add_Vec_K (
      Result : out PolyVec_K;
      A      : in  PolyVec_K;
      B      : in  PolyVec_K
   ) with
      Global => null;

   procedure Sub_Vec_K (
      Result : out PolyVec_K;
      A      : in  PolyVec_K;
      B      : in  PolyVec_K
   ) with
      Global => null;

   procedure Add_Vec_L (
      Result : out PolyVec_L;
      A      : in  PolyVec_L;
      B      : in  PolyVec_L
   ) with
      Global => null;

   procedure Sub_Vec_L (
      Result : out PolyVec_L;
      A      : in  PolyVec_L;
      B      : in  PolyVec_L
   ) with
      Global => null;

   --  =========================================================================
   --  Rounding Operations on Vectors
   --  =========================================================================

   procedure Power2Round_Vec_K (
      V  : in  PolyVec_K;
      V1 : out PolyVec_K;
      V0 : out PolyVec_K
   ) with
      Global => null;

   procedure Decompose_Vec_K (
      V  : in  PolyVec_K;
      V1 : out PolyVec_K;
      V0 : out PolyVec_K
   ) with
      Global => null;

   procedure HighBits_Vec_K (
      V  : in  PolyVec_K;
      V1 : out PolyVec_K
   ) with
      Global => null;

   procedure LowBits_Vec_K (
      V  : in  PolyVec_K;
      V0 : out PolyVec_K
   ) with
      Global => null;

   --  =========================================================================
   --  Norm Operations
   --  =========================================================================

   function Infinity_Norm_Vec_L (V : PolyVec_L) return Natural with
      Global => null;

   function Infinity_Norm_Vec_K (V : PolyVec_K) return Natural with
      Global => null;

   function Check_Norm_Bound_Vec_L (
      V     : PolyVec_L;
      Bound : Natural
   ) return Boolean with
      Global => null;

   --  =========================================================================
   --  Hint Operations
   --  =========================================================================

   --  MakeHint for vectors: create hint vector h
   procedure MakeHint_Vec_K (
      H : out PolyVec_K;  -- Hint polynomial (Boolean stored as 0/1 in Coeff)
      Z : in  PolyVec_K;
      R : in  PolyVec_K;
      Ones_Count : out Natural
   ) with
      Global => null,
      Post   => Ones_Count <= K * N;

   --  UseHint for vectors: recover w1 from w and h
   procedure UseHint_Vec_K (
      Result : out PolyVec_K;
      H      : in  PolyVec_K;
      R      : in  PolyVec_K
   ) with
      Global => null;

   --  =========================================================================
   --  Utility Operations
   --  =========================================================================

   procedure Zeroize_Vec_K (V : in out PolyVec_K) with
      Global => null;

   procedure Zeroize_Vec_L (V : in out PolyVec_L) with
      Global => null;

end SparkPass.Crypto.MLDSA87.PolyVec;
