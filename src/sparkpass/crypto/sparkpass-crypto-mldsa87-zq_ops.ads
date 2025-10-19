--  ============================================================================
--  ML-DSA-87 Modular Arithmetic in Zq
--  ============================================================================
--
--  This package provides branch-free modular operations for Zq [0, Q-1].
--  All hot-path arithmetic uses unsigned types to avoid overflow.
--
--  Conversions to/from centered representation are provided for the few
--  places that need it (norms, rounding, hints).
--
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Crypto.MLDSA87.Params;

package SparkPass.Crypto.MLDSA87.ZQ_Ops is

   --  =========================================================================
   --  Modular Arithmetic (branch-free)
   --  =========================================================================

   --  Addition mod q: (a + b) mod q
   function AddQ (A, B : SparkPass.Crypto.MLDSA87.Params.Zq) return SparkPass.Crypto.MLDSA87.Params.Zq with Inline;

   --  Subtraction mod q: (a - b) mod q
   function SubQ (A, B : SparkPass.Crypto.MLDSA87.Params.Zq) return SparkPass.Crypto.MLDSA87.Params.Zq with Inline;

   --  =========================================================================
   --  Montgomery Reduction
   --  =========================================================================

   --  Montgomery reduce: Given T ∈ [0, q*2^32), return T / 2^32 mod q
   --  FIPS 204 Algorithm 35
   function MontReduce (T : SparkPass.Crypto.MLDSA87.Params.U64) return SparkPass.Crypto.MLDSA87.Params.Zq with Inline;

   --  Montgomery multiply: (A * B) / 2^32 mod q
   function MontMul (A, B : SparkPass.Crypto.MLDSA87.Params.Zq) return SparkPass.Crypto.MLDSA87.Params.Zq with Inline;

   --  =========================================================================
   --  Montgomery Domain Conversions
   --  =========================================================================

   --  Encode to Montgomery domain: A → A * R mod q
   function MontEncode (A : SparkPass.Crypto.MLDSA87.Params.Zq) return SparkPass.Crypto.MLDSA87.Params.Zq with Inline;

   --  Decode from Montgomery domain: A → A * R^-1 mod q
   function MontDecode (A : SparkPass.Crypto.MLDSA87.Params.Zq) return SparkPass.Crypto.MLDSA87.Params.Zq with Inline;

   --  =========================================================================
   --  Polynomial-Wide Montgomery Conversions
   --  =========================================================================

   --  Encode entire polynomial to Montgomery domain
   procedure Encode_Poly (
      Out_Mont  : out SparkPass.Crypto.MLDSA87.Params.Poly_Zq;
      In_Plain  : in  SparkPass.Crypto.MLDSA87.Params.Poly_Zq
   ) with
      Global => null;

   --  Decode entire polynomial from Montgomery domain
   procedure Decode_Poly (
      Out_Plain : out SparkPass.Crypto.MLDSA87.Params.Poly_Zq;
      In_Mont   : in  SparkPass.Crypto.MLDSA87.Params.Poly_Zq
   ) with
      Global => null;

   --  =========================================================================
   --  Domain Conversions (use sparingly)
   --  =========================================================================

   --  Convert centered [-((Q-1)/2), ((Q-1)/2)] to Zq [0, Q-1]
   function To_Zq (X : SparkPass.Crypto.MLDSA87.Params.Coeff_Centered) return SparkPass.Crypto.MLDSA87.Params.Zq with Inline;

   --  Convert Zq [0, Q-1] to centered [-((Q-1)/2), ((Q-1)/2)]
   function To_Centered (X : SparkPass.Crypto.MLDSA87.Params.Zq) return SparkPass.Crypto.MLDSA87.Params.Coeff_Centered with Inline;

end SparkPass.Crypto.MLDSA87.ZQ_Ops;
