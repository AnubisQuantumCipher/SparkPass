--  ============================================================================
--  ML-DSA-87 Parameters (NIST FIPS 204 Table 1)
--  ============================================================================
--
--  **Parameter Set**: ML-DSA-87 (Dilithium Level 5)
--  **Security Level**: NIST Level 5 (beyond AES-256)
--
--  All constants taken directly from FIPS 204 Table 1.
--
--  ============================================================================

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package SparkPass.Crypto.MLDSA87.Params is

   --  =========================================================================
   --  Ring Parameters (FIPS 204 Section 3)
   --  =========================================================================

   --  Polynomial ring: Zq[X]/(X^256 + 1)
   N : constant := 256;  -- Polynomial degree
   Q : constant := 8380417;  -- Prime modulus

   --  Shorthand types
   subtype U32 is Unsigned_32;
   subtype U64 is Unsigned_64;
   subtype I32 is Integer_32;

   --  Non-centered representation mod q (primary working domain)
   subtype Zq is U32 range 0 .. Q - 1;

   --  Centered representation (only for norms, rounding, hints)
   subtype Coeff_Centered is I32 range -I32 ((Q - 1) / 2) .. I32 ((Q - 1) / 2);

   --  Index types
   subtype Poly_Index is Natural range 0 .. N - 1;

   --  =========================================================================
   --  ML-DSA-87 Parameters (FIPS 204 Table 1)
   --  =========================================================================

   --  Dimension parameters
   K : constant := 8;  -- Public key vector dimension (rows of A)
   L : constant := 7;  -- Secret key vector dimension (columns of A)

   --  Rounding parameters
   D     : constant := 13;  -- Dropped bits from t
   Gamma1 : constant := 524288;  -- 2^19, for decomposition
   Gamma2 : constant := 261888;  -- (q-1)/32, for rounding

   --  Rejection parameters
   Tau : constant := 60;  -- Challenge polynomial weight
   Eta : constant := 2;   -- Small coefficient bound for s1, s2
   Beta : constant := 120;  -- Tau * Eta, rejection bound

   --  Hint polynomial parameters
   Omega : constant := 75;  -- Maximum weight of hint polynomial h

   --  =========================================================================
   --  Polynomial Types
   --  =========================================================================

   --  Primary polynomial type (non-centered Zq coefficients)
   type Poly_Zq is array (Poly_Index) of Zq;

   --  Centered polynomial (for edges: norms, Power2Round, hints)
   type Poly_C is array (Poly_Index) of Coeff_Centered;

   --  Range for small coefficients (centered)
   subtype Small_Coeff is Coeff_Centered range -Eta .. Eta;

   --  Range for challenge coefficients (±1)
   subtype Challenge_Coeff is Coeff_Centered range -1 .. 1;

   --  =========================================================================
   --  Packing Sizes (FIPS 204 Table 2)
   --  =========================================================================

   --  Bit lengths for packing
   Poly_T1_Bytes  : constant := 320;   -- 10 bits per coeff * 256 / 8
   Poly_T0_Bytes  : constant := 416;   -- 13 bits per coeff * 256 / 8
   Poly_S_Bytes   : constant := 96;    -- 3 bits per coeff * 256 / 8 (for η=2)
   Poly_Z_Bytes   : constant := 640;   -- 20 bits per coeff * 256 / 8
   Poly_W1_Bytes  : constant := 128;   -- 4 bits per coeff * 256 / 8

   --  Key component sizes
   Seed_Bytes     : constant := 32;    -- ρ, K, tr seeds
   T1_Bytes       : constant := K * Poly_T1_Bytes;  -- 2560 bytes
   T0_Bytes       : constant := K * Poly_T0_Bytes;  -- 3328 bytes
   S1_Bytes       : constant := L * Poly_S_Bytes;   -- 672 bytes
   S2_Bytes       : constant := K * Poly_S_Bytes;   -- 768 bytes

   --  Signature component sizes
   C_Tilde_Bytes  : constant := 32;    -- Challenge hash
   Z_Bytes        : constant := L * Poly_Z_Bytes;  -- 4480 bytes
   W1_Bytes       : constant := K * Poly_W1_Bytes; -- 1024 bytes
   H_Bytes        : constant := Omega + K;  -- 83 bytes (includes indices)

   --  =========================================================================
   --  Hash Function Parameters
   --  =========================================================================

   --  SHAKE instances used (FIPS 204 Section 3.5)
   --  - SHAKE-128: For matrix A expansion
   --  - SHAKE-256: For key generation, signing

   --  =========================================================================
   --  NTT and Montgomery Parameters (FIPS 204 Section 3.7)
   --  =========================================================================

   --  Root of unity: ζ = 1753 (primitive 512-th root of unity mod q)
   Zeta_Root : constant := 1753;

   --  Montgomery domain: R = 2^32 mod q
   Mont_R  : constant U32 := 4193792;    -- 2^32 mod q
   Mont_R2 : constant U32 := 2365951;    -- R^2 mod q (for conversion to Montgomery)

   --  Montgomery reduction constant: QInv = -q^{-1} mod 2^64
   --  Used in Montgomery reduction: m = (a * QInv) mod 2^32
   --  We store as U64 but only use lower 32 bits in MontReduce
   QInv : constant U64 := 16#E7F5BF9FFC7FDFFF#;  -- 16,714,476,285,912,408,063

end SparkPass.Crypto.MLDSA87.Params;
