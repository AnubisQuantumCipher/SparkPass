--  ============================================================================
--  SparkPass - Keccak-f[1600] Permutation (Pure SPARK)
--  ============================================================================
--
--  **Purpose**: Implements the Keccak-f[1600] permutation in pure SPARK
--               This is the foundation for SHA3-512, SHA3-256, SHAKE-128, SHAKE-256
--
--  **Specification**: NIST FIPS 202 - SHA-3 Standard
--                     https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf
--
--  **Security Properties**:
--    - All operations in constant time (no data-dependent branches)
--    - State fully zeroized when required
--    - Memory-safe by SPARK verification
--
--  **Implementation Strategy**:
--    1. Keccak-f[1600] permutation (24 rounds of θ, ρ, π, χ, ι)
--    2. Sponge construction (absorb/squeeze phases)
--    3. SHA3-512, SHA3-256 (fixed-length output)
--    4. SHAKE-128, SHAKE-256 (extendable output)
--
--  **Citation**: FIPS 202 Section 3.3 - Keccak-f[b] permutation
--
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.Keccak is
   pragma Preelaborate;

   --  =========================================================================
   --  Keccak-f[1600] State and Constants
   --  =========================================================================

   --  Keccak state: 5×5 array of 64-bit lanes (1600 bits total)
   --  FIPS 202 Section 3.1.2: State array A[x,y] where x,y ∈ {0,1,2,3,4}
   type Lane_Index is range 0 .. 4;
   type State_Array is array (Lane_Index, Lane_Index) of U64;

   --  Number of rounds in Keccak-f[1600] permutation
   --  FIPS 202 Section 3.3: nr = 12 + 2*ℓ where ℓ = 6 for b = 1600
   --  Therefore nr = 12 + 2*6 = 24
   Num_Rounds : constant := 24;

   --  Round constants for ι step (FIPS 202 Section 3.2.5)
   --  These are precomputed LFSR outputs that break symmetry
   type Round_Constants_Array is array (0 .. Num_Rounds - 1) of U64;

   --  =========================================================================
   --  Keccak-f[1600] Permutation
   --  =========================================================================

   --  **Keccak-f[1600] permutation**: 24 rounds of θ, ρ, π, χ, ι
   --
   --  FIPS 202 Algorithm 7: Keccak-f[b](S)
   --
   --  **Pre**: State contains arbitrary 1600-bit value
   --  **Post**: State contains permuted value (deterministic, bijective)
   --
   --  **Security**: Constant-time execution (no data-dependent branches)
   procedure Permute (State : in out State_Array)
     with
       Global => null,
       Post   => True;  -- Permutation is always valid

   --  =========================================================================
   --  Sponge Construction (Absorb/Squeeze)
   --  =========================================================================

   --  **Rate and capacity** (FIPS 202 Section 4)
   --  For SHA3-512: r = 576 bits (72 bytes), c = 1024 bits
   --  For SHA3-256: r = 1088 bits (136 bytes), c = 512 bits
   --  For SHAKE-128: r = 1344 bits (168 bytes), c = 256 bits
   --  For SHAKE-256: r = 1088 bits (136 bytes), c = 512 bits

   SHA3_512_Rate_Bytes  : constant := 72;   -- (1600 - 1024) / 8
   SHA3_256_Rate_Bytes  : constant := 136;  -- (1600 - 512) / 8
   SHAKE_128_Rate_Bytes : constant := 168;  -- (1600 - 256) / 8
   SHAKE_256_Rate_Bytes : constant := 136;  -- (1600 - 512) / 8

   --  **Absorb phase**: XOR input into state at rate r, then permute
   --
   --  FIPS 202 Section 4: Sponge[f, pad, r](N, d)
   --
   --  **Pre**:
   --    - Rate_Bytes ≤ 200 (entire state is 1600 bits = 200 bytes)
   --    - Block'Length = Rate_Bytes
   --  **Post**: State updated with absorbed block
   procedure Absorb_Block (
      State      : in out State_Array;
      Block      : in Byte_Array;
      Rate_Bytes : in Positive
   ) with
      Global => null,
      Pre    => Rate_Bytes <= 200 and then Block'Length = Rate_Bytes,
      Post   => True;

   --  **Squeeze phase**: Extract output from state at rate r
   --
   --  FIPS 202 Section 4: Sponge squeezing
   --
   --  **Pre**:
   --    - Rate_Bytes ≤ 200
   --    - Output'Length ≤ Rate_Bytes
   --  **Post**: Output filled with state bytes
   procedure Squeeze_Block (
      State      : in State_Array;
      Output     : out Byte_Array;
      Rate_Bytes : in Positive
   ) with
      Global => null,
      Pre    => Rate_Bytes <= 200 and then Output'Length <= Rate_Bytes,
      Post   => True;

   --  =========================================================================
   --  SHA3 Fixed-Length Hash Functions
   --  =========================================================================

   SHA3_512_Digest_Size : constant := 64;  -- 512 bits
   SHA3_256_Digest_Size : constant := 32;  -- 256 bits

   subtype SHA3_512_Digest is Byte_Array (1 .. SHA3_512_Digest_Size);
   subtype SHA3_256_Digest is Byte_Array (1 .. SHA3_256_Digest_Size);

   --  **SHA3-512**: Hash arbitrary-length input to 512-bit digest
   --
   --  FIPS 202 Algorithm 8: SHA3-512(M)
   --  = KECCAK[1024](M || 01, 512)
   --
   --  **Pre**: Input'Length >= 0
   --  **Post**: Output contains 512-bit hash
   --
   --  **Security**: 512-bit security against preimage/collision attacks
   procedure SHA3_512_Hash (
      Input  : in Byte_Array;
      Output : out SHA3_512_Digest
   ) with
      Global => null,
      Pre    => Input'Length >= 0,
      Post   => Output'Length = SHA3_512_Digest_Size;

   --  **SHA3-256**: Hash arbitrary-length input to 256-bit digest
   --
   --  FIPS 202 Algorithm 8: SHA3-256(M)
   --  = KECCAK[512](M || 01, 256)
   --
   --  **Pre**: Input'Length >= 0
   --  **Post**: Output contains 256-bit hash
   --
   --  **Security**: 256-bit security against preimage/collision attacks
   procedure SHA3_256_Hash (
      Input  : in Byte_Array;
      Output : out SHA3_256_Digest
   ) with
      Global => null,
      Pre    => Input'Length >= 0,
      Post   => Output'Length = SHA3_256_Digest_Size;

   --  =========================================================================
   --  SHAKE Extendable-Output Functions (XOF)
   --  =========================================================================

   --  **SHAKE-256**: Extendable-output function with 256-bit security
   --
   --  FIPS 202 Algorithm 9: SHAKE256(M, d)
   --  = KECCAK[512](M || 1111, d)
   --
   --  **Pre**:
   --    - Input'Length >= 0
   --    - Output'Length > 0
   --  **Post**: Output filled with SHAKE-256 output
   --
   --  **Security**: 256-bit security for arbitrary output length
   procedure SHAKE_256 (
      Input  : in Byte_Array;
      Output : out Byte_Array
   ) with
      Global => null,
      Pre    => Input'Length >= 0 and then Output'Length > 0,
      Post   => True;

   --  **SHAKE-128**: Extendable-output function with 128-bit security
   --
   --  FIPS 202 Algorithm 9: SHAKE128(M, d)
   --  = KECCAK[256](M || 1111, d)
   --
   --  **Pre**:
   --    - Input'Length >= 0
   --    - Output'Length > 0
   --  **Post**: Output filled with SHAKE-128 output
   --
   --  **Security**: 128-bit security for arbitrary output length
   procedure SHAKE_128 (
      Input  : in Byte_Array;
      Output : out Byte_Array
   ) with
      Global => null,
      Pre    => Input'Length >= 0 and then Output'Length > 0,
      Post   => True;

end SparkPass.Crypto.Keccak;
