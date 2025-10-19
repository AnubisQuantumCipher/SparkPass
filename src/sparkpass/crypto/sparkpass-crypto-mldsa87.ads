--  ============================================================================
--  SparkPass ML-DSA-87 (Pure SPARK) - NIST FIPS 204
--  ============================================================================
--
--  **Purpose**: Post-quantum digital signatures (Dilithium Level 5)
--
--  **Specification**: NIST FIPS 204 - Module-Lattice-Based Digital Signature
--                     Standard (ML-DSA)
--                     https://csrc.nist.gov/pubs/fips/204/final
--
--  **Parameter Set**: ML-DSA-87 (k=8, l=7, highest security level)
--
--  **Security Level**: NIST Level 5 (beyond AES-256 classical security)
--
--  **Key Sizes** (FIPS 204 Table 1):
--    - Public Key:  2592 bytes (ρ || t1)
--    - Secret Key:  4896 bytes (ρ || K || tr || s1 || s2 || t0)
--    - Signature:   4627 bytes (c̃ || z || h)
--
--  **Operations**:
--    - KeyGen: Generate ML-DSA-87 key pair
--    - Sign:   Create deterministic signature
--    - Verify: Verify signature
--
--  **Implementation**:
--    - Pure SPARK (zero FFI)
--    - Constant-time operations where required
--    - Memory-safe (SPARK-proven)
--
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.MLDSA87 is

   --  =========================================================================
   --  ML-DSA-87 Constants (FIPS 204 Table 1)
   --  =========================================================================

   --  Key and signature sizes
   Public_Key_Bytes  : constant := 2592;  -- ρ (32) + t1 (1280*2)
   Secret_Key_Bytes  : constant := 4896;  -- Full secret key
   Signature_Bytes   : constant := 4627;  -- c̃ (32) + z (640*7) + h (75+ω)

   --  Type definitions
   subtype Public_Key_Array  is Byte_Array (1 .. Public_Key_Bytes);
   subtype Secret_Key_Array  is Byte_Array (1 .. Secret_Key_Bytes);
   subtype Signature_Array   is Byte_Array (1 .. Signature_Bytes);

   --  =========================================================================
   --  ML-DSA-87 Operations
   --  =========================================================================

   --  **KeyGen**: Generate ML-DSA-87 key pair
   --
   --  FIPS 204 Algorithm 1: ML-DSA.KeyGen
   --
   --  Generates:
   --    - Public key (pk): 2592 bytes
   --    - Secret key (sk): 4896 bytes
   --
   --  Security: Uses SHAKE-256 for randomness expansion
   --
   procedure KeyGen (
      Public_Key  : out Public_Key_Array;
      Secret_Key  : out Secret_Key_Array
   ) with
      Global => null;

   --  **Sign_Deterministic**: Create deterministic signature
   --
   --  FIPS 204 Algorithm 2: ML-DSA.Sign (deterministic variant)
   --
   --  Inputs:
   --    - Secret key (4896 bytes)
   --    - Message (arbitrary length)
   --
   --  Output:
   --    - Signature (4627 bytes)
   --
   --  Security: Deterministic (no randomness required)
   --
   procedure Sign_Deterministic (
      Secret_Key  : in  Secret_Key_Array;
      Message     : in  Byte_Array;
      Signature   : out Signature_Array
   ) with
      Global => null,
      Pre    => Message'Length > 0 and Message'Length <= 65536;

   --  **Verify**: Verify ML-DSA-87 signature
   --
   --  FIPS 204 Algorithm 3: ML-DSA.Verify
   --
   --  Inputs:
   --    - Public key (2592 bytes)
   --    - Message (arbitrary length)
   --    - Signature (4627 bytes)
   --
   --  Output:
   --    - Valid: True if signature is valid, False otherwise
   --
   --  Security: Constant-time signature comparison
   --
   function Verify (
      Public_Key : in Public_Key_Array;
      Message    : in Byte_Array;
      Signature  : in Signature_Array
   ) return Boolean with
      Global => null,
      Pre    => Message'Length > 0 and Message'Length <= 65536;

end SparkPass.Crypto.MLDSA87;
