--  ========================================================================
--  SparkPass HMAC (Hash-based Message Authentication Code) - Pure SPARK
--  ========================================================================
--
--  **Purpose**: HMAC-SHA512 for HKDF and other authenticated operations
--               Pure SPARK implementation using Keccak SHA3-512
--
--  **Specification**: RFC 2104 (HMAC), FIPS 198-1
--
--  **Algorithm**: HMAC(K, m) = H((K ⊕ opad) || H((K ⊕ ipad) || m))
--    where:
--      H = SHA3-512 (via Keccak)
--      ipad = 0x36 repeated (inner padding)
--      opad = 0x5C repeated (outer padding)
--      K = key (padded/hashed to block size if needed)
--
--  **Security**: 512-bit output, collision resistance, PRF security
--
--  ========================================================================

pragma SPARK_Mode (On);

with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.HMAC is

   --  SHA3-512 produces 64-byte output
   subtype HMAC_Output is Byte_Array(1 .. 64);

   --  =====================================================================
   --  HMAC-SHA512 Function
   --  =====================================================================
   --
   --  **Purpose**: Compute HMAC-SHA512 for message authentication
   --
   --  **Algorithm** (RFC 2104):
   --    1. If key > block_size: key = H(key)
   --    2. If key < block_size: pad with zeros
   --    3. Compute: H((key ⊕ opad) || H((key ⊕ ipad) || message))
   --
   --  **Parameters**:
   --    - Key: Authentication key (any length)
   --    - Message: Data to authenticate (any length)
   --    - Output: 64-byte HMAC tag
   --
   --  **Security Properties**:
   --    - Collision resistance (inherited from SHA3-512)
   --    - PRF security (pseudorandom function)
   --    - Key recovery resistance
   --
   --  **Usage**:
   --      Tag : HMAC_Output;
   --      HMAC_SHA512(Key => Master_Key, Message => Data, Output => Tag);
   --
   --  =====================================================================

   procedure HMAC_SHA512 (
      Key     : in Byte_Array;
      Message : in Byte_Array;
      Output  : out HMAC_Output
   ) with
      Global => null,
      Pre    => Key'Length > 0 and
                Key'Length <= 65536 and
                Message'Length <= 65536 and
                Key'First = 1 and
                Message'First = 1,
      Post   => Output'Length = 64 and
                Output'First = 1;
   --  **Purpose**: Compute HMAC-SHA512(Key, Message)
   --  **Input**:
   --    - Key: Authentication key (1..65536 bytes)
   --    - Message: Data to authenticate (0..65536 bytes)
   --  **Output**:
   --    - Output: 64-byte HMAC-SHA512 tag
   --  **SPARK Contract**: Memory-safe, no global state

   --  =====================================================================
   --  HMAC-SHA256 Function (using SHA3-256)
   --  =====================================================================
   --
   --  **Purpose**: Compute HMAC-SHA256 for 32-byte tags
   --  **Same algorithm as HMAC-SHA512 but using SHA3-256**
   --  =====================================================================

   subtype HMAC256_Output is Byte_Array(1 .. 32);

   procedure HMAC_SHA256 (
      Key     : in Byte_Array;
      Message : in Byte_Array;
      Output  : out HMAC256_Output
   ) with
      Global => null,
      Pre    => Key'Length > 0 and
                Key'Length <= 65536 and
                Message'Length <= 65536 and
                Key'First = 1 and
                Message'First = 1,
      Post   => Output'Length = 32 and
                Output'First = 1;
   --  **Purpose**: Compute HMAC-SHA256(Key, Message)
   --  **Same as HMAC_SHA512 but using SHA3-256 (32-byte output)**

end SparkPass.Crypto.HMAC;
