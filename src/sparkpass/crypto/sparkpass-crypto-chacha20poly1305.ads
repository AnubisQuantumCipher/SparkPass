pragma SPARK_Mode (On);
with Interfaces; use Interfaces;
with SparkPass.Config;
with SparkPass.Types; use SparkPass.Types;

--  ChaCha20-Poly1305 AEAD (RFC 8439) authenticated encryption.
--  This implementation uses SPARKNaCl's verified ChaCha20-Poly1305 AEAD,
--  providing a fully SPARK-verified alternative to OpenSSL AES-GCM-SIV.
--
--  ChaCha20-Poly1305 provides authenticated encryption with:
--  - ChaCha20 stream cipher (nonce-misuse resistant)
--  - Poly1305 MAC (128-bit authentication)
--  - Additional Authenticated Data (AAD) support
--  - 100% SPARK verification (no FFI)
--
--  References:
--  - RFC 8439: ChaCha20-Poly1305 for IETF Protocols
--  - SPARKNaCl 4.0.1: Rod Chapman's verified crypto library
package SparkPass.Crypto.ChaCha20Poly1305 is
   procedure Seal
     (Key        : in  Key_Array;
      Nonce      : in  Nonce_Array;
      Plaintext  : in  Byte_Array;
      AAD        : in  Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out Tag_Array)
     with
       Global => null,
       Pre    => Ciphertext'Length = Plaintext'Length and then
                 Plaintext'Length <= SparkPass.Config.Max_Data_Length and then
                 AAD'Length <= 256,
       Post   => Ciphertext'Length = Plaintext'Length;

   procedure Open
     (Key        : in  Key_Array;
      Nonce      : in  Nonce_Array;
      Ciphertext : in  Byte_Array;
      AAD        : in  Byte_Array;
      Tag        : in  Tag_Array;
      Plaintext  : out Byte_Array;
      Success    : out Boolean)
     with
       Global => null,
       Pre    => Plaintext'Length = Ciphertext'Length and then
                 Ciphertext'Length <= SparkPass.Config.Max_Data_Length and then
                 AAD'Length <= 256,
       Post   => Plaintext'Length = Ciphertext'Length and then
                 (if not Success then
                    (for all I in Plaintext'Range => Plaintext(I) = 0));

end SparkPass.Crypto.ChaCha20Poly1305;
