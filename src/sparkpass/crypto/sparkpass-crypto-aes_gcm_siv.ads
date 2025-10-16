pragma SPARK_Mode (On);
with SparkPass.Config;
with SparkPass.Types; use SparkPass.Types;

--  AES-256-GCM-SIV (Synthetic IV) authenticated encryption.
--  GCM-SIV provides nonce-misuse resistance: reusing a nonce with the same key
--  leaks only whether the plaintext is identical, not the plaintext itself.
--
--  This implementation uses OpenSSL 3.2+ EVP_aes_256_gcm_siv.
package SparkPass.Crypto.AES_GCM_SIV is
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
       Post   => Plaintext'Length = Ciphertext'Length;

end SparkPass.Crypto.AES_GCM_SIV;
