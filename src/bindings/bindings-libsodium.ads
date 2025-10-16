pragma SPARK_Mode (On);
with System;
with Interfaces.C;

package Bindings.Libsodium is
   pragma Preelaborate;

   function Sodium_Init return Interfaces.C.int;
   pragma Import (C, Sodium_Init, "sodium_init");

   procedure Randombytes_Buf (Buffer : System.Address; Size : Interfaces.C.size_t);
   pragma Import (C, Randombytes_Buf, "randombytes_buf");

   procedure Sodium_Memzero (Buffer : System.Address; Size : Interfaces.C.size_t);
   pragma Import (C, Sodium_Memzero, "sodium_memzero");

   --  Constant-time memory comparison (returns 0 if equal, -1 if different)
   function Sodium_Memcmp
     (B1   : System.Address;
      B2   : System.Address;
      Size : Interfaces.C.size_t) return Interfaces.C.int;
   pragma Import (C, Sodium_Memcmp, "sodium_memcmp");

   function Crypto_Hash_Sha512
     (Output_Buffer : System.Address;
      Input_Buffer  : System.Address;
      Input_Length  : Interfaces.C.unsigned_long_long) return Interfaces.C.int;
   pragma Import (C, Crypto_Hash_Sha512, "crypto_hash_sha512");

   --  HMAC-SHA512 for message authentication
   function Crypto_Auth_Hmacsha512
     (Output_Buffer : System.Address;
      Input_Buffer  : System.Address;
      Input_Length  : Interfaces.C.unsigned_long_long;
      Key_Buffer    : System.Address) return Interfaces.C.int;
   pragma Import (C, Crypto_Auth_Hmacsha512, "crypto_auth_hmacsha512");

   function Crypto_Pwhash
     (Output_Buffer : System.Address;
      Output_Length : Interfaces.C.size_t;
      Password_Buffer : System.Address;
      Password_Length : Interfaces.C.size_t;
      Salt_Buffer     : System.Address;
      Opslimit        : Interfaces.C.unsigned_long_long;
      Memlimit        : Interfaces.C.size_t;
      Algorithm       : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Crypto_Pwhash, "crypto_pwhash");

   Crypto_Pwhash_Alg_Argon2id13 : constant Interfaces.C.int := 2;

   function Crypto_Aead_Aes256gcm_Is_Available return Interfaces.C.int;
   pragma Import (C, Crypto_Aead_Aes256gcm_Is_Available,
                  "crypto_aead_aes256gcm_is_available");

   function Crypto_Aead_Aes256gcm_Encrypt_Detached
     (Ciphertext_Buffer : System.Address;
      Tag_Buffer        : System.Address;
      Tag_Length        : Interfaces.C.size_t;
      Message_Buffer    : System.Address;
      Message_Length    : Interfaces.C.size_t;
      AAD_Buffer        : System.Address;
      AAD_Length        : Interfaces.C.size_t;
      Secret_Nonce      : System.Address;
      Nonce_Buffer      : System.Address;
      Key_Buffer        : System.Address) return Interfaces.C.int;
   pragma Import (C, Crypto_Aead_Aes256gcm_Encrypt_Detached,
                  "crypto_aead_aes256gcm_encrypt_detached");

   function Crypto_Aead_Aes256gcm_Decrypt_Detached
     (Message_Buffer    : System.Address;
      Secret_Nonce      : System.Address;
      Ciphertext_Buffer : System.Address;
      Ciphertext_Length : Interfaces.C.size_t;
      Tag_Buffer        : System.Address;
      Nonce_Buffer      : System.Address;
      Key_Buffer        : System.Address;
      AAD_Buffer        : System.Address;
      AAD_Length        : Interfaces.C.size_t) return Interfaces.C.int;
   pragma Import (C, Crypto_Aead_Aes256gcm_Decrypt_Detached,
                  "crypto_aead_aes256gcm_decrypt_detached");

   function Crypto_Aead_Chacha20poly1305_Ietf_Encrypt_Detached
     (Ciphertext_Buffer : System.Address;
      Tag_Buffer        : System.Address;
      Tag_Length        : Interfaces.C.size_t;
      Message_Buffer    : System.Address;
      Message_Length    : Interfaces.C.size_t;
      AAD_Buffer        : System.Address;
      AAD_Length        : Interfaces.C.size_t;
      Secret_Nonce      : System.Address;
      Nonce_Buffer      : System.Address;
      Key_Buffer        : System.Address) return Interfaces.C.int;
   pragma Import
     (C,
      Crypto_Aead_Chacha20poly1305_Ietf_Encrypt_Detached,
      "crypto_aead_chacha20poly1305_ietf_encrypt_detached");

   function Crypto_Aead_Chacha20poly1305_Ietf_Decrypt_Detached
     (Message_Buffer    : System.Address;
      Secret_Nonce      : System.Address;
      Ciphertext_Buffer : System.Address;
      Ciphertext_Length : Interfaces.C.size_t;
      Tag_Buffer        : System.Address;
      Nonce_Buffer      : System.Address;
      Key_Buffer        : System.Address;
      AAD_Buffer        : System.Address;
      AAD_Length        : Interfaces.C.size_t) return Interfaces.C.int;
   pragma Import
     (C,
      Crypto_Aead_Chacha20poly1305_Ietf_Decrypt_Detached,
      "crypto_aead_chacha20poly1305_ietf_decrypt_detached");

   --  Non-detached AES-256-GCM API (combines ciphertext + tag)
   --  These do NOT require beforenm key schedule precomputation
   function Crypto_Aead_Aes256gcm_Encrypt
     (Ciphertext_Buffer : System.Address;
      Ciphertext_Length : access Interfaces.C.unsigned_long_long;
      Message_Buffer    : System.Address;
      Message_Length    : Interfaces.C.unsigned_long_long;
      AAD_Buffer        : System.Address;
      AAD_Length        : Interfaces.C.unsigned_long_long;
      Secret_Nonce      : System.Address;
      Nonce_Buffer      : System.Address;
      Key_Buffer        : System.Address) return Interfaces.C.int;
   pragma Import (C, Crypto_Aead_Aes256gcm_Encrypt,
                  "crypto_aead_aes256gcm_encrypt");

   function Crypto_Aead_Aes256gcm_Decrypt
     (Message_Buffer    : System.Address;
      Message_Length    : access Interfaces.C.unsigned_long_long;
      Secret_Nonce      : System.Address;
      Ciphertext_Buffer : System.Address;
      Ciphertext_Length : Interfaces.C.unsigned_long_long;
      AAD_Buffer        : System.Address;
      AAD_Length        : Interfaces.C.unsigned_long_long;
      Nonce_Buffer      : System.Address;
      Key_Buffer        : System.Address) return Interfaces.C.int;
   pragma Import (C, Crypto_Aead_Aes256gcm_Decrypt,
                  "crypto_aead_aes256gcm_decrypt");

   --  AES-256-GCM tag size constant
   Crypto_Aead_Aes256gcm_Abytes : constant := 16;

end Bindings.Libsodium;
