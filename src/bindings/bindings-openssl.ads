pragma SPARK_Mode (Off);
with System;
with Interfaces.C;

package Bindings.OpenSSL is
   pragma Preelaborate;

   --  OpenSSL EVP Cipher context (opaque)
   type EVP_CIPHER_CTX is null record;
   type EVP_CIPHER_CTX_Access is access all EVP_CIPHER_CTX;
   pragma Convention (C, EVP_CIPHER_CTX_Access);

   type EVP_CIPHER is null record;
   type EVP_CIPHER_Access is access constant EVP_CIPHER;
   pragma Convention (C, EVP_CIPHER_Access);

   --  Library context (opaque)
   type OSSL_LIB_CTX is null record;
   type OSSL_LIB_CTX_Access is access all OSSL_LIB_CTX;
   pragma Convention (C, OSSL_LIB_CTX_Access);

   --  Cipher fetching (OpenSSL 3.x provider system)
   function EVP_CIPHER_fetch
     (ctx        : OSSL_LIB_CTX_Access;
      algorithm  : Interfaces.C.char_array;
      properties : Interfaces.C.char_array) return EVP_CIPHER_Access
     with Import, Convention => C, External_Name => "EVP_CIPHER_fetch";

   procedure EVP_CIPHER_free (cipher : EVP_CIPHER_Access)
     with Import, Convention => C, External_Name => "EVP_CIPHER_free";

   --  Context management
   function EVP_CIPHER_CTX_new return EVP_CIPHER_CTX_Access
     with Import, Convention => C, External_Name => "EVP_CIPHER_CTX_new";

   procedure EVP_CIPHER_CTX_free (ctx : EVP_CIPHER_CTX_Access)
     with Import, Convention => C, External_Name => "EVP_CIPHER_CTX_free";

   function EVP_CIPHER_CTX_reset (ctx : EVP_CIPHER_CTX_Access) return Interfaces.C.int
     with Import, Convention => C, External_Name => "EVP_CIPHER_CTX_reset";

   --  Encryption/Decryption operations
   function EVP_EncryptInit_ex
     (ctx    : EVP_CIPHER_CTX_Access;
      cipher : EVP_CIPHER_Access;
      impl   : System.Address;
      key    : System.Address;
      iv     : System.Address) return Interfaces.C.int
     with Import, Convention => C, External_Name => "EVP_EncryptInit_ex";

   function EVP_EncryptUpdate
     (ctx    : EVP_CIPHER_CTX_Access;
      output : System.Address;
      outlen : access Interfaces.C.int;
      input  : System.Address;
      inlen  : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "EVP_EncryptUpdate";

   function EVP_EncryptFinal_ex
     (ctx    : EVP_CIPHER_CTX_Access;
      output : System.Address;
      outlen : access Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "EVP_EncryptFinal_ex";

   function EVP_DecryptInit_ex
     (ctx    : EVP_CIPHER_CTX_Access;
      cipher : EVP_CIPHER_Access;
      impl   : System.Address;
      key    : System.Address;
      iv     : System.Address) return Interfaces.C.int
     with Import, Convention => C, External_Name => "EVP_DecryptInit_ex";

   function EVP_DecryptUpdate
     (ctx    : EVP_CIPHER_CTX_Access;
      output : System.Address;
      outlen : access Interfaces.C.int;
      input  : System.Address;
      inlen  : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "EVP_DecryptUpdate";

   function EVP_DecryptFinal_ex
     (ctx    : EVP_CIPHER_CTX_Access;
      output : System.Address;
      outlen : access Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "EVP_DecryptFinal_ex";

   --  AEAD-specific control operations
   EVP_CTRL_AEAD_SET_IVLEN : constant := 16#9#;
   EVP_CTRL_AEAD_GET_TAG   : constant := 16#10#;
   EVP_CTRL_AEAD_SET_TAG   : constant := 16#11#;

   function EVP_CIPHER_CTX_ctrl
     (ctx  : EVP_CIPHER_CTX_Access;
      typ  : Interfaces.C.int;
      arg  : Interfaces.C.int;
      ptr  : System.Address) return Interfaces.C.int
     with Import, Convention => C, External_Name => "EVP_CIPHER_CTX_ctrl";

end Bindings.OpenSSL;
