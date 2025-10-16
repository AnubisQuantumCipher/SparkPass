pragma SPARK_Mode (On);
with System;
with Interfaces; use type Interfaces.Unsigned_8;
with Interfaces.C;
with Bindings.OpenSSL;
with SparkPass.Crypto.Zeroize;
use type Bindings.OpenSSL.EVP_CIPHER_Access;
use type Bindings.OpenSSL.EVP_CIPHER_CTX_Access;

package body SparkPass.Crypto.AES_GCM_SIV is

   procedure Seal
     (Key        : in  Key_Array;
      Nonce      : in  Nonce_Array;
      Plaintext  : in  Byte_Array;
      AAD        : in  Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out Tag_Array)
   is
      use Interfaces.C;
      Ctx : Bindings.OpenSSL.EVP_CIPHER_CTX_Access;
      Cipher : Bindings.OpenSSL.EVP_CIPHER_Access;
      Result : Interfaces.C.int;
      Outlen : aliased Interfaces.C.int := 0;
      Tmplen : aliased Interfaces.C.int := 0;
   begin
      if Ciphertext'Length /= Plaintext'Length then
         raise Constraint_Error with "Ciphertext length mismatch";
      end if;

      --  Fetch AES-256-GCM-SIV cipher using OpenSSL 3 provider system
      Cipher := Bindings.OpenSSL.EVP_CIPHER_fetch
        (ctx        => null,  -- Use default library context
         algorithm  => Interfaces.C.To_C ("AES-256-GCM-SIV"),
         properties => Interfaces.C.To_C (""));

      if Cipher = null then
         raise Program_Error with "AES-256-GCM-SIV not available (requires OpenSSL 3.2+)";
      end if;

      --  Create and initialize context
      Ctx := Bindings.OpenSSL.EVP_CIPHER_CTX_new;
      if Ctx = null then
         Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
         raise Program_Error with "Failed to create cipher context";
      end if;

      --  Initialize encryption with key and nonce (IV)
      Result := Bindings.OpenSSL.EVP_EncryptInit_ex
        (ctx    => Ctx,
         cipher => Cipher,
         impl   => System.Null_Address,
         key    => Key (Key'First)'Address,
         iv     => Nonce (Nonce'First)'Address);

      if Result /= 1 then
         Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
         Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
         SparkPass.Crypto.Zeroize.Wipe (Ciphertext);
         SparkPass.Crypto.Zeroize.Wipe_Tag (Tag);
         raise Program_Error with "AES-256-GCM-SIV init failed";
      end if;

      --  Provide AAD (Additional Authenticated Data)
      if AAD'Length > 0 then
         Result := Bindings.OpenSSL.EVP_EncryptUpdate
           (ctx    => Ctx,
            output => System.Null_Address,
            outlen => Tmplen'Access,
            input  => AAD (AAD'First)'Address,
            inlen  => Interfaces.C.int (AAD'Length));

         if Result /= 1 then
            Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
            Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
            SparkPass.Crypto.Zeroize.Wipe (Ciphertext);
            SparkPass.Crypto.Zeroize.Wipe_Tag (Tag);
            raise Program_Error with "AES-256-GCM-SIV AAD failed";
         end if;
      end if;

      --  Encrypt plaintext
      Result := Bindings.OpenSSL.EVP_EncryptUpdate
        (ctx    => Ctx,
         output => Ciphertext (Ciphertext'First)'Address,
         outlen => Outlen'Access,
         input  => Plaintext (Plaintext'First)'Address,
         inlen  => Interfaces.C.int (Plaintext'Length));

      if Result /= 1 then
         Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
         Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
         SparkPass.Crypto.Zeroize.Wipe (Ciphertext);
         SparkPass.Crypto.Zeroize.Wipe_Tag (Tag);
         raise Program_Error with "AES-256-GCM-SIV encryption failed";
      end if;

      --  Finalize encryption
      Result := Bindings.OpenSSL.EVP_EncryptFinal_ex
        (ctx    => Ctx,
         output => System.Null_Address,
         outlen => Tmplen'Access);

      if Result /= 1 then
         Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
         Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
         SparkPass.Crypto.Zeroize.Wipe (Ciphertext);
         SparkPass.Crypto.Zeroize.Wipe_Tag (Tag);
         raise Program_Error with "AES-256-GCM-SIV finalization failed";
      end if;

      --  Get authentication tag
      Result := Bindings.OpenSSL.EVP_CIPHER_CTX_ctrl
        (ctx => Ctx,
         typ => Bindings.OpenSSL.EVP_CTRL_AEAD_GET_TAG,
         arg => Tag'Length,
         ptr => Tag (Tag'First)'Address);

      if Result /= 1 then
         Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
         Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
         SparkPass.Crypto.Zeroize.Wipe (Ciphertext);
         SparkPass.Crypto.Zeroize.Wipe_Tag (Tag);
         raise Program_Error with "AES-256-GCM-SIV tag extraction failed";
      end if;

      --  Clean up context and cipher
      Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
      Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
   end Seal;

   procedure Open
     (Key        : in  Key_Array;
      Nonce      : in  Nonce_Array;
      Ciphertext : in  Byte_Array;
      AAD        : in  Byte_Array;
      Tag        : in  Tag_Array;
      Plaintext  : out Byte_Array;
      Success    : out Boolean)
   is
      use Interfaces.C;
      Ctx : Bindings.OpenSSL.EVP_CIPHER_CTX_Access;
      Cipher : Bindings.OpenSSL.EVP_CIPHER_Access;
      Result : Interfaces.C.int;
      Outlen : aliased Interfaces.C.int := 0;
      Tmplen : aliased Interfaces.C.int := 0;
   begin
      if Plaintext'Length /= Ciphertext'Length then
         raise Constraint_Error with "Plaintext length mismatch";
      end if;

      Success := False;
      SparkPass.Crypto.Zeroize.Wipe (Plaintext);

      --  Fetch AES-256-GCM-SIV cipher using OpenSSL 3 provider system
      Cipher := Bindings.OpenSSL.EVP_CIPHER_fetch
        (ctx        => null,  -- Use default library context
         algorithm  => Interfaces.C.To_C ("AES-256-GCM-SIV"),
         properties => Interfaces.C.To_C (""));

      if Cipher = null then
         raise Program_Error with "AES-256-GCM-SIV not available (requires OpenSSL 3.2+)";
      end if;

      --  Create and initialize context
      Ctx := Bindings.OpenSSL.EVP_CIPHER_CTX_new;
      if Ctx = null then
         Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
         return;
      end if;

      --  Initialize decryption with key and nonce (IV)
      Result := Bindings.OpenSSL.EVP_DecryptInit_ex
        (ctx    => Ctx,
         cipher => Cipher,
         impl   => System.Null_Address,
         key    => Key (Key'First)'Address,
         iv     => Nonce (Nonce'First)'Address);

      if Result /= 1 then
         Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
         Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
         SparkPass.Crypto.Zeroize.Wipe (Plaintext);
         return;
      end if;

      --  Set expected authentication tag (MUST be set before processing AAD/ciphertext for GCM-SIV)
      Result := Bindings.OpenSSL.EVP_CIPHER_CTX_ctrl
        (ctx => Ctx,
         typ => Bindings.OpenSSL.EVP_CTRL_AEAD_SET_TAG,
         arg => Tag'Length,
         ptr => Tag (Tag'First)'Address);

      if Result /= 1 then
         Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
         Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
         SparkPass.Crypto.Zeroize.Wipe (Plaintext);
         return;
      end if;

      --  Provide AAD (Additional Authenticated Data)
      if AAD'Length > 0 then
         Result := Bindings.OpenSSL.EVP_DecryptUpdate
           (ctx    => Ctx,
            output => System.Null_Address,
            outlen => Tmplen'Access,
            input  => AAD (AAD'First)'Address,
            inlen  => Interfaces.C.int (AAD'Length));

         if Result /= 1 then
            Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
            Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
            SparkPass.Crypto.Zeroize.Wipe (Plaintext);
            return;
         end if;
      end if;

      --  Decrypt ciphertext
      Result := Bindings.OpenSSL.EVP_DecryptUpdate
        (ctx    => Ctx,
         output => Plaintext (Plaintext'First)'Address,
         outlen => Outlen'Access,
         input  => Ciphertext (Ciphertext'First)'Address,
         inlen  => Interfaces.C.int (Ciphertext'Length));

      if Result /= 1 then
         Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
         Bindings.OpenSSL.EVP_CIPHER_free (Cipher);
         SparkPass.Crypto.Zeroize.Wipe (Plaintext);
         return;
      end if;

      --  Finalize decryption and verify tag
      Result := Bindings.OpenSSL.EVP_DecryptFinal_ex
        (ctx    => Ctx,
         output => System.Null_Address,
         outlen => Tmplen'Access);

      --  Clean up context and cipher
      Bindings.OpenSSL.EVP_CIPHER_CTX_free (Ctx);
      Bindings.OpenSSL.EVP_CIPHER_free (Cipher);

      if Result = 1 then
         Success := True;
      else
         Success := False;
         SparkPass.Crypto.Zeroize.Wipe (Plaintext);
      end if;
   end Open;

end SparkPass.Crypto.AES_GCM_SIV;
