pragma SPARK_Mode (On);
with SparkPass.Crypto.AES_GCM_SIV;
with SparkPass.Crypto.Random;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.Wrapping is

   --  -------------------------------------------------------------------------
   --  Internal helpers
   --  -------------------------------------------------------------------------

   --  Derive KEK from input material using Argon2id
   procedure Derive_KEK
     (Input      : in     Byte_Array;
      Salt       : in     Salt_Array;
      KDF_Params : in     SparkPass.Crypto.Argon2id.Parameters;
      KEK        : out    Key_Array;
      Success    : out    Boolean)
   with
     Global => null,
     Pre    => Input'Length > 0 and Input'Length <= 128,
     Post   => (if not Success then
                  (for all I in KEK'Range => KEK (I) = 0))
   is
      Params : SparkPass.Crypto.Argon2id.Parameters := KDF_Params;
   begin
      Params.Salt := Salt;
      SparkPass.Crypto.Argon2id.Derive (Input, Params, KEK, Success);
   end Derive_KEK;

   --  Seal data with AES-256-GCM-SIV
   procedure Seal_Data
     (KEK        : in     Key_Array;
      Plaintext  : in     Byte_Array;
      Nonce      : out    Nonce_Array;
      Ciphertext : out    Byte_Array;
      Tag        : out    Tag_Array;
      Success    : out    Boolean)
   with
     Global => null,
     Pre    => Plaintext'Length = Ciphertext'Length and
               Plaintext'Length <= 32,
     Post   => (if not Success then
                  (for all I in Ciphertext'Range => Ciphertext (I) = 0))
   is
      AAD : constant Byte_Array (1 .. 0) := (others => 0);  -- Empty AAD
   begin
      Success := False;

      --  Generate random nonce
      declare
         Random_Block : Byte_Array (1 .. 64) := (others => 0);
      begin
         SparkPass.Crypto.Random.Fill (Random_Block);
         Nonce := Random_Block (1 .. 12);
         SparkPass.Crypto.Zeroize.Wipe (Random_Block);
      end;

      --  Seal with AES-256-GCM-SIV
      SparkPass.Crypto.AES_GCM_SIV.Seal
        (Key        => KEK,
         Nonce      => Nonce,
         Plaintext  => Plaintext,
         AAD        => AAD,
         Ciphertext => Ciphertext,
         Tag        => Tag);

      Success := True;
   end Seal_Data;

   --  Open data with AES-256-GCM-SIV
   procedure Open_Data
     (KEK        : in     Key_Array;
      Nonce      : in     Nonce_Array;
      Ciphertext : in     Byte_Array;
      Tag        : in     Tag_Array;
      Plaintext  : out    Byte_Array;
      Success    : out    Boolean)
   with
     Global => null,
     Pre    => Plaintext'Length = Ciphertext'Length and
               Ciphertext'Length <= 32,
     Post   => (if not Success then
                  (for all I in Plaintext'Range => Plaintext (I) = 0))
   is
      AAD : constant Byte_Array (1 .. 0) := (others => 0);  -- Empty AAD
   begin
      SparkPass.Crypto.AES_GCM_SIV.Open
        (Key        => KEK,
         Nonce      => Nonce,
         Ciphertext => Ciphertext,
         AAD        => AAD,
         Tag        => Tag,
         Plaintext  => Plaintext,
         Success    => Success);

      if not Success then
         SparkPass.Crypto.Zeroize.Wipe (Plaintext);
      end if;
   end Open_Data;

   --  -------------------------------------------------------------------------
   --  Wrap A: Passphrase
   --  -------------------------------------------------------------------------

   procedure Wrap_With_Passphrase
     (Root_Key   : in     Key_Array;
      Passphrase : in     Byte_Array;
      Salt       : in     Salt_Array;
      KDF_Params : in     SparkPass.Crypto.Argon2id.Parameters;
      Wrapped    : out    Wrapped_Key;
      Success    : out    Boolean)
   is
      KEK : Key_Array := (others => 0);
      KEK_Success : Boolean := False;
   begin
      Wrapped.Present := False;
      Success := False;

      --  Derive KEK from passphrase
      Derive_KEK (Passphrase, Salt, KDF_Params, KEK, KEK_Success);
      if not KEK_Success then
         SparkPass.Crypto.Zeroize.Wipe_Key (KEK);
         return;
      end if;

      --  Seal Root_Key with KEK
      Seal_Data
        (KEK        => KEK,
         Plaintext  => Root_Key,
         Nonce      => Wrapped.Nonce,
         Ciphertext => Wrapped.Ciphertext,
         Tag        => Wrapped.Tag,
         Success    => Success);

      --  Zeroize KEK
      SparkPass.Crypto.Zeroize.Wipe_Key (KEK);

      if Success then
         Wrapped.Present := True;
      end if;
   end Wrap_With_Passphrase;

   procedure Unwrap_With_Passphrase
     (Wrapped    : in     Wrapped_Key;
      Passphrase : in     Byte_Array;
      Salt       : in     Salt_Array;
      KDF_Params : in     SparkPass.Crypto.Argon2id.Parameters;
      Root_Key   : out    Key_Array;
      Success    : out    Boolean)
   is
      KEK : Key_Array := (others => 0);
      KEK_Success : Boolean := False;
   begin
      Root_Key := (others => 0);
      Success := False;

      --  Derive KEK from passphrase
      Derive_KEK (Passphrase, Salt, KDF_Params, KEK, KEK_Success);
      if not KEK_Success then
         SparkPass.Crypto.Zeroize.Wipe_Key (KEK);
         return;
      end if;

      --  Open wrapped Root_Key
      Open_Data
        (KEK        => KEK,
         Nonce      => Wrapped.Nonce,
         Ciphertext => Wrapped.Ciphertext,
         Tag        => Wrapped.Tag,
         Plaintext  => Root_Key,
         Success    => Success);

      --  Zeroize KEK
      SparkPass.Crypto.Zeroize.Wipe_Key (KEK);

      if not Success then
         SparkPass.Crypto.Zeroize.Wipe_Key (Root_Key);
      end if;
   end Unwrap_With_Passphrase;

   --  -------------------------------------------------------------------------
   --  Wrap B: Recovery words
   --  -------------------------------------------------------------------------

   procedure Wrap_With_Recovery
     (Root_Key       : in     Key_Array;
      Recovery_Words : in     Recovery_Entropy;
      Salt           : in     Salt_Array;
      KDF_Params     : in     SparkPass.Crypto.Argon2id.Parameters;
      Wrapped        : out    Wrapped_Key;
      Success        : out    Boolean)
   is
      KEK : Key_Array := (others => 0);
      KEK_Success : Boolean := False;
   begin
      Wrapped.Present := False;
      Success := False;

      --  Derive KEK from recovery entropy
      Derive_KEK (Recovery_Words, Salt, KDF_Params, KEK, KEK_Success);
      if not KEK_Success then
         SparkPass.Crypto.Zeroize.Wipe_Key (KEK);
         return;
      end if;

      --  Seal Root_Key with KEK
      Seal_Data
        (KEK        => KEK,
         Plaintext  => Root_Key,
         Nonce      => Wrapped.Nonce,
         Ciphertext => Wrapped.Ciphertext,
         Tag        => Wrapped.Tag,
         Success    => Success);

      --  Zeroize KEK
      SparkPass.Crypto.Zeroize.Wipe_Key (KEK);

      if Success then
         Wrapped.Present := True;
      end if;
   end Wrap_With_Recovery;

   procedure Unwrap_With_Recovery
     (Wrapped        : in     Wrapped_Key;
      Recovery_Words : in     Recovery_Entropy;
      Salt           : in     Salt_Array;
      KDF_Params     : in     SparkPass.Crypto.Argon2id.Parameters;
      Root_Key       : out    Key_Array;
      Success        : out    Boolean)
   is
      KEK : Key_Array := (others => 0);
      KEK_Success : Boolean := False;
   begin
      Root_Key := (others => 0);
      Success := False;

      --  Derive KEK from recovery entropy
      Derive_KEK (Recovery_Words, Salt, KDF_Params, KEK, KEK_Success);
      if not KEK_Success then
         SparkPass.Crypto.Zeroize.Wipe_Key (KEK);
         return;
      end if;

      --  Open wrapped Root_Key
      Open_Data
        (KEK        => KEK,
         Nonce      => Wrapped.Nonce,
         Ciphertext => Wrapped.Ciphertext,
         Tag        => Wrapped.Tag,
         Plaintext  => Root_Key,
         Success    => Success);

      --  Zeroize KEK
      SparkPass.Crypto.Zeroize.Wipe_Key (KEK);

      if not Success then
         SparkPass.Crypto.Zeroize.Wipe_Key (Root_Key);
      end if;
   end Unwrap_With_Recovery;

   --  -------------------------------------------------------------------------
   --  Wrap C-N: Shamir secret sharing
   --  -------------------------------------------------------------------------

   procedure Wrap_With_Shamir
     (Root_Key      : in     Key_Array;
      Threshold     : in     SparkPass.Crypto.Shamir.Share_Count;
      Total_Shares  : in     SparkPass.Crypto.Shamir.Share_Count;
      Share_KEKs    : in     Byte_Array;
      Sealed_Shares : out    Sealed_Share_Array;
      Success       : out    Boolean)
   is
      Shares : SparkPass.Crypto.Shamir.Share_Set (1 .. Total_Shares);
      Split_Success : Boolean := False;
      KEK_Offset : Positive := Share_KEKs'First;
   begin
      Success := False;

      --  Initialize sealed shares
      for I in Sealed_Shares'Range loop
         Sealed_Shares (I).Share_Data := (others => 0);
         Sealed_Shares (I).Nonce := (others => 0);
         Sealed_Shares (I).Tag := (others => 0);
      end loop;

      --  Split Root_Key into shares
      SparkPass.Crypto.Shamir.Split
        (Root_Key     => Root_Key,
         Threshold    => Threshold,
         Total_Shares => Total_Shares,
         Shares       => Shares,
         Success      => Split_Success);

      if not Split_Success then
         SparkPass.Crypto.Shamir.Wipe_Share_Set (Shares);
         return;
      end if;

      --  Seal each share with its KEK
      for I in 1 .. Total_Shares loop
         declare
            KEK : Key_Array;
            Seal_Success : Boolean := False;
         begin
            --  Extract KEK for this share
            for J in KEK'Range loop
               KEK (J) := Share_KEKs (KEK_Offset);
               KEK_Offset := KEK_Offset + 1;
            end loop;

            --  Seal the share
            Seal_Data
              (KEK        => KEK,
               Plaintext  => Shares (I),
               Nonce      => Sealed_Shares (I).Nonce,
               Ciphertext => Sealed_Shares (I).Share_Data,
               Tag        => Sealed_Shares (I).Tag,
               Success    => Seal_Success);

            --  Zeroize KEK
            SparkPass.Crypto.Zeroize.Wipe_Key (KEK);

            if not Seal_Success then
               SparkPass.Crypto.Shamir.Wipe_Share_Set (Shares);
               --  Wipe already-sealed shares
               for K in 1 .. I loop
                  Wipe_Sealed_Share (Sealed_Shares (K));
               end loop;
               return;
            end if;
         end;
      end loop;

      --  Zeroize plaintext shares
      SparkPass.Crypto.Shamir.Wipe_Share_Set (Shares);

      Success := True;
   end Wrap_With_Shamir;

   procedure Unwrap_With_Shamir
     (Sealed_Shares : in     Sealed_Share_Array;
      Threshold     : in     SparkPass.Crypto.Shamir.Share_Count;
      Share_KEKs    : in     Byte_Array;
      Root_Key      : out    Key_Array;
      Success       : out    Boolean)
   is
      Shares : SparkPass.Crypto.Shamir.Share_Set (1 .. Threshold);
      KEK_Offset : Positive := Share_KEKs'First;
      Combine_Success : Boolean := False;
   begin
      Root_Key := (others => 0);
      Success := False;

      --  Initialize shares
      for I in Shares'Range loop
         Shares (I) := (others => 0);
      end loop;

      --  Unseal each share with its KEK
      for I in 1 .. Threshold loop
         declare
            KEK : Key_Array;
            Open_Success : Boolean := False;
         begin
            --  Extract KEK for this share
            for J in KEK'Range loop
               KEK (J) := Share_KEKs (KEK_Offset);
               KEK_Offset := KEK_Offset + 1;
            end loop;

            --  Unseal the share
            Open_Data
              (KEK        => KEK,
               Nonce      => Sealed_Shares (I).Nonce,
               Ciphertext => Sealed_Shares (I).Share_Data,
               Tag        => Sealed_Shares (I).Tag,
               Plaintext  => Shares (I),
               Success    => Open_Success);

            --  Zeroize KEK
            SparkPass.Crypto.Zeroize.Wipe_Key (KEK);

            if not Open_Success then
               SparkPass.Crypto.Shamir.Wipe_Share_Set (Shares);
               return;
            end if;
         end;
      end loop;

      --  Reconstruct Root_Key from shares
      SparkPass.Crypto.Shamir.Combine
        (Shares    => Shares,
         Threshold => Threshold,
         Root_Key  => Root_Key,
         Success   => Combine_Success);

      --  Zeroize shares
      SparkPass.Crypto.Shamir.Wipe_Share_Set (Shares);

      if not Combine_Success then
         SparkPass.Crypto.Zeroize.Wipe_Key (Root_Key);
         return;
      end if;

      Success := True;
   end Unwrap_With_Shamir;

   --  -------------------------------------------------------------------------
   --  Wrap D: Touch ID / Keychain
   --  -------------------------------------------------------------------------

   procedure Wrap_With_Touch_ID
     (Root_Key      : in     Key_Array;
      Device_Secret : in     Key_Array;
      Wrapped       : out    Wrapped_Key;
      Success       : out    Boolean)
   is
   begin
      Wrapped.Present := False;
      Success := False;

      --  Seal Root_Key with Device_Secret (no KDF needed)
      Seal_Data
        (KEK        => Device_Secret,
         Plaintext  => Root_Key,
         Nonce      => Wrapped.Nonce,
         Ciphertext => Wrapped.Ciphertext,
         Tag        => Wrapped.Tag,
         Success    => Success);

      if Success then
         Wrapped.Present := True;
      end if;
   end Wrap_With_Touch_ID;

   procedure Unwrap_With_Touch_ID
     (Wrapped       : in     Wrapped_Key;
      Device_Secret : in     Key_Array;
      Root_Key      : out    Key_Array;
      Success       : out    Boolean)
   is
   begin
      Root_Key := (others => 0);
      Success := False;

      --  Open wrapped Root_Key with Device_Secret
      Open_Data
        (KEK        => Device_Secret,
         Nonce      => Wrapped.Nonce,
         Ciphertext => Wrapped.Ciphertext,
         Tag        => Wrapped.Tag,
         Plaintext  => Root_Key,
         Success    => Success);

      if not Success then
         SparkPass.Crypto.Zeroize.Wipe_Key (Root_Key);
      end if;
   end Unwrap_With_Touch_ID;

   --  -------------------------------------------------------------------------
   --  Utility functions
   --  -------------------------------------------------------------------------

   procedure Serialize_Wrapped_Key
     (Wrapped : in     Wrapped_Key;
      Buffer  : out    Wrapped_Key_Array;
      Success : out    Boolean)
   is
      Offset : Positive := Buffer'First;
   begin
      Success := False;

      if not Wrapped.Present then
         Buffer := (others => 0);
         return;
      end if;

      --  Layout: Nonce (12) + Ciphertext (32) + Tag (16) = 60 bytes
      for I in Wrapped.Nonce'Range loop
         Buffer (Offset) := Wrapped.Nonce (I);
         Offset := Offset + 1;
      end loop;

      for I in Wrapped.Ciphertext'Range loop
         Buffer (Offset) := Wrapped.Ciphertext (I);
         Offset := Offset + 1;
      end loop;

      for I in Wrapped.Tag'Range loop
         Buffer (Offset) := Wrapped.Tag (I);
         Offset := Offset + 1;
      end loop;

      Success := True;
   end Serialize_Wrapped_Key;

   procedure Deserialize_Wrapped_Key
     (Buffer  : in     Wrapped_Key_Array;
      Wrapped : out    Wrapped_Key;
      Success : out    Boolean)
   is
      Offset : Positive := Buffer'First;
   begin
      Wrapped.Present := False;
      Success := False;

      --  Layout: Nonce (12) + Ciphertext (32) + Tag (16) = 60 bytes
      for I in Wrapped.Nonce'Range loop
         Wrapped.Nonce (I) := Buffer (Offset);
         Offset := Offset + 1;
      end loop;

      for I in Wrapped.Ciphertext'Range loop
         Wrapped.Ciphertext (I) := Buffer (Offset);
         Offset := Offset + 1;
      end loop;

      for I in Wrapped.Tag'Range loop
         Wrapped.Tag (I) := Buffer (Offset);
         Offset := Offset + 1;
      end loop;

      Wrapped.Present := True;
      Success := True;
   end Deserialize_Wrapped_Key;

   procedure Wipe_Wrapped_Key (Wrapped : in out Wrapped_Key) is
   begin
      Wrapped.Present := False;
      SparkPass.Crypto.Zeroize.Wipe (Wrapped.Nonce);
      SparkPass.Crypto.Zeroize.Wipe_Key (Wrapped.Ciphertext);
      SparkPass.Crypto.Zeroize.Wipe_Tag (Wrapped.Tag);
   end Wipe_Wrapped_Key;

   procedure Wipe_Sealed_Share (Share : in out Sealed_Share) is
   begin
      SparkPass.Crypto.Shamir.Wipe_Share (Share.Share_Data);
      SparkPass.Crypto.Zeroize.Wipe (Share.Nonce);
      SparkPass.Crypto.Zeroize.Wipe_Tag (Share.Tag);
   end Wipe_Sealed_Share;

end SparkPass.Crypto.Wrapping;
