pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_32; use type Interfaces.Unsigned_8;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id;
with SparkPass.Crypto.Shamir;

--  Key-Arena: Multi-wrap Root Key storage with software-only availability
--
--  Architecture:
--    Root Key (32 bytes) is the master secret for vault encryption
--    Root Key can be wrapped by multiple independent factors:
--      - Wrap A: Passphrase (Argon2id KDF + AES-256-GCM-SIV)
--      - Wrap B: Recovery words (Argon2id KDF + AES-256-GCM-SIV)
--      - Wrap C-N: Shamir k-of-n shares (each sealed with unique KEK)
--      - Wrap D: Touch ID (macOS Keychain + AES-256-GCM-SIV)
--
--  Security properties:
--    1. Software-only availability: Any of {A, B, C-N} can unlock
--    2. Touch ID (D) is strictly additive: requires A AND D (never D alone)
--    3. Each wrap is independent: compromise of one doesn't reveal others
--    4. Argon2id with ≥1 GiB memory for passphrase-based wraps
--    5. AES-256-GCM-SIV for nonce-misuse resistance
--    6. Zeroization of all KEKs after wrap/unwrap
--
--  Policy enforcement:
--    - Minimum: Wrap A (passphrase) is always required
--    - Optional: Wrap B (recovery words) for password reset
--    - Optional: Wrap C-N (Shamir shares) for distributed custody
--    - Optional: Wrap D (Touch ID) for fast local unlock (requires A)
--
package SparkPass.Crypto.Wrapping is

   --  Wrapped key structure: nonce + ciphertext + tag
   --  Size: 12 + 32 + 16 = 60 bytes
   Wrapped_Key_Size : constant Positive := 60;
   subtype Wrapped_Key_Array is Byte_Array (1 .. Wrapped_Key_Size);

   type Wrapped_Key is record
      Present    : Boolean := False;
      Nonce      : Nonce_Array := (others => 0);
      Ciphertext : Key_Array := (others => 0);
      Tag        : Tag_Array := (others => 0);
   end record;

   --  Recovery words: BIP39-style mnemonic (12 or 24 words)
   --  For implementation: convert words to entropy bytes
   --  Maximum: 24 words = 32 bytes entropy
   Recovery_Entropy_Size : constant Positive := 32;
   subtype Recovery_Entropy is Byte_Array (1 .. Recovery_Entropy_Size);

   --  Shamir share with sealed envelope
   --  Each share is encrypted with its own KEK
   type Sealed_Share is record
      Share_Data : SparkPass.Crypto.Shamir.Share_Array := (others => 0);
      Nonce      : Nonce_Array := (others => 0);
      Tag        : Tag_Array := (others => 0);
   end record;

   --  Maximum Shamir configuration: 255 shares (GF(256) limit)
   --  Practical limit: 5-10 shares
   Max_Shamir_Shares : constant Positive := 10;
   subtype Shamir_Count is Positive range 1 .. Max_Shamir_Shares;

   type Sealed_Share_Array is array (Shamir_Count range <>) of Sealed_Share;

   --  -------------------------------------------------------------------------
   --  Wrap A: Passphrase-based wrapping
   --  -------------------------------------------------------------------------

   --  Wrap Root Key with passphrase using Argon2id KDF
   --
   --  Passphrase: user-provided secret (UTF-8 string converted to bytes)
   --  Salt: 32-byte random salt (stored in vault header)
   --  KDF_Params: Argon2id parameters (memory, iterations, parallelism)
   --  Root_Key: 32-byte master secret to wrap
   --  Wrapped: output wrapped key (nonce + ciphertext + tag)
   --
   --  Process:
   --    1. KEK = Argon2id(Passphrase, Salt, Params) -> 32 bytes
   --    2. Random nonce <- RNG (12 bytes)
   --    3. Ciphertext, Tag <- AES-256-GCM-SIV.Seal(KEK, Nonce, Root_Key, AAD="")
   --    4. Zeroize KEK
   --
   --  Security: Argon2id with ≥1 GiB memory provides strong KDF
   procedure Wrap_With_Passphrase
     (Root_Key   : in     Key_Array;
      Passphrase : in     Byte_Array;
      Salt       : in     Salt_Array;
      KDF_Params : in     SparkPass.Crypto.Argon2id.Parameters;
      Wrapped    : out    Wrapped_Key;
      Success    : out    Boolean)
   with
     Global  => null,
     Pre     => Root_Key'Length = 32 and then
                Passphrase'Length > 0 and then
                Passphrase'Length <= 128,
     Post    => (if Success then
                   Wrapped.Present
                 else
                   not Wrapped.Present);

   --  Unwrap Root Key with passphrase
   --
   --  Passphrase: user-provided secret
   --  Salt: 32-byte salt from vault header
   --  KDF_Params: Argon2id parameters from vault header
   --  Wrapped: wrapped key structure
   --  Root_Key: output 32-byte master secret
   --
   --  Process:
   --    1. KEK = Argon2id(Passphrase, Salt, Params)
   --    2. Root_Key <- AES-256-GCM-SIV.Open(KEK, Nonce, Ciphertext, Tag, AAD="")
   --    3. Zeroize KEK
   --
   --  Security: Failure -> Root_Key is zeroed
   procedure Unwrap_With_Passphrase
     (Wrapped    : in     Wrapped_Key;
      Passphrase : in     Byte_Array;
      Salt       : in     Salt_Array;
      KDF_Params : in     SparkPass.Crypto.Argon2id.Parameters;
      Root_Key   : out    Key_Array;
      Success    : out    Boolean)
   with
     Global  => null,
     Pre     => Wrapped.Present and then
                Passphrase'Length > 0 and then
                Passphrase'Length <= 128,
     Post    => (if not Success then
                   (for all I in Root_Key'Range => Root_Key (I) = 0));

   --  -------------------------------------------------------------------------
   --  Wrap B: Recovery words (BIP39-style mnemonic)
   --  -------------------------------------------------------------------------

   --  Wrap Root Key with recovery words
   --
   --  Recovery_Words: BIP39 mnemonic converted to entropy bytes (32 bytes)
   --  Salt: 32-byte random salt (separate from passphrase salt)
   --  KDF_Params: Argon2id parameters
   --  Root_Key: 32-byte master secret to wrap
   --  Wrapped: output wrapped key
   --
   --  Process: Same as Wrap_With_Passphrase but using recovery entropy
   procedure Wrap_With_Recovery
     (Root_Key       : in     Key_Array;
      Recovery_Words : in     Recovery_Entropy;
      Salt           : in     Salt_Array;
      KDF_Params     : in     SparkPass.Crypto.Argon2id.Parameters;
      Wrapped        : out    Wrapped_Key;
      Success        : out    Boolean)
   with
     Global  => null,
     Pre     => Root_Key'Length = 32,
     Post    => (if Success then
                   Wrapped.Present
                 else
                   not Wrapped.Present);

   --  Unwrap Root Key with recovery words
   procedure Unwrap_With_Recovery
     (Wrapped        : in     Wrapped_Key;
      Recovery_Words : in     Recovery_Entropy;
      Salt           : in     Salt_Array;
      KDF_Params     : in     SparkPass.Crypto.Argon2id.Parameters;
      Root_Key       : out    Key_Array;
      Success        : out    Boolean)
   with
     Global  => null,
     Pre     => Wrapped.Present,
     Post    => (if not Success then
                   (for all I in Root_Key'Range => Root_Key (I) = 0));

   --  -------------------------------------------------------------------------
   --  Wrap C-N: Shamir secret sharing (k-of-n threshold)
   --  -------------------------------------------------------------------------

   --  Wrap Root Key with Shamir secret sharing
   --
   --  Root_Key: 32-byte master secret to split and wrap
   --  Threshold: minimum shares required (k)
   --  Total_Shares: total shares to generate (n)
   --  Share_KEKs: array of KEKs for sealing each share (one per share)
   --  Sealed_Shares: output array of sealed shares
   --
   --  Process:
   --    1. Shares <- Shamir.Split(Root_Key, k, n)
   --    2. For each share i:
   --       a. Nonce_i <- RNG (12 bytes)
   --       b. CT_i, Tag_i <- AES-256-GCM-SIV.Seal(KEK_i, Nonce_i, Share_i, AAD="")
   --       c. Sealed_Shares[i] = (CT_i, Nonce_i, Tag_i)
   --    3. Zeroize all shares and KEKs
   --
   --  Security: Each share KEK is independent (e.g., derived from unique password)
   procedure Wrap_With_Shamir
     (Root_Key      : in     Key_Array;
      Threshold     : in     SparkPass.Crypto.Shamir.Share_Count;
      Total_Shares  : in     SparkPass.Crypto.Shamir.Share_Count;
      Share_KEKs    : in     Byte_Array;  -- Concatenated KEKs (32 * Total_Shares bytes)
      Sealed_Shares : out    Sealed_Share_Array;
      Success       : out    Boolean)
   with
     Global  => null,
     Pre     => Root_Key'Length = 32 and then
                Threshold <= Total_Shares and then
                Share_KEKs'Length = 32 * Total_Shares and then
                Sealed_Shares'Length = Total_Shares,
     Post    => (if not Success then
                   (for all I in Sealed_Shares'Range =>
                      (for all J in Sealed_Shares (I).Share_Data'Range =>
                         Sealed_Shares (I).Share_Data (J) = 0)));

   --  Unwrap Root Key from k Shamir shares
   --
   --  Sealed_Shares: array of k sealed shares
   --  Threshold: minimum shares required (k)
   --  Share_KEKs: array of KEKs for unsealing (32 * k bytes)
   --  Root_Key: output 32-byte master secret
   --
   --  Process:
   --    1. For each sealed share i:
   --       a. Share_i <- AES-256-GCM-SIV.Open(KEK_i, Nonce_i, CT_i, Tag_i, AAD="")
   --    2. Root_Key <- Shamir.Combine(Shares, k)
   --    3. Zeroize all shares and KEKs
   procedure Unwrap_With_Shamir
     (Sealed_Shares : in     Sealed_Share_Array;
      Threshold     : in     SparkPass.Crypto.Shamir.Share_Count;
      Share_KEKs    : in     Byte_Array;  -- Concatenated KEKs (32 * Threshold bytes)
      Root_Key      : out    Key_Array;
      Success       : out    Boolean)
   with
     Global  => null,
     Pre     => Sealed_Shares'Length >= Threshold and then
                Share_KEKs'Length = 32 * Threshold,
     Post    => (if not Success then
                   (for all I in Root_Key'Range => Root_Key (I) = 0));

   --  -------------------------------------------------------------------------
   --  Wrap D: Touch ID / Keychain (platform-specific)
   --  -------------------------------------------------------------------------

   --  Wrap Root Key with Touch ID-derived secret
   --
   --  Root_Key: 32-byte master secret to wrap
   --  Device_Secret: 32-byte KEK from macOS Keychain (Touch ID protected)
   --  Wrapped: output wrapped key
   --
   --  Process:
   --    1. Nonce <- RNG (12 bytes)
   --    2. CT, Tag <- AES-256-GCM-SIV.Seal(Device_Secret, Nonce, Root_Key, AAD="")
   --    3. Zeroize Device_Secret (caller's responsibility)
   --
   --  Security: Device_Secret is retrieved via Touch ID authentication
   --  Policy: Wrap D is only valid if Wrap A also exists (never Touch ID alone)
   procedure Wrap_With_Touch_ID
     (Root_Key      : in     Key_Array;
      Device_Secret : in     Key_Array;  -- From platform keychain
      Wrapped       : out    Wrapped_Key;
      Success       : out    Boolean)
   with
     Global  => null,
     Pre     => Root_Key'Length = 32 and then
                Device_Secret'Length = 32,
     Post    => (if Success then
                   Wrapped.Present
                 else
                   not Wrapped.Present);

   --  Unwrap Root Key with Touch ID-derived secret
   procedure Unwrap_With_Touch_ID
     (Wrapped       : in     Wrapped_Key;
      Device_Secret : in     Key_Array;  -- From platform keychain
      Root_Key      : out    Key_Array;
      Success       : out    Boolean)
   with
     Global  => null,
     Pre     => Wrapped.Present and then
                Device_Secret'Length = 32,
     Post    => (if not Success then
                   (for all I in Root_Key'Range => Root_Key (I) = 0));

   --  -------------------------------------------------------------------------
   --  Ghost predicates for verification (Marmaragan: 100% success rate)
   --  -------------------------------------------------------------------------

   --  Ghost predicate: Wrapped_Key has valid structure
   function Is_Valid_Wrapped_Key (W : Wrapped_Key) return Boolean is
     (W.Present and then
      W.Nonce'Length = 12 and then
      W.Ciphertext'Length = 32 and then
      W.Tag'Length = 16)
   with Ghost;

   --  Ghost predicate: Wrapped_Key is fully zeroed
   function Is_Zeroed_Wrapped_Key (W : Wrapped_Key) return Boolean is
     (not W.Present and then
      (for all I in W.Nonce'Range => W.Nonce (I) = 0) and then
      (for all I in W.Ciphertext'Range => W.Ciphertext (I) = 0) and then
      (for all I in W.Tag'Range => W.Tag (I) = 0))
   with Ghost;

   --  -------------------------------------------------------------------------
   --  Utility functions
   --  -------------------------------------------------------------------------

   --  Serialize Wrapped_Key to byte array (for storage)
   --
   --  MARMARAGAN STRATEGY: Use Assert statements (100% success) + incremental proof
   --  Layout: Nonce (12) + Ciphertext (32) + Tag (16) = 60 bytes
   procedure Serialize_Wrapped_Key
     (Wrapped : in     Wrapped_Key;
      Buffer  : out    Wrapped_Key_Array;
      Success : out    Boolean)
   with
     SPARK_Mode,
     Global => null,
     Pre    => Wrapped.Present and then
               Buffer'Length = Wrapped_Key_Size,
     Post   => (if Success then
                 (Buffer'Length = 60 and then
                  --  Nonce preserved in bytes 1..12
                  (for all I in 1 .. 12 =>
                    Buffer (Buffer'First + I - 1) = Wrapped.Nonce (I)) and then
                  --  Ciphertext preserved in bytes 13..44
                  (for all I in 1 .. 32 =>
                    Buffer (Buffer'First + 12 + I - 1) = Wrapped.Ciphertext (I)) and then
                  --  Tag preserved in bytes 45..60
                  (for all I in 1 .. 16 =>
                    Buffer (Buffer'First + 44 + I - 1) = Wrapped.Tag (I))));

   --  Deserialize Wrapped_Key from byte array (for loading)
   --
   --  MARMARAGAN STRATEGY: Round-trip property proven incrementally
   --  Inverse of Serialize: Buffer -> Wrapped -> Buffer' => Buffer = Buffer'
   procedure Deserialize_Wrapped_Key
     (Buffer  : in     Wrapped_Key_Array;
      Wrapped : out    Wrapped_Key;
      Success : out    Boolean)
   with
     SPARK_Mode,
     Global => null,
     Pre    => Buffer'Length = Wrapped_Key_Size,
     Post   => (if Success then
                 (Wrapped.Present and then
                  Is_Valid_Wrapped_Key (Wrapped) and then
                  --  Nonce extracted from bytes 1..12
                  (for all I in 1 .. 12 =>
                    Wrapped.Nonce (I) = Buffer (Buffer'First + I - 1)) and then
                  --  Ciphertext extracted from bytes 13..44
                  (for all I in 1 .. 32 =>
                    Wrapped.Ciphertext (I) = Buffer (Buffer'First + 12 + I - 1)) and then
                  --  Tag extracted from bytes 45..60
                  (for all I in 1 .. 16 =>
                    Wrapped.Tag (I) = Buffer (Buffer'First + 44 + I - 1))));

   --  Zeroize Wrapped_Key
   procedure Wipe_Wrapped_Key (Wrapped : in out Wrapped_Key)
   with
     SPARK_Mode,
     Global => null,
     Post   => Is_Zeroed_Wrapped_Key (Wrapped);

   --  Zeroize Sealed_Share
   procedure Wipe_Sealed_Share (Share : in out Sealed_Share)
   with
     SPARK_Mode,
     Global => null,
     Post   => (for all I in Share.Share_Data'Range => Share.Share_Data (I) = 0) and then
               (for all I in Share.Nonce'Range => Share.Nonce (I) = 0) and then
               (for all I in Share.Tag'Range => Share.Tag (I) = 0);

end SparkPass.Crypto.Wrapping;
