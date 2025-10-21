pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_32; use type Interfaces.Unsigned_64;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Config;

package SparkPass.Vault is

   type Vault_State is limited record
      Header      : SparkPass.Types.Header;
      Master_Key  : Key_Array := (others => 0);
      Chain_Key   : Chain_Key_Array := (others => 0);
      Wrap_Key    : Key_Array := (others => 0);
      Entries     : Entry_Table;
      Entry_Count : Entry_Count_Type := 0;
      Unlocked    : Boolean := False;
   end record;

   --  Check if vault is unlocked and ready for operations
   --  Returns True if master keys are loaded and vault can decrypt entries
   function Is_Unlocked (State : Vault_State) return Boolean
     with
       Global  => null,
       Depends => (Is_Unlocked'Result => State),
       Post    => Is_Unlocked'Result = State.Unlocked;

   --  Clear and zeroize all sensitive vault data
   --  Wipes master keys, chain key, wrap key, and all entries from memory
   --  Must be called before deallocation to prevent key material leakage
   procedure Clear (State : in out Vault_State)
     with
       Global  => null,
       Depends => (State => State),
       Post    => not State.Unlocked and then
                  State.Entry_Count = 0;

   --  Create a new vault with freshly generated cryptographic keys
   --  Generates: ML-DSA-87 signing keypair, ML-KEM-1024 KEM keypair,
   --  random master key, random chain key
   --  Password is hashed with Argon2id (1 GiB memory, 4 iterations)
   --  Timestamp is used to initialize Created_At and Modified_At fields
   procedure Create
     (State     : out Vault_State;
      Password  : Byte_Array;
      Timestamp : U64;
      Path      : String)
     with
       Global  => null,
        Depends => (State => (Password, Timestamp, Path)),
       Pre     => Password'Length >= 12 and then
                  Password'Length <= 128 and then
                  Password'First >= 1 and then
                  Path'Length > 0 and then Path'Length <= 4096,
       Post    => State.Unlocked and then
                  State.Entry_Count = 0 and then
                  State.Header.Created_At = Timestamp and then
                  State.Header.Modified_At = Timestamp and then
                  State.Header.Nonce_Counter >= 1;

   type Open_Status is
     (Success,
      Io_Error,
      Format_Error,
      Integrity_Error,
      Authentication_Failed);

   --  Open an existing vault from disk
   --  Validates: file ownership, permissions (0600), ML-DSA-87 signature,
   --  HMAC-SHA512 fingerprint, Argon2id password hash
   --  On success: loads header, decrypts master keys, validates all entry signatures
   --  Status codes:
   --    Success: vault opened and unlocked
   --    Io_Error: file not found or cannot be read
   --    Format_Error: invalid file format or corrupted data
   --    Integrity_Error: signature/fingerprint mismatch, wrong owner, or bad permissions
   --    Authentication_Failed: incorrect password
   procedure Open
     (State     : out Vault_State;
      Path      : String;
      Password  : Byte_Array;
      Status    : out Open_Status)
     with
       Global  => null,
       Depends => ((State, Status) => (Path, Password)),
       Pre     => Path'Length > 0 and then
                  Path'Length <= 4096 and then
                  Password'Length >= 12 and then
                  Password'Length <= 128 and then
                  Password'First >= 1,
       Post    => (if Status = Success then
                     (State.Unlocked and then
                      State.Entry_Count <= Entry_Count_Type (SparkPass.Config.Max_Entries))
                   else
                     not State.Unlocked);

   --  Open an existing vault using a pre-derived wrap key (for biometric unlock)
   --  Validates: file ownership, permissions (0600), ML-DSA-87 signature,
   --  HMAC-SHA512 fingerprint
   --  SKIPS: Argon2id password derivation (uses provided wrap_key directly)
   --  On success: loads header, decrypts master keys with wrap_key
   --  Status codes:
   --    Success: vault opened and unlocked
   --    Io_Error: file not found or cannot be read
   --    Format_Error: invalid file format or corrupted data
   --    Integrity_Error: signature/fingerprint mismatch, wrong owner, or bad permissions
   --    Authentication_Failed: incorrect wrap_key (decryption failed)
   --
   --  Use case: biometric authentication retrieves cached wrap_key from OS keychain,
   --  bypassing expensive Argon2id KDF (~2.5s) for instant unlock (~50ms)
   procedure Open_With_Key
     (State     : out Vault_State;
      Path      : String;
      Wrap_Key  : Key_Array;
      Status    : out Open_Status)
     with
       Global  => null,
       Depends => ((State, Status) => (Path, Wrap_Key)),
       Pre     => Path'Length > 0 and then
                  Path'Length <= 4096 and then
                  Wrap_Key'First >= 1,
       Post    => (if Status = Success then
                     (State.Unlocked and then
                      State.Entry_Count <= Entry_Count_Type (SparkPass.Config.Max_Entries))
                   else
                     not State.Unlocked);

   type Save_Status is (Saved, Io_Error);

   --  Save vault to disk with atomic write operation
   --  Updates: Modified_At timestamp, header signature, entry signatures
   --  Write process: temp file -> fsync -> atomic rename -> fsync parent dir
   --  Ensures crash safety: either old vault or new vault exists, never partial
   procedure Save
     (State  : in out Vault_State;
      Path   : String;
      Status : out Save_Status)
     with
       Global  => null,
       Depends => ((State, Status) => (State, Path)),
       Pre     => State.Unlocked and then
                  Path'Length > 0 and then
                  Path'Length <= 4096,
       Post    => State.Unlocked and then
                  State.Entry_Count = State.Entry_Count'Old and then
                  State.Header.Modified_At >= State.Header.Modified_At'Old;

   --  Add a new password/note/TOTP entry to the vault
   --  Label: unique identifier for the entry (e.g., "github.com")
   --  Kind: Password, Note, or TOTP
   --  Plaintext: secret data (password text, note text, or TOTP seed)
   --  Encryption: AES-256-GCM-SIV with per-entry key derived from master + chain key
   --  Signature: ML-DSA-87 over encrypted entry for authenticity
   --  Success: False if label already exists or entry limit reached
   procedure Add_Entry
     (State     : in out Vault_State;
      Label     : Byte_Array;
      Kind      : Entry_Type;
      Plaintext : Byte_Array;
      Timestamp : U64;
      Success   : out Boolean)
     with
       Global  => null,
       Depends => ((State, Success) => (State, Label, Kind, Plaintext, Timestamp)),
       Pre     => State.Unlocked and then
                  Label'Length > 0 and then
                  Label'Length <= SparkPass.Config.Max_Label_Length and then
                  Plaintext'Length <= SparkPass.Config.Max_Data_Length and then
                  Plaintext'First >= 1 and then
                  Label'First >= 1 and then
                  State.Entry_Count < Entry_Count_Type (SparkPass.Config.Max_Entries),
       Post    => State.Unlocked and then
                  (if Success then
                     (State.Entry_Count = State.Entry_Count'Old + 1 and then
                      State.Header.Nonce_Counter > State.Header.Nonce_Counter'Old)
                   else
                     State.Entry_Count = State.Entry_Count'Old);

   --  Retrieve and decrypt an entry by label
   --  Label: identifier to search for (constant-time comparison to prevent timing attacks)
   --  Plaintext: output buffer (must be >= Max_Data_Length)
   --  Data_Len: actual length of decrypted data written to Plaintext
   --  Verification: validates ML-DSA-87 signature before returning data
   --  Success: False if label not found or signature verification fails
   procedure Get_Entry
     (State     : Vault_State;
      Label     : Byte_Array;
      Plaintext : out Byte_Array;
      Data_Len  : out Natural;
      Success   : out Boolean)
     with
       Global  => null,
       Depends => ((Plaintext, Data_Len, Success) => (State, Label, Plaintext)),
       Pre     => State.Unlocked and then
                  Label'Length > 0 and then
                  Label'Length <= SparkPass.Config.Max_Label_Length and then
                  Plaintext'Length >= SparkPass.Config.Max_Data_Length and then
                  Label'First >= 1 and then
                  Plaintext'First >= 1,
       Post    => Success in Boolean and then
                  (if Success then
                     (Data_Len > 0 and then
                      Data_Len <= SparkPass.Config.Max_Data_Length and then
                      Data_Len <= Plaintext'Length)
                   else
                     Data_Len = 0);

   --  Remove an entry from the vault and zeroize its data
   --  Label: identifier of entry to remove (constant-time search)
   --  Timestamp: new Modified_At timestamp
   --  The removed entry is securely wiped from memory
   --  Success: False if label not found
   procedure Remove_Entry
     (State     : in out Vault_State;
      Label     : Byte_Array;
      Timestamp : U64;
      Success   : out Boolean)
     with
       Global  => null,
       Depends => ((State, Success) => (State, Label, Timestamp)),
       Pre     => State.Unlocked and then
                  Label'Length > 0 and then
                  Label'Length <= SparkPass.Config.Max_Label_Length and then
                  Label'First >= 1 and then
                  State.Entry_Count > 0,
       Post    => State.Unlocked and then
                  (if Success then
                     (State.Entry_Count = State.Entry_Count'Old - 1 and then
                      State.Entry_Count >= 0)
                   else
                     State.Entry_Count = State.Entry_Count'Old);

   --  Rotate master key and chain key for forward secrecy
   --  Generates: new random master key, new random chain key
   --  Re-encrypts: all existing entries with new keys
   --  Re-signs: all entries with new signatures
   --  Forward secrecy: compromise of new keys cannot decrypt old vault snapshots
   --  Backward secrecy: compromise of old keys cannot decrypt new vault snapshots
   --  This operation is cryptographically expensive (re-encrypts all entries)
   procedure Rotate_Master_Key
     (State     : in out Vault_State;
      Timestamp : U64;
      Success   : out Boolean)
     with
       Global  => null,
       Depends => ((State, Success) => (State, Timestamp)),
       Pre     => State.Unlocked,
       Post    => State.Unlocked and then
                  State.Entry_Count = State.Entry_Count'Old and then
                  (if Success then
                     State.Header.Nonce_Counter > State.Header.Nonce_Counter'Old);

   --  Export ML-KEM-1024 public key for recovery access
   --  Writes the public key to a file for external storage
   --  Recovery workflow: encrypt vault with this public key, later decrypt with private key
   --  The vault must already have ML-KEM keys generated (created or opened)
   procedure Export_Recovery
     (State          : Vault_State;
      Recovery_Path  : String;
      Success        : out Boolean)
     with
       Global  => null,
       Depends => (Success => (State, Recovery_Path)),
       Pre     => State.Unlocked and then
                  Recovery_Path'Length > 0 and then
                  Recovery_Path'Length <= 4096,
       Post    => Success in Boolean;

   --  Import and decrypt vault using ML-KEM-1024 recovery mechanism
   --  Vault_Path: path to original vault (needed to decrypt ML-KEM secret key)
   --  Recovery_Path: file containing encrypted master key ciphertext
   --  Password: used to decrypt the vault and ML-KEM private key from vault header
   --  Decryption: uses ML-KEM-1024 decapsulation to recover master key
   --  This provides quantum-resistant recovery access without knowing the main password
   procedure Import_Recovery
     (Vault_Path     : String;
      Recovery_Path  : String;
      Password       : Byte_Array;
      State          : out Vault_State;
      Success        : out Boolean)
     with
       Global  => null,
       Depends => ((State, Success) => (Vault_Path, Recovery_Path, Password)),
       Pre     => Vault_Path'Length > 0 and then
                  Vault_Path'Length <= 4096 and then
                  Recovery_Path'Length > 0 and then
                  Recovery_Path'Length <= 4096 and then
                  Password'Length >= 12 and then
                  Password'Length <= 128 and then
                  Password'First >= 1,
       Post    => (if Success then
                     (State.Unlocked and then
                      State.Entry_Count <= Entry_Count_Type (SparkPass.Config.Max_Entries))
                   else
                     not State.Unlocked);

   --  Enroll Touch ID for fast vault unlock
   --  Creates Wrap D in the vault's Key-Arena, allowing the root key to be
   --  unwrapped using a device secret stored in the macOS Keychain with
   --  biometric protection.
   --
   --  This procedure:
   --    1. Generates a 32-byte random device secret
   --    2. Wraps the vault's root key (Wrap_Key) with the device secret -> Wrap D
   --    3. Adds Wrap D to the vault's Key-Arena
   --    4. The device secret is stored externally in the macOS Keychain
   --       (caller's responsibility via SparkPass.Platform.Keychain)
   --
   --  Security properties:
   --    - Wrap D is NEVER sufficient alone (policy enforced by Key-Arena)
   --    - Wrap A (passphrase) remains required for vault unlock
   --    - Touch ID provides fast unlock path but does not replace software factors
   --    - Device secret is 32 bytes of cryptographically secure random data
   --
   --  State: Unlocked vault state (must have Wrap_Key available)
   --  Device_Secret: 32-byte KEK for wrapping root key (caller-provided)
   --  Timestamp: Current timestamp for vault modification tracking
   --  Success: True if enrollment succeeded, False otherwise
   --
   --  Preconditions:
   --    - Vault must be unlocked (State.Unlocked = True)
   --    - Device_Secret must be 32 bytes
   --    - Timestamp must be valid
   --
   --  Postconditions:
   --    - On success: Wrap D is present in vault, header updated and signed
   --    - On failure: Vault state unchanged
   procedure Enroll_Touch_ID
     (State         : in out Vault_State;
      Device_Secret : in     Key_Array;
      Timestamp     : U64;
      Success       : out    Boolean)
     with
       Global  => null,
       Depends => ((State, Success) => (State, Device_Secret, Timestamp)),
       Pre     => State.Unlocked and then
                  Device_Secret'Length = 32,
       Post    => State.Unlocked and then
                  (if not Success then
                     State.Entry_Count = State.Entry_Count'Old);

   --  Unenroll Touch ID from vault
   --  Removes Wrap D from the vault's Key-Arena, disabling fast unlock.
   --
   --  This procedure:
   --    1. Removes Wrap D from the vault's Key-Arena
   --    2. Updates vault header with new fingerprint and signature
   --    3. The device secret in the macOS Keychain should be deleted separately
   --       (caller's responsibility via SparkPass.Platform.Keychain)
   --
   --  Security properties:
   --    - Vault remains unlockable with Wrap A (passphrase) after unenrollment
   --    - No sensitive data leak (Wrap D is zeroized)
   --    - Atomic operation: either fully unenrolled or unchanged
   --
   --  State: Unlocked vault state
   --  Timestamp: Current timestamp for vault modification tracking
   --  Success: True if unenrollment succeeded, False if not enrolled or error
   --
   --  Preconditions:
   --    - Vault must be unlocked (State.Unlocked = True)
   --    - Timestamp must be valid
   --
   --  Postconditions:
   --    - On success: Wrap D removed, header updated and signed
   --    - On failure: Vault state unchanged
   procedure Unenroll_Touch_ID
     (State     : in out Vault_State;
      Timestamp : U64;
      Success   : out    Boolean)
     with
       Global  => null,
       Depends => ((State, Success) => (State, Timestamp)),
       Pre     => State.Unlocked,
       Post    => State.Unlocked and then
                  State.Entry_Count = State.Entry_Count'Old;

   --  Check if Touch ID (Wrap D) is enrolled for a vault
   --  This is a lightweight check that only loads the vault header to inspect
   --  the Has_Wrap_D flag without fully opening/decrypting the vault.
   --
   --  Path: Path to .spass vault file
   --  Enrolled: True if Wrap D is present, False otherwise
   --  Status: Same status codes as Open procedure
   --
   --  Use case: CLI commands like `device status` need to check enrollment
   --  without requiring the user to unlock the vault.
   function Is_Touch_ID_Enrolled
     (Path : String) return Boolean
     with
       Global  => null,
       Pre     => Path'Length > 0 and then
                  Path'Length <= 4096;

end SparkPass.Vault;
