pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_8; use type Interfaces.Unsigned_64;
with SparkPass.Types; use SparkPass.Types;

--  Platform-specific keychain integration for biometric authentication
--
--  This package provides secure storage of the vault wrap_key in the OS keychain,
--  protected by biometric authentication (Touch ID on macOS, Windows Hello, etc.).
--
--  Security properties:
--    1. Wrap key is encrypted by OS using hardware-backed key (Secure Enclave/TPM)
--    2. Retrieval requires biometric authentication
--    3. Cache expires after 7 days (enforced by timestamp check)
--    4. Cache is invalidated on security events (system sleep, failed attempts)
--    5. All sensitive data is zeroized after use
--
--  Usage pattern:
--    1. User unlocks vault with password (slow: ~2.5s Argon2id)
--    2. Wrap key is cached via Store_Wrap_Key
--    3. Subsequent unlocks use Retrieve_Wrap_Key (fast: ~50ms biometric)
--    4. After 7 days, cache expires and password is required again
--
package SparkPass.Platform.Keychain is

   --  Maximum cache age in seconds (7 days)
   Cache_Max_Age : constant := 7 * 24 * 60 * 60;  -- 604,800 seconds

   --  Store wrap key in OS keychain with biometric protection
   --
   --  The wrap key is stored with:
   --    - Service name: "com.sparkpass.vault"
   --    - Account name: vault file path (to support multiple vaults)
   --    - Access control: Requires biometric authentication to retrieve
   --    - Accessibility: Only when device is unlocked (not in backup)
   --
   --  @param Wrap_Key The 32-byte wrap key to store (will be zeroized after use)
   --  @param Vault_Path Absolute path to vault file (used as unique identifier)
   --  @param Timestamp Current Unix timestamp (for cache expiration tracking)
   --  @param Success True if key was stored successfully, False on error
   --
   --  Errors: May fail if biometric hardware unavailable, keychain locked,
   --          or duplicate entry exists (must call Delete_Wrap_Key first)
   procedure Store_Wrap_Key
     (Wrap_Key   : in out Key_Array;
      Vault_Path : String;
      Timestamp  : U64;
      Success    : out Boolean)
   with
     Pre => Vault_Path'Length > 0 and Vault_Path'Length < 4096,
     Post => (for all I in Wrap_Key'Range => Wrap_Key (I) = 0);  -- Zeroized

   --  Retrieve wrap key from OS keychain (triggers biometric prompt)
   --
   --  This operation will:
   --    1. Prompt user for biometric authentication (Touch ID, Face ID, etc.)
   --    2. Retrieve encrypted wrap key from keychain
   --    3. Check cache age against 7-day expiration
   --    4. Return decrypted wrap key if valid and not expired
   --
   --  @param Wrap_Key Output buffer for 32-byte wrap key (zeroized on entry)
   --  @param Vault_Path Absolute path to vault file (used as unique identifier)
   --  @param Current_Time Current Unix timestamp (for expiration check)
   --  @param Success True if key retrieved and valid, False on error or expired
   --
   --  Errors: May fail if user cancels biometric prompt, cache expired,
   --          no cached key exists, or biometric authentication failed
   procedure Retrieve_Wrap_Key
     (Wrap_Key     : out Key_Array;
      Vault_Path   : String;
      Current_Time : U64;
      Success      : out Boolean)
   with
     Pre => Vault_Path'Length > 0 and Vault_Path'Length < 4096;

   --  Delete wrap key from OS keychain (invalidate cache)
   --
   --  Used when:
   --    - User explicitly disables biometric unlock
   --    - Security event detected (wrong password attempt)
   --    - Vault password changed
   --    - Key rotation performed
   --
   --  @param Vault_Path Absolute path to vault file
   --  @param Success True if key deleted or not found, False on other errors
   procedure Delete_Wrap_Key
     (Vault_Path : String;
      Success    : out Boolean)
   with
     Pre => Vault_Path'Length > 0 and Vault_Path'Length < 4096;

   --  Check if cached wrap key exists in keychain (without authentication)
   --
   --  This performs an existence check only, without retrieving the actual key data.
   --  No biometric authentication is required, making this suitable for status displays.
   --
   --  Note: Cannot determine cache age without authentication (would require data retrieval).
   --        Expiration checking happens during Retrieve_Wrap_Key when authentication is provided.
   --
   --  @param Vault_Path Absolute path to vault file
   --  @return True if cached key exists, False otherwise
   function Has_Cached_Key (Vault_Path : String) return Boolean
   with
     Pre => Vault_Path'Length > 0 and Vault_Path'Length < 4096;

end SparkPass.Platform.Keychain;
