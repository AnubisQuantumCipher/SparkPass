pragma SPARK_Mode (Off);  -- CLI operations involve I/O and are not SPARK-verifiable
with SparkPass.Types; use SparkPass.Types;

--  SparkPass Device Management CLI
--
--  Provides CLI commands for managing biometric (Touch ID) device enrollment,
--  testing, and unenrollment. Integrates with the Key-Arena Wrap D layer and
--  the vault policy engine to provide fast unlock while maintaining software-only
--  availability guarantees.
--
--  Security model:
--    - Touch ID NEVER as sole unlock factor (policy enforced)
--    - Fast unlock requires Passphrase + Touch ID (multi-factor)
--    - Vault remains unlockable with passphrase alone on any device
--    - TTL enforcement: fast unlock expires after N minutes
--    - Scope enforcement: read-only vs full access control
--    - Device secret stored in macOS Keychain with biometric protection
--
--  Commands:
--    device enroll --touchid    : Enroll Touch ID for fast unlock
--    device test                : Test Touch ID availability and status
--    device unenroll            : Remove Touch ID enrollment
--    device status              : Show device enrollment status
--
package SparkPass.CLI.Device is

   --  Device enrollment status codes
   type Enrollment_Status is
     (Not_Enrolled,       -- No Touch ID enrollment
      Enrolled,           -- Touch ID enrolled and active
      Expired,            -- Touch ID enrollment expired (TTL exceeded)
      Hardware_Unavailable); -- Touch ID hardware not available

   --  Enroll Touch ID for fast vault unlock
   --
   --  This command performs the complete Touch ID enrollment workflow:
   --    1. Check LAContext availability (macOS 10.12+, Touch ID hardware)
   --    2. Prompt for Touch ID authentication
   --    3. Prompt for vault passphrase (policy requirement)
   --    4. Generate ephemeral device secret (32 bytes random)
   --    5. Store device secret in macOS Keychain with biometric protection
   --    6. Wrap Root Key with device secret -> Wrap D
   --    7. Add Wrap D to Key-Arena in vault header
   --    8. Update vault policy (enable fast unlock, set TTL/scope)
   --    9. Write updated header to disk with ML-DSA signature
   --
   --  Parameters:
   --    Vault_Path: Path to .spass vault file
   --    TTL_Minutes: Time-to-live for fast unlock (default: 15, max: 1440)
   --    Scope: Read_Only or Full_Access (default: Read_Only)
   --    Verbose: Show detailed progress messages
   --
   --  Security guarantees:
   --    - Requires both Touch ID AND passphrase (never biometric alone)
   --    - Device secret stored in Secure Enclave via Keychain
   --    - Vault remains software-unlockable on other machines (Wrap A present)
   --    - TTL enforced at policy layer (re-authentication after expiry)
   --
   --  Error conditions:
   --    - Touch ID not available -> clear error, suggest passphrase-only
   --    - Keychain access denied -> prompt for System Preferences
   --    - Vault already enrolled -> show current status, offer re-enrollment
   --    - Policy violation -> explain why enrollment failed
   procedure Cmd_Enroll_Touch_ID
     (Vault_Path  : String;
      TTL_Minutes : Natural := 15;
      Scope       : String  := "read-only";
      Verbose     : Boolean := False);

   --  Test Touch ID availability and enrollment status
   --
   --  This command checks:
   --    1. LAContext.canEvaluatePolicy (hardware availability)
   --    2. Attempt test authentication (doesn't access vault)
   --    3. Query macOS Keychain for device secret
   --    4. Load vault policy to check enrollment status
   --    5. Display detailed status report
   --
   --  Output:
   --    Touch ID: Available / Not Available / Locked Out
   --    Keychain: Device secret present / missing
   --    Policy: Fast unlock enabled / disabled
   --    TTL: N minutes remaining (if enrolled)
   --    Scope: Read-only / Full access (if enrolled)
   --
   --  Parameters:
   --    Vault_Path: Path to .spass vault file (optional, for status check)
   --    Verbose: Show detailed diagnostic information
   procedure Cmd_Test_Touch_ID
     (Vault_Path : String  := "";
      Verbose    : Boolean := False);

   --  Unenroll Touch ID and remove device secret
   --
   --  This command performs the complete unenrollment workflow:
   --    1. Prompt for confirmation (requires passphrase unlock)
   --    2. Open vault with passphrase (verify user has access)
   --    3. Remove Wrap D from Key-Arena
   --    4. Delete device secret from macOS Keychain
   --    5. Update vault policy (disable fast unlock)
   --    6. Write updated header with ML-DSA signature
   --
   --  Security guarantees:
   --    - Requires passphrase to prove ownership before unenrollment
   --    - Vault remains unlockable with passphrase (Wrap A still present)
   --    - Device secret purged from Keychain (no residual data)
   --
   --  Parameters:
   --    Vault_Path: Path to .spass vault file
   --    Confirm: Skip confirmation prompt (for automation)
   --    Verbose: Show detailed progress messages
   procedure Cmd_Unenroll_Touch_ID
     (Vault_Path : String;
      Confirm    : Boolean := False;
      Verbose    : Boolean := False);

   --  Show device enrollment status
   --
   --  This command displays:
   --    - Enrollment status (enrolled, not enrolled, expired)
   --    - TTL information (time remaining until re-authentication)
   --    - Scope (read-only vs full access)
   --    - Touch ID hardware status
   --    - Keychain status (device secret present/missing)
   --    - Policy configuration
   --
   --  Parameters:
   --    Vault_Path: Path to .spass vault file
   --    JSON: Output in JSON format (for automation)
   --    Verbose: Show detailed diagnostic information
   procedure Cmd_Device_Status
     (Vault_Path : String;
      JSON       : Boolean := False;
      Verbose    : Boolean := False);

   --  Get enrollment status for a vault
   --
   --  Internal helper function used by other commands to check
   --  if Touch ID is currently enrolled and active.
   --
   --  Returns:
   --    Enrollment_Status indicating current state
   function Get_Enrollment_Status (Vault_Path : String) return Enrollment_Status;

   --  Check if Touch ID hardware is available
   --
   --  Uses LAContext to check if:
   --    - macOS LocalAuthentication framework is available
   --    - Touch ID or Face ID hardware is present
   --    - Biometric authentication can be evaluated
   --
   --  Returns:
   --    True if Touch ID is available, False otherwise
   function Is_Touch_ID_Available return Boolean;

   --  Get human-readable description of enrollment status
   --
   --  Converts Enrollment_Status enum to user-friendly string.
   --
   --  Returns:
   --    String like "Enrolled and active" or "Not enrolled"
   function Status_Description (Status : Enrollment_Status) return String;

end SparkPass.CLI.Device;
