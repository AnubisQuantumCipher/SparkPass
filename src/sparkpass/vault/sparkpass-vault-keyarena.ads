pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_8; use type Interfaces.Unsigned_32;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Wrapping;
with SparkPass.Crypto.Shamir;

--  Key-Arena: Multi-wrap Root Key storage structure
--
--  Binary layout (fixed-size for deterministic parsing):
--    Header (4 bytes):
--      - Magic: "KARN" (4 bytes)
--    Wrap A (Passphrase): 60 bytes
--      - Present flag (1 byte: 0x01 = present, 0x00 = absent)
--      - Wrapped key data (60 bytes: nonce + ciphertext + tag) [59 bytes padding if absent]
--    Wrap B (Recovery): 60 bytes
--      - Present flag (1 byte)
--      - Wrapped key data (60 bytes) [59 bytes padding if absent]
--    Wrap C-N (Shamir): Variable
--      - Share count (1 byte: 0-10)
--      - Threshold (1 byte: k value)
--      - For each share (if count > 0):
--        - Sealed share (33 + 12 + 16 = 61 bytes: share_data + nonce + tag)
--    Wrap D (Touch ID): 60 bytes
--      - Present flag (1 byte)
--      - Wrapped key data (60 bytes) [59 bytes padding if absent]
--
--  Total size (maximum):
--    4 (header) + 61 (A) + 61 (B) + 2 (shamir header) + 610 (10 shares * 61) + 61 (D) = 799 bytes
--
--  Policy enforcement:
--    - Wrap A (passphrase) is always required (Present = true)
--    - Wraps B, C-N, D are optional
--    - Wrap D requires Wrap A (never Touch ID alone)
--
package SparkPass.Vault.KeyArena is

   --  Key-Arena magic for binary format validation
   KeyArena_Magic : constant String := "KARN";

   --  Maximum size of Key-Arena in bytes
   KeyArena_Max_Size : constant Positive := 799;

   --  Key-Arena structure
   type Key_Arena is record
      --  Wrap A: Passphrase (required)
      Wrap_A_Present : Boolean := False;
      Wrap_A : SparkPass.Crypto.Wrapping.Wrapped_Key;

      --  Wrap B: Recovery words (optional)
      Wrap_B_Present : Boolean := False;
      Wrap_B : SparkPass.Crypto.Wrapping.Wrapped_Key;

      --  Wrap C-N: Shamir shares (optional)
      Shamir_Threshold    : Natural := 0;  -- k value (0 = not present)
      Shamir_Total_Shares : Natural := 0;  -- n value (0 = not present)
      Shamir_Shares : SparkPass.Crypto.Wrapping.Sealed_Share_Array (1 .. 10) :=
        (others => (Share_Data => (others => 0),
                    Nonce      => (others => 0),
                    Tag        => (others => 0)));

      --  Wrap D: Touch ID (optional, requires Wrap A)
      Wrap_D_Present : Boolean := False;
      Wrap_D : SparkPass.Crypto.Wrapping.Wrapped_Key;
   end record;

   --  Parse status for Key-Arena deserialization
   type Parse_Status is
     (Ok,
      Invalid_Magic,
      Invalid_Size,
      Invalid_Wrap_Data,
      Missing_Required_Wrap,  -- Wrap A not present
      Policy_Violation);      -- Touch ID without passphrase

   --  Serialize Key-Arena to byte array
   --
   --  Arena: Input Key-Arena structure
   --  Buffer: Output byte array (must be at least KeyArena_Max_Size)
   --  Actual_Size: Number of bytes written
   --  Status: Ok on success, error code otherwise
   --
   --  Post: On success, Buffer contains binary representation
   --        On failure, Buffer is zeroed
   procedure Serialize
     (Arena       : in     Key_Arena;
      Buffer      : out    Byte_Array;
      Actual_Size : out    Natural;
      Status      : out    Parse_Status)
   with
     Global => null,
     Pre    => Buffer'Length >= KeyArena_Max_Size,
     Post   => (if Status = Ok then
                  (Actual_Size > 0 and Actual_Size <= KeyArena_Max_Size)
                else
                  Actual_Size = 0);

   --  Deserialize Key-Arena from byte array (total parsing)
   --
   --  Buffer: Input byte array containing serialized Key-Arena
   --  Arena: Output Key-Arena structure
   --  Status: Ok on success, specific error code otherwise
   --
   --  Post: On success, Arena is fully populated
   --        On failure, Arena is zeroed and Status indicates error
   --
   --  Total parsing property: No partial state, single error path
   procedure Deserialize
     (Buffer : in     Byte_Array;
      Arena  : out    Key_Arena;
      Status : out    Parse_Status)
   with
     Global => null,
     Post   => (if Status /= Ok then
                  not Arena.Wrap_A_Present);

   --  Validate Key-Arena policy constraints
   --
   --  Checks:
   --    1. Wrap A (passphrase) is present (required)
   --    2. If Wrap D present, Wrap A must be present (Touch ID requires passphrase)
   --    3. Shamir configuration is valid (1 <= k <= n <= 10)
   --
   --  Returns: True if policy is satisfied, False otherwise
   function Is_Valid_Policy (Arena : Key_Arena) return Boolean
   with
     Global => null,
     Post   => Is_Valid_Policy'Result =
                 (Arena.Wrap_A_Present and then
                  (if Arena.Wrap_D_Present then Arena.Wrap_A_Present) and then
                  (if Arena.Shamir_Total_Shares > 0 then
                     (Arena.Shamir_Threshold > 0 and then
                      Arena.Shamir_Threshold <= Arena.Shamir_Total_Shares and then
                      Arena.Shamir_Total_Shares <= 10)));

   --  Zeroize Key-Arena
   --
   --  Wipes all wrapped keys and sensitive metadata
   --  Post: All fields are zeroed/cleared
   procedure Wipe_Arena (Arena : in out Key_Arena)
   with
     Global => null,
     Post   => not Arena.Wrap_A_Present and then
               not Arena.Wrap_B_Present and then
               not Arena.Wrap_D_Present and then
               Arena.Shamir_Threshold = 0 and then
               Arena.Shamir_Total_Shares = 0;

end SparkPass.Vault.KeyArena;
