pragma SPARK_Mode (On);
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Calendar;     use Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Interfaces;       use type Interfaces.Unsigned_64; use type Interfaces.Unsigned_8;
with Interfaces.C;
with Interfaces.C.Strings;
with SparkPass.Types;  use SparkPass.Types;
with SparkPass.Config;
with SparkPass.Crypto.LibOQS;
with SparkPass.Crypto.Self_Test;
with SparkPass.Crypto.Zeroize;
with SparkPass.Vault;
with SparkPass.Vault.Storage; use SparkPass.Vault.Storage;
with SparkPass.CLI.Password_Input;
with SparkPass.CLI.Device;
with SparkPass.Platform.Keychain;
with Bindings.LibOQS;

procedure Sparkpass_Main is

   --  Vault_Buffer: heap-allocated state buffer
   --  Supports up to Max_Entries without stack overflow
   type Vault_State_Access is access all SparkPass.Vault.Vault_State;
   procedure Free is new Ada.Unchecked_Deallocation (SparkPass.Vault.Vault_State, Vault_State_Access);

   Vault_Buffer : Vault_State_Access;

   function To_Bytes (S : String) return Byte_Array is
      subtype Range_Type is Positive range 1 .. (if S'Length = 0 then 1 else S'Length);
      Result : Byte_Array (Range_Type);
   begin
      if S'Length = 0 then
         Result (Result'First) := 0;
      else
         for Offset in 0 .. S'Length - 1 loop
            Result (Result'First + Offset) :=
              U8 (Character'Pos (S (S'First + Offset)) mod 256);
         end loop;
      end if;
      return Result;
   end To_Bytes;

   function To_String (Bytes : Byte_Array; Length : Natural) return String is
   begin
      if Length = 0 then
         return "";
      end if;

      declare
         Result : String (1 .. Length);
      begin
         for Offset in 0 .. Length - 1 loop
            Result (Offset + 1) :=
              Character'Val (Integer (Bytes (Bytes'First + Offset)));
         end loop;
         return Result;
      end;
   end To_String;

   function Timestamp return U64 is
      use Ada.Calendar;
      Now    : constant Time := Clock;
      Epoch  : constant Time := Time_Of (Year => 1970, Month => 1, Day => 1,
                                         Seconds => 0.0);
      Time_Delta  : constant Duration := Now - Epoch;
   begin
      if Time_Delta <= 0.0 then
         return 0;
      end if;

      declare
         Elapsed_Seconds : constant Long_Long_Integer := Long_Long_Integer (Time_Delta);
      begin
         if Elapsed_Seconds <= 0 then
            return 0;
         end if;
         return U64 (Interfaces.Unsigned_64 (Elapsed_Seconds));
      end;
   end Timestamp;

   procedure Usage is
   begin
      Put_Line ("SparkPass v1.0 - Hybrid Post-Quantum Password Vault");
      Put_Line ("");
      Put_Line ("USAGE:");
      Put_Line ("  sparkpass <command> [arguments]");
      Put_Line ("");
      Put_Line ("COMMANDS:");
      Put_Line ("  init <vault>                    Create new vault");
      Put_Line ("  add <vault> <label>             Add password/secret");
      Put_Line ("  get <vault> <label>             Retrieve password/secret");
      Put_Line ("  ls <vault>                      List all entries");
      Put_Line ("  rm <vault> <label>              Remove entry");
      Put_Line ("  rotate <vault>                  Rotate master key");
      Put_Line ("  export <vault>                  Export recovery key");
      Put_Line ("  import <vault> <recovery-file>  Restore master keys from recovery");
      Put_Line ("  doctor <vault>                  Inspect vault metadata");
      Put_Line ("  unlock <vault>                  Test vault password");
      Put_Line ("  device enroll <vault>           Enroll Touch ID for fast unlock");
      Put_Line ("  device test [<vault>]           Test Touch ID availability");
      Put_Line ("  device unenroll <vault>         Remove Touch ID enrollment");
      Put_Line ("  device status <vault>           Show device enrollment status");
      Put_Line ("  pqtest                          Run crypto self-tests (alias for self-test)");
      Put_Line ("  self-test [--comprehensive]     Run comprehensive self-tests");
      Put_Line ("            [--json]              Output JSON for CI/CD");
      Put_Line ("            [--verbose]           Show detailed test information");
      Put_Line ("");
      Put_Line ("EXAMPLES:");
      Put_Line ("  # Create vault");
      Put_Line ("  sparkpass init my_vault.spass");
      Put_Line ("");
      Put_Line ("  # Add GitHub token");
      Put_Line ("  sparkpass add my_vault.spass github");
      Put_Line ("");
      Put_Line ("  # Retrieve GitHub token");
      Put_Line ("  sparkpass get my_vault.spass github");
      Put_Line ("");
      Put_Line ("  # List all stored passwords");
      Put_Line ("  sparkpass ls my_vault.spass");
      Put_Line ("");
      Put_Line ("NOTES:");
      Put_Line ("  - Passwords must be at least 12 characters");
      Put_Line ("  - Vault files must have 0600 permissions (owner-only)");
      Put_Line ("  - Unlock takes ~2.5 seconds with password (Argon2id with 1 GiB memory)");
      Put_Line ("  - Touch ID (if available) bypasses Argon2id for instant unlock");
      Put_Line ("  - Biometric cache expires after 7 days (requires re-authentication)");
      Put_Line ("");
      Put_Line ("PASSWORD INPUT METHODS:");
      Put_Line ("  1. Interactive (default): Prompted securely with echo disabled");
      Put_Line ("     sparkpass unlock vault.spass");
      Put_Line ("");
      Put_Line ("  2. Environment variable (for automation):");
      Put_Line ("     export SPARKPASS_PASSWORD='my_secure_password'");
      Put_Line ("     sparkpass unlock vault.spass");
      Put_Line ("");
      Put_Line ("  3. Stdin pipe/redirect (for scripts):");
      Put_Line ("     echo 'my_secure_password' | sparkpass unlock vault.spass");
      Put_Line ("     sparkpass unlock vault.spass < password.txt");
      Put_Line ("");
      Put_Line ("  Security note: Environment variables can be seen in process listings.");
      Put_Line ("  Use stdin or interactive mode for better security.");
      Put_Line ("");
      Put_Line ("For more info: https://github.com/sicarii/sparkpass");
   end Usage;

   procedure Ensure_Vault_Allocated is
   begin
      if Vault_Buffer = null then
         Vault_Buffer := new SparkPass.Vault.Vault_State;
         SparkPass.Vault.Clear (Vault_Buffer.all);
      end if;
   end Ensure_Vault_Allocated;

begin
   --  Defer vault allocation until needed (lazy allocation)
   --  Some commands (help, pqtest, sizes) don't need the full vault buffer
   Vault_Buffer := null;

   if Argument_Count = 0 then
      Usage;
      return;
   end if;

   declare
      Cmd : constant String := Argument (1);
   begin
      --  Handle --help and --version flags
      if Cmd = "--help" or else Cmd = "-h" or else Cmd = "help" then
         Usage;
         return;
      elsif Cmd = "--version" or else Cmd = "-v" or else Cmd = "version" then
         Put_Line ("SparkPass version 1.0.0");
         Put_Line ("Post-quantum hybrid password vault");
         Put_Line ("Cryptography: ML-KEM-1024, ML-DSA-87, AES-256-GCM-SIV, Argon2id");
         return;
      elsif Cmd = "init" then
         if Argument_Count < 2 then
            Usage;
            return;
         end if;

         Ensure_Vault_Allocated;

         declare
            Path          : constant String := Argument (2);
            Password_Buf  : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Password_Len  : Natural;
            Confirm_Buf   : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Confirm_Len   : Natural;
            Read_Success  : Boolean;
            Password      : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Save_State    : SparkPass.Vault.Save_Status;
            Passwords_Match : Boolean := True;
         begin
            --  Read password
            SparkPass.CLI.Password_Input.Read_Password
              ("Enter password: ", Password_Buf, Password_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read password");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len = 0 then
               Put_Line ("✗ password must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len < SparkPass.Config.Min_Password_Length then
               Put_Line ("✗ password must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            --  Read password confirmation
            SparkPass.CLI.Password_Input.Read_Password
              ("Confirm password: ", Confirm_Buf, Confirm_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read confirmation");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               SparkPass.Crypto.Zeroize.Wipe (Confirm_Buf);
               return;
            end if;

            --  Check if passwords match (constant-time comparison)
            if Password_Len /= Confirm_Len then
               Passwords_Match := False;
            else
               Passwords_Match := SparkPass.Crypto.Zeroize.Equal(
                 Password_Buf(1 .. Password_Len),
                 Confirm_Buf(1 .. Confirm_Len));
            end if;

            --  Zeroize confirmation buffer immediately
            SparkPass.Crypto.Zeroize.Wipe (Confirm_Buf);

            if not Passwords_Match then
               Put_Line ("✗ passwords do not match");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            --  Copy password to working buffer
            for I in 0 .. Password_Len - 1 loop
               Password (Password'First + I) := Password_Buf (Password_Buf'First + I);
            end loop;

            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Vault.Create (Vault_Buffer.all, Password (1 .. Password_Len), Timestamp);
            SparkPass.Vault.Save (Vault_Buffer.all, Path, Save_State);
            case Save_State is
               when SparkPass.Vault.Saved =>
                  Put_Line ("✓ vault initialized at " & Path);
               when SparkPass.Vault.Io_Error =>
                  Put_Line ("✗ failed to save vault (I/O error)");
            end case;
            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Crypto.Zeroize.Wipe (Password);
            SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         end;

      elsif Cmd = "doctor" then
         if Argument_Count < 2 then
            Usage;
            return;
         end if;
        declare
           Path    : constant String := Argument (2);
           Header  : SparkPass.Types.Header;
           Entries : SparkPass.Types.Entry_Table;
           Count   : SparkPass.Types.Entry_Count_Type;
            Status  : SparkPass.Vault.Storage.Status;
         begin
            SparkPass.Vault.Storage.Load (Path, Header, Entries, Count, Status);
            case Status is
               when SparkPass.Vault.Storage.Ok =>
                  Put_Line ("Vault fingerprint: ");
                  declare
                     Hex : String (1 .. Header.Vault_Fingerprint'Length * 2);
                     Hex_Map : constant array (Interfaces.Unsigned_8 range 0 .. 15) of Character :=
                       ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
                  begin
                     for Index in Header.Vault_Fingerprint'Range loop
                        declare
                           Value : constant Interfaces.Unsigned_8 := Interfaces.Unsigned_8 (Header.Vault_Fingerprint (Index));
                           Pos   : constant Natural := (Index - Header.Vault_Fingerprint'First) * 2;
                        begin
                           Hex (Pos + 1) := Hex_Map (Interfaces.Unsigned_8 (Value / 16));
                           Hex (Pos + 2) := Hex_Map (Interfaces.Unsigned_8 (Value mod 16));
                        end;
                     end loop;
                     Put_Line (Hex);
                  end;
                  Put_Line ("Entries: " & Interfaces.Unsigned_32'Image (Header.Entry_Count));
               when SparkPass.Vault.Storage.Io_Error =>
                  Put_Line ("✗ failed to load vault (I/O error)");
               when SparkPass.Vault.Storage.Format_Error =>
                  Put_Line ("✗ failed to load vault (format error)");
               when SparkPass.Vault.Storage.Integrity_Error =>
                  Put_Line ("✗ failed to load vault (integrity error)");
               when SparkPass.Vault.Storage.Permission_Error =>
                  Put_Line ("✗ vault file has insecure permissions");
                  Put_Line ("  Expected: 0600 (owner read/write only)");
                  Put_Line ("  Fix: chmod 600 " & Path);
            end case;

            SparkPass.Crypto.Zeroize.Wipe (Header.Wrapped_Master_Key);
            SparkPass.Crypto.Zeroize.Wipe (Header.Wrapped_Master_Nonce);
            SparkPass.Crypto.Zeroize.Wipe (Header.Wrapped_Master_Tag);
            SparkPass.Crypto.Zeroize.Wipe (Header.Chain_Key_Value);
            SparkPass.Crypto.Zeroize.Wipe (Header.Chain_Key_Nonce);
            SparkPass.Crypto.Zeroize.Wipe (Header.Chain_Key_Tag);
            SparkPass.Crypto.Zeroize.Wipe (Header.MLDsa_Secret_Value);
            SparkPass.Crypto.Zeroize.Wipe (Header.MLDsa_Secret_Nonce);
            SparkPass.Crypto.Zeroize.Wipe (Header.MLDsa_Secret_Tag);
            SparkPass.Crypto.Zeroize.Wipe (Header.MLDsa_Public_Key);
            SparkPass.Crypto.Zeroize.Wipe (Header.MLKem_Public_Key);
            SparkPass.Crypto.Zeroize.Wipe (Header.Header_Signature);
            SparkPass.Crypto.Zeroize.Wipe (Header.Argon2_Salt);
            SparkPass.Crypto.Zeroize.Wipe (Header.Vault_Fingerprint);

            for Index in Entries'Range loop
               SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Ciphertext);
               SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Label);
               SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Signature);
               SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Nonce);
               SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Tag);
               SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Id);
            end loop;
         end;

      elsif Cmd = "unlock" then
         if Argument_Count < 2 then
            Usage;
            return;
         end if;

         Ensure_Vault_Allocated;

         declare
            Path         : constant String := Argument (2);
            Password_Buf : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Password_Len : Natural;
            Read_Success : Boolean;
            Password     : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Open_State   : SparkPass.Vault.Open_Status;
            Wrap_Key_Cached : Key_Array := (others => 0);
            Cache_Success : Boolean := False;
            Current_Time : constant U64 := Timestamp;
         begin
            SparkPass.Vault.Clear (Vault_Buffer.all);

            --  Try biometric unlock first
            SparkPass.Platform.Keychain.Retrieve_Wrap_Key
              (Wrap_Key_Cached, Path, Current_Time, Cache_Success);

            if Cache_Success then
               --  Attempt biometric unlock with cached wrap key
               SparkPass.Vault.Open_With_Key (Vault_Buffer.all, Path, Wrap_Key_Cached, Open_State);
               SparkPass.Crypto.Zeroize.Wipe (Wrap_Key_Cached);

               case Open_State is
                  when SparkPass.Vault.Success =>
                     Put_Line ("✓ unlocked with biometric authentication");
                     SparkPass.Vault.Clear (Vault_Buffer.all);
                     return;
                  when others =>
                     --  If biometric unlock failed, clear state and fall through to password
                     SparkPass.Vault.Clear (Vault_Buffer.all);
               end case;
            end if;

            --  Biometric unlock not available or failed - prompt for password
            SparkPass.CLI.Password_Input.Read_Password
              ("Enter password: ", Password_Buf, Password_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read password");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len = 0 then
               Put_Line ("✗ password must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len < SparkPass.Config.Min_Password_Length then
               Put_Line ("✗ password must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            for I in 0 .. Password_Len - 1 loop
               Password (Password'First + I) := Password_Buf (Password_Buf'First + I);
            end loop;

            SparkPass.Vault.Open (Vault_Buffer.all, Path, Password (1 .. Password_Len), Open_State);
            case Open_State is
               when SparkPass.Vault.Success =>
                  Put_Line ("✓ password accepted");

                  --  Cache wrap_key for future biometric unlock
                  SparkPass.Platform.Keychain.Store_Wrap_Key
                    (Vault_Buffer.all.Wrap_Key, Path, Current_Time, Cache_Success);

                  if Cache_Success then
                     Put_Line ("  (biometric unlock enabled for 7 days)");
                  end if;

                  SparkPass.Vault.Clear (Vault_Buffer.all);
               when SparkPass.Vault.Authentication_Failed =>
                  Put_Line ("✗ authentication failed");
               when SparkPass.Vault.Io_Error =>
                  Put_Line ("✗ failed to open vault (I/O error)");
               when SparkPass.Vault.Format_Error =>
                  Put_Line ("✗ failed to open vault (format error)");
               when SparkPass.Vault.Integrity_Error =>
                  Put_Line ("✗ failed to open vault (integrity error)");
            end case;
            SparkPass.Crypto.Zeroize.Wipe (Password);
            SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         end;

      elsif Cmd = "add" then
         if Argument_Count < 3 then
            Usage;
            return;
         end if;

         Ensure_Vault_Allocated;

         declare
            Path         : constant String := Argument (2);
            Label_Str    : constant String := Argument (3);
            Secret_Buf   : Byte_Array (1 .. SparkPass.Config.Max_Data_Length) := (others => 0);
            Secret_Len   : Natural;
            Password_Buf : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Password_Len : Natural;
            Read_Success : Boolean;
            Password     : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Label        : Byte_Array := To_Bytes (Label_Str);
            Open_State   : SparkPass.Vault.Open_Status;
            Added        : Boolean := False;
            Save_State   : SparkPass.Vault.Save_Status;
         begin
            if Label_Str'Length = 0 then
               Put_Line ("✗ label must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Label);
               SparkPass.Crypto.Zeroize.Wipe (Secret_Buf);
               return;
            end if;

            if Label_Str'Length > SparkPass.Config.Max_Label_Length then
               Put_Line ("✗ label too long (length:" & Natural'Image (Label_Str'Length) &
                         ", max:" & Natural'Image (SparkPass.Config.Max_Label_Length) & ")");
               SparkPass.Crypto.Zeroize.Wipe (Label);
               SparkPass.Crypto.Zeroize.Wipe (Secret_Buf);
               return;
            end if;

            --  Prompt for secret securely (not from command-line)
            SparkPass.CLI.Password_Input.Read_Password
              ("Enter secret: ", Secret_Buf, Secret_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read secret");
               SparkPass.Crypto.Zeroize.Wipe (Label);
               SparkPass.Crypto.Zeroize.Wipe (Secret_Buf);
               return;
            end if;

            if Secret_Len = 0 then
               Put_Line ("✗ secret must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Label);
               SparkPass.Crypto.Zeroize.Wipe (Secret_Buf);
               return;
            end if;

            if Secret_Len > SparkPass.Config.Max_Data_Length then
               Put_Line ("✗ secret too long (length:" & Natural'Image (Secret_Len) &
                         ", max:" & Natural'Image (SparkPass.Config.Max_Data_Length) & ")");
               SparkPass.Crypto.Zeroize.Wipe (Label);
               SparkPass.Crypto.Zeroize.Wipe (Secret_Buf);
               return;
            end if;

            SparkPass.CLI.Password_Input.Read_Password
              ("Enter password: ", Password_Buf, Password_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read password");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               SparkPass.Crypto.Zeroize.Wipe (Label);
               SparkPass.Crypto.Zeroize.Wipe (Secret_Buf);
               return;
            end if;

            if Password_Len = 0 then
                Put_Line ("✗ password must not be empty");
                SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
                SparkPass.Crypto.Zeroize.Wipe (Label);
                SparkPass.Crypto.Zeroize.Wipe (Secret_Buf);
                return;
            end if;

            if Password_Len < SparkPass.Config.Min_Password_Length then
               Put_Line ("✗ password must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               SparkPass.Crypto.Zeroize.Wipe (Label);
               SparkPass.Crypto.Zeroize.Wipe (Secret_Buf);
               return;
            end if;

            for I in 0 .. Password_Len - 1 loop
               Password (Password'First + I) := Password_Buf (Password_Buf'First + I);
            end loop;

            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Vault.Open (Vault_Buffer.all, Path, Password (1 .. Password_Len), Open_State);
            case Open_State is
               when SparkPass.Vault.Success =>
                  SparkPass.Vault.Add_Entry
                    (State     => Vault_Buffer.all,
                     Label     => Label,
                     Kind      => SparkPass.Types.Password,
                     Plaintext => Secret_Buf (1 .. Secret_Len),
                     Timestamp => Timestamp,
                     Success   => Added);
                  if Added then
                     SparkPass.Vault.Save (Vault_Buffer.all, Path, Save_State);
                     case Save_State is
                        when SparkPass.Vault.Saved =>
                           Put_Line ("✓ entry added");
                        when SparkPass.Vault.Io_Error =>
                           Put_Line ("✗ failed to save vault (I/O error)");
                     end case;
                  else
                     Put_Line ("✗ entry not added");
                  end if;
                  SparkPass.Vault.Clear (Vault_Buffer.all);
               when SparkPass.Vault.Authentication_Failed =>
                  Put_Line ("✗ authentication failed");
               when SparkPass.Vault.Io_Error =>
                  Put_Line ("✗ failed to open vault (I/O error)");
               when SparkPass.Vault.Format_Error =>
                  Put_Line ("✗ failed to open vault (format error)");
               when SparkPass.Vault.Integrity_Error =>
                  Put_Line ("✗ failed to open vault (integrity error)");
            end case;

            SparkPass.Crypto.Zeroize.Wipe (Password);
            SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
            SparkPass.Crypto.Zeroize.Wipe (Label);
            SparkPass.Crypto.Zeroize.Wipe (Secret_Buf);
         end;

      elsif Cmd = "get" then
         if Argument_Count < 3 then
            Usage;
            return;
         end if;

         Ensure_Vault_Allocated;

         declare
            Path         : constant String := Argument (2);
            Label_Str    : constant String := Argument (3);
            Password_Buf : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Password_Len : Natural;
            Read_Success : Boolean;
            Password     : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Label        : Byte_Array := To_Bytes (Label_Str);
            Open_State   : SparkPass.Vault.Open_Status;
            Plain        : Byte_Array (1 .. SparkPass.Config.Max_Data_Length) := (others => 0);
            Data_Len     : Natural := 0;
            Found        : Boolean := False;
         begin
            if Label_Str'Length = 0 then
               Put_Line ("✗ label must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            if Label_Str'Length > SparkPass.Config.Max_Label_Length then
               Put_Line ("✗ label too long (length:" & Natural'Image (Label_Str'Length) &
                         ", max:" & Natural'Image (SparkPass.Config.Max_Label_Length) & ")");
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            SparkPass.CLI.Password_Input.Read_Password
              ("Enter password: ", Password_Buf, Password_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read password");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            if Password_Len = 0 then
               Put_Line ("✗ password must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            if Password_Len < SparkPass.Config.Min_Password_Length then
               Put_Line ("✗ password must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            for I in 0 .. Password_Len - 1 loop
               Password (Password'First + I) := Password_Buf (Password_Buf'First + I);
            end loop;

            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Vault.Open (Vault_Buffer.all, Path, Password (1 .. Password_Len), Open_State);
            case Open_State is
               when SparkPass.Vault.Success =>
                  SparkPass.Vault.Get_Entry
                    (State     => Vault_Buffer.all,
                     Label     => Label,
                     Plaintext => Plain,
                     Data_Len  => Data_Len,
                     Success   => Found);
                  if Found then
                     declare
                        Secret : constant String := To_String (Plain, Data_Len);
                        Confirm : String (1 .. 256);
                        Last : Natural;
                     begin
                        Put ("WARNING: Secret will be printed to stdout. Continue? (y/N): ");
                        Flush;
                        Get_Line (Confirm, Last);

                        if Last > 0 and then (Confirm (1) = 'y' or else Confirm (1) = 'Y') then
                           Put_Line ("✓ " & Secret);
                        else
                           Put_Line ("Cancelled - secret not displayed");
                        end if;
                     end;
                  else
                     Put_Line ("✗ entry not found");
                  end if;
                  SparkPass.Vault.Clear (Vault_Buffer.all);
               when SparkPass.Vault.Authentication_Failed =>
                  Put_Line ("✗ authentication failed");
               when SparkPass.Vault.Io_Error =>
                  Put_Line ("✗ failed to open vault (I/O error)");
               when SparkPass.Vault.Format_Error =>
                  Put_Line ("✗ failed to open vault (format error)");
               when SparkPass.Vault.Integrity_Error =>
                  Put_Line ("✗ failed to open vault (integrity error)");
            end case;

            SparkPass.Crypto.Zeroize.Wipe (Password);
            SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
            SparkPass.Crypto.Zeroize.Wipe (Label);
            SparkPass.Crypto.Zeroize.Wipe (Plain);
         end;

      elsif Cmd = "ls" then
         if Argument_Count < 2 then
            Usage;
            return;
         end if;

         Ensure_Vault_Allocated;

         declare
            Path         : constant String := Argument (2);
            Password_Buf : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Password_Len : Natural;
            Read_Success : Boolean;
            Password     : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Open_State   : SparkPass.Vault.Open_Status;
         begin
            SparkPass.CLI.Password_Input.Read_Password
              ("Enter password: ", Password_Buf, Password_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read password");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len = 0 then
               Put_Line ("✗ password must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len < SparkPass.Config.Min_Password_Length then
               Put_Line ("✗ password must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            for I in 0 .. Password_Len - 1 loop
               Password (Password'First + I) := Password_Buf (Password_Buf'First + I);
            end loop;

            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Vault.Open (Vault_Buffer.all, Path, Password (1 .. Password_Len), Open_State);
            case Open_State is
               when SparkPass.Vault.Success =>
                  Put_Line ("Entries: " & Interfaces.Unsigned_32'Image (Vault_Buffer.all.Entry_Count));
                  for Index in 1 .. Natural (Vault_Buffer.all.Entry_Count) loop
                     declare
                        Vault_Entry : constant SparkPass.Types.Entry_Record := Vault_Buffer.all.Entries (Index);
                        Label_Length : constant Natural := Natural (Vault_Entry.Label_Len);
                        Label_Value  : constant String := To_String (Vault_Entry.Label, Label_Length);
                     begin
                        Put_Line ("  - " & Label_Value &
                                  " [" & SparkPass.Types.Entry_Type'Image (Vault_Entry.Kind) & "]");
                     end;
                  end loop;
                  SparkPass.Vault.Clear (Vault_Buffer.all);
               when SparkPass.Vault.Authentication_Failed =>
                  Put_Line ("✗ authentication failed");
               when SparkPass.Vault.Io_Error =>
                  Put_Line ("✗ failed to open vault (I/O error)");
               when SparkPass.Vault.Format_Error =>
                  Put_Line ("✗ failed to open vault (format error)");
               when SparkPass.Vault.Integrity_Error =>
                  Put_Line ("✗ failed to open vault (integrity error)");
            end case;

            SparkPass.Crypto.Zeroize.Wipe (Password);
            SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         end;

      elsif Cmd = "rm" then
         if Argument_Count < 3 then
            Usage;
            return;
         end if;

         Ensure_Vault_Allocated;

        declare
           Path         : constant String := Argument (2);
            Label_Str    : constant String := Argument (3);
            Password_Buf : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Password_Len : Natural;
            Read_Success : Boolean;
            Password     : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Label        : Byte_Array := To_Bytes (Label_Str);
            Open_State   : SparkPass.Vault.Open_Status;
            Removed      : Boolean := False;
            Save_State   : SparkPass.Vault.Save_Status;
         begin
            if Label_Str'Length = 0 then
               Put_Line ("✗ label must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            if Label_Str'Length > SparkPass.Config.Max_Label_Length then
               Put_Line ("✗ label too long (length:" & Natural'Image (Label_Str'Length) &
                         ", max:" & Natural'Image (SparkPass.Config.Max_Label_Length) & ")");
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            SparkPass.CLI.Password_Input.Read_Password
              ("Enter password: ", Password_Buf, Password_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read password");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            if Password_Len = 0 then
               Put_Line ("✗ password must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            if Password_Len < SparkPass.Config.Min_Password_Length then
               Put_Line ("✗ password must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               SparkPass.Crypto.Zeroize.Wipe (Label);
               return;
            end if;

            for I in 0 .. Password_Len - 1 loop
               Password (Password'First + I) := Password_Buf (Password_Buf'First + I);
            end loop;

            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Vault.Open (Vault_Buffer.all, Path, Password (1 .. Password_Len), Open_State);
            case Open_State is
               when SparkPass.Vault.Success =>
                  SparkPass.Vault.Remove_Entry
                    (State     => Vault_Buffer.all,
                     Label     => Label,
                     Timestamp => Timestamp,
                     Success   => Removed);
                  if Removed then
                     SparkPass.Vault.Save (Vault_Buffer.all, Path, Save_State);
                     case Save_State is
                        when SparkPass.Vault.Saved =>
                           Put_Line ("✓ entry removed");
                        when SparkPass.Vault.Io_Error =>
                           Put_Line ("✗ failed to save vault (I/O error)");
                     end case;
                  else
                     Put_Line ("✗ entry not found");
                  end if;
                  SparkPass.Vault.Clear (Vault_Buffer.all);
               when SparkPass.Vault.Authentication_Failed =>
                  Put_Line ("✗ authentication failed");
               when SparkPass.Vault.Io_Error =>
                  Put_Line ("✗ failed to open vault (I/O error)");
               when SparkPass.Vault.Format_Error =>
                  Put_Line ("✗ failed to open vault (format error)");
               when SparkPass.Vault.Integrity_Error =>
                  Put_Line ("✗ failed to open vault (integrity error)");
            end case;

            SparkPass.Crypto.Zeroize.Wipe (Password);
            SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
            SparkPass.Crypto.Zeroize.Wipe (Label);
         end;

      elsif Cmd = "rotate" then
         if Argument_Count < 2 then
            Usage;
            return;
         end if;

         Ensure_Vault_Allocated;

         declare
            Path         : constant String := Argument (2);
            Password_Buf : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Password_Len : Natural;
            Read_Success : Boolean;
            Password     : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Open_State   : SparkPass.Vault.Open_Status;
            Rotated      : Boolean := False;
            Save_State   : SparkPass.Vault.Save_Status;
         begin
            SparkPass.CLI.Password_Input.Read_Password
              ("Enter password: ", Password_Buf, Password_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read password");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len = 0 then
               Put_Line ("✗ password must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len < SparkPass.Config.Min_Password_Length then
               Put_Line ("✗ password must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            for I in 0 .. Password_Len - 1 loop
               Password (Password'First + I) := Password_Buf (Password_Buf'First + I);
            end loop;

            Put_Line ("Rotating master key...");
            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Vault.Open (Vault_Buffer.all, Path, Password (1 .. Password_Len), Open_State);
            case Open_State is
               when SparkPass.Vault.Success =>
                  SparkPass.Vault.Rotate_Master_Key
                    (State     => Vault_Buffer.all,
                     Timestamp => Timestamp,
                     Success   => Rotated);
                  if Rotated then
                     Put_Line ("  - Generating new master key... ✓");
                     Put_Line ("  - Re-encrypting all entries (" &
                              Interfaces.Unsigned_32'Image (Vault_Buffer.all.Entry_Count) & ")... ✓");
                     Put_Line ("  - Updating chain key... ✓");
                     SparkPass.Vault.Save (Vault_Buffer.all, Path, Save_State);
                     case Save_State is
                        when SparkPass.Vault.Saved =>
                           Put_Line ("  - Signing vault... ✓");
                           Put_Line ("✓ Master key rotated");
                           Put_Line ("✓ Vault saved");
                        when SparkPass.Vault.Io_Error =>
                           Put_Line ("✗ failed to save vault (I/O error)");
                     end case;
                  else
                     Put_Line ("✗ key rotation failed");
                  end if;
                  SparkPass.Vault.Clear (Vault_Buffer.all);
               when SparkPass.Vault.Authentication_Failed =>
                  Put_Line ("✗ authentication failed");
               when SparkPass.Vault.Io_Error =>
                  Put_Line ("✗ failed to open vault (I/O error)");
               when SparkPass.Vault.Format_Error =>
                  Put_Line ("✗ failed to open vault (format error)");
               when SparkPass.Vault.Integrity_Error =>
                  Put_Line ("✗ failed to open vault (integrity error)");
            end case;

            SparkPass.Crypto.Zeroize.Wipe (Password);
            SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         end;

      elsif Cmd = "export" then
         if Argument_Count < 2 then
            Usage;
            return;
         end if;

         Ensure_Vault_Allocated;

         declare
            Path           : constant String := Argument (2);
            Password_Buf   : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Password_Len   : Natural;
            Read_Success   : Boolean;
            Password       : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Open_State     : SparkPass.Vault.Open_Status;
            Export_Success : Boolean := False;
            Recovery_Path  : constant String := Path & ".recovery";
         begin
            SparkPass.CLI.Password_Input.Read_Password
              ("Enter password: ", Password_Buf, Password_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read password");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len = 0 then
               Put_Line ("✗ password must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len < SparkPass.Config.Min_Password_Length then
               Put_Line ("✗ password must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            for I in 0 .. Password_Len - 1 loop
               Password (Password'First + I) := Password_Buf (Password_Buf'First + I);
            end loop;

            Put_Line ("Generating recovery share with ML-KEM-1024...");
            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Vault.Open (Vault_Buffer.all, Path, Password (1 .. Password_Len), Open_State);
            case Open_State is
               when SparkPass.Vault.Success =>
                  SparkPass.Vault.Export_Recovery
                    (State         => Vault_Buffer.all,
                     Recovery_Path => Recovery_Path,
                     Success       => Export_Success);
                  if Export_Success then
                     Put_Line ("✓ Recovery share created: " & Recovery_Path);
                     Put_Line ("");
                     Put_Line ("IMPORTANT:");
                     Put_Line ("  1. Store recovery file in a safe place");
                     Put_Line ("  2. Test recovery before relying on it");
                     Put_Line ("  3. Keep recovery file secure (contains ML-KEM secret key)");
                  else
                     Put_Line ("✗ failed to create recovery share");
                  end if;
                  SparkPass.Vault.Clear (Vault_Buffer.all);
               when SparkPass.Vault.Authentication_Failed =>
                  Put_Line ("✗ authentication failed");
               when SparkPass.Vault.Io_Error =>
                  Put_Line ("✗ failed to open vault (I/O error)");
               when SparkPass.Vault.Format_Error =>
                  Put_Line ("✗ failed to open vault (format error)");
               when SparkPass.Vault.Integrity_Error =>
                  Put_Line ("✗ failed to open vault (integrity error)");
            end case;

            SparkPass.Crypto.Zeroize.Wipe (Password);
            SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         end;

      elsif Cmd = "import" then
         if Argument_Count < 3 then
            Usage;
            return;
         end if;

         Ensure_Vault_Allocated;

         declare
            Vault_Path     : constant String := Argument (2);
            Recovery_Path  : constant String := Argument (3);
            Password_Buf   : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Password_Len   : Natural;
            Read_Success   : Boolean;
            Password       : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
            Import_Success : Boolean := False;
         begin
            SparkPass.CLI.Password_Input.Read_Password
              ("Enter password: ", Password_Buf, Password_Len, Read_Success);

            if not Read_Success then
               Put_Line ("✗ failed to read password");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len = 0 then
               Put_Line ("✗ password must not be empty");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            if Password_Len < SparkPass.Config.Min_Password_Length then
               Put_Line ("✗ password must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters");
               SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
               return;
            end if;

            for I in 0 .. Password_Len - 1 loop
               Password (Password'First + I) := Password_Buf (Password_Buf'First + I);
            end loop;

            Put_Line ("Reading recovery share from: " & Recovery_Path);
            Put_Line ("Target vault: " & Vault_Path);
            Put_Line ("");
            Put_Line ("Note: The vault must exist for recovery to work.");
            Put_Line ("      Recovery restores the master keys from the recovery file.");
            Put_Line ("");

            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Vault.Import_Recovery
              (Vault_Path    => Vault_Path,
               Recovery_Path => Recovery_Path,
               Password      => Password (1 .. Password_Len),
               State         => Vault_Buffer.all,
               Success       => Import_Success);

            if Import_Success then
               Put_Line ("✓ Vault recovered successfully!");
               Put_Line ("");
               Put_Line ("The master keys have been restored from the recovery file.");
               Put_Line ("You can now access your vault entries normally.");
            else
               Put_Line ("✗ Failed to import recovery share");
               Put_Line ("");
               Put_Line ("Possible causes:");
               Put_Line ("  1. Vault file does not exist at: " & Vault_Path);
               Put_Line ("  2. Recovery file does not exist at: " & Recovery_Path);
               Put_Line ("  3. Wrong password (must match vault creation password)");
               Put_Line ("  4. Corrupted recovery file");
               Put_Line ("  5. Vault file has insecure permissions (must be 0600)");
               Put_Line ("");
               Put_Line ("Verify that both files exist and have correct permissions:");
               Put_Line ("  ls -la " & Vault_Path);
               Put_Line ("  ls -la " & Recovery_Path);
            end if;

            SparkPass.Vault.Clear (Vault_Buffer.all);
            SparkPass.Crypto.Zeroize.Wipe (Password);
            SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         end;

      elsif Cmd = "sizes" then
         --  Diagnostic: Show actual liboqs sizes
         declare
            use Interfaces.C;
            use Interfaces.C.Strings;
            use type Bindings.LibOQS.Kem_Handle;
            use type Bindings.LibOQS.Sig_Handle;
            Kem_Name_C : chars_ptr := New_String ("ML-KEM-1024");
            Sig_Name_C : chars_ptr := New_String ("ML-DSA-87");
            Kem : Bindings.LibOQS.Kem_Handle;
            Sig : Bindings.LibOQS.Sig_Handle;
         begin
            Kem := Bindings.LibOQS.OQS_KEM_New (Kem_Name_C);
            if Kem /= null then
               Put_Line ("ML-KEM-1024 actual sizes (from liboqs):");
               Put_Line ("  Public key: " & Natural'Image (Natural (size_t'Pos (Kem.all.Length_Public_Key))) & " bytes");
               Put_Line ("  Secret key: " & Natural'Image (Natural (size_t'Pos (Kem.all.Length_Secret_Key))) & " bytes");
               Put_Line ("  Ciphertext: " & Natural'Image (Natural (size_t'Pos (Kem.all.Length_Ciphertext))) & " bytes");
               Put_Line ("  Shared sec: " & Natural'Image (Natural (size_t'Pos (Kem.all.Length_Shared_Secret))) & " bytes");
               Bindings.LibOQS.OQS_KEM_Free (Kem);
            else
               Put_Line ("✗ ML-KEM-1024 not available");
            end if;

            Put_Line ("");
            Sig := Bindings.LibOQS.OQS_SIG_New (Sig_Name_C);
            if Sig /= null then
               Put_Line ("ML-DSA-87 actual sizes (from liboqs):");
               Put_Line ("  Public key: " & Natural'Image (Natural (size_t'Pos (Sig.all.Length_Public_Key))) & " bytes");
               Put_Line ("  Secret key: " & Natural'Image (Natural (size_t'Pos (Sig.all.Length_Secret_Key))) & " bytes");
               Put_Line ("  Signature:  " & Natural'Image (Natural (size_t'Pos (Sig.all.Length_Signature))) & " bytes");
               Bindings.LibOQS.OQS_SIG_Free (Sig);
            else
               Put_Line ("✗ ML-DSA-87 not available");
            end if;

            Put_Line ("");
            Put_Line ("SparkPass.Config constants:");
            Put_Line ("  ML-KEM-1024 public:  " & Positive'Image (SparkPass.Config.MLKem_Public_Key_Length));
            Put_Line ("  ML-KEM-1024 secret:  " & Positive'Image (SparkPass.Config.MLKem_Secret_Key_Length));
            Put_Line ("  ML-KEM-1024 cipher:  " & Positive'Image (SparkPass.Config.MLKem_Ciphertext_Length));
            Put_Line ("  ML-KEM-1024 shared:  " & Positive'Image (SparkPass.Config.MLKem_Shared_Key_Length));
            Put_Line ("  ML-DSA-87 public:    " & Positive'Image (SparkPass.Config.MLDsa_Public_Key_Length));
            Put_Line ("  ML-DSA-87 secret:    " & Positive'Image (SparkPass.Config.MLDsa_Secret_Key_Length));
            Put_Line ("  ML-DSA-87 signature: " & Positive'Image (SparkPass.Config.MLDsa_Signature_Length));

            Free (Kem_Name_C);
            Free (Sig_Name_C);
         end;

      elsif Cmd = "device" then
         --  Device management subcommand dispatcher
         if Argument_Count < 2 then
            Put_Line ("Error: device command requires a subcommand");
            Put_Line ("");
            Put_Line ("Usage:");
            Put_Line ("  sparkpass device enroll <vault>      Enroll Touch ID");
            Put_Line ("  sparkpass device test [<vault>]      Test Touch ID");
            Put_Line ("  sparkpass device unenroll <vault>    Remove Touch ID");
            Put_Line ("  sparkpass device status <vault>      Show status");
            return;
         end if;

         declare
            SubCmd : constant String := Argument (2);
         begin
            if SubCmd = "enroll" then
               if Argument_Count < 3 then
                  Put_Line ("Error: 'device enroll' requires vault path");
                  Put_Line ("Usage: sparkpass device enroll <vault> [--ttl <minutes>] [--scope read-only|full]");
                  return;
               end if;

               declare
                  Vault_Path : constant String := Argument (3);
                  TTL : Natural := 15;  -- Default 15 minutes
                  Scope_Str : String (1 .. 20) := (others => ' ');
                  Scope_Len : Natural := 9;  -- "read-only"
                  Verbose : Boolean := False;
               begin
                  --  Initialize with default scope
                  Scope_Str (1 .. 9) := "read-only";

                  --  Parse optional arguments
                  for I in 4 .. Argument_Count loop
                     declare
                        Arg : constant String := Argument (I);
                     begin
                        if Arg = "--ttl" and I < Argument_Count then
                           TTL := Natural'Value (Argument (I + 1));
                        elsif Arg = "--scope" and I < Argument_Count then
                           declare
                              New_Scope : constant String := Argument (I + 1);
                           begin
                              Scope_Len := New_Scope'Length;
                              Scope_Str (1 .. Scope_Len) := New_Scope;
                           end;
                        elsif Arg = "--verbose" or Arg = "-v" then
                           Verbose := True;
                        end if;
                     end;
                  end loop;

                  SparkPass.CLI.Device.Cmd_Enroll_Touch_ID (Vault_Path, TTL, Scope_Str (1 .. Scope_Len), Verbose);
               end;

            elsif SubCmd = "test" then
               declare
                  Vault_Path : constant String := (if Argument_Count >= 3 then Argument (3) else "");
                  Verbose : Boolean := False;
               begin
                  for I in 4 .. Argument_Count loop
                     if Argument (I) = "--verbose" or Argument (I) = "-v" then
                        Verbose := True;
                     end if;
                  end loop;

                  SparkPass.CLI.Device.Cmd_Test_Touch_ID (Vault_Path, Verbose);
               end;

            elsif SubCmd = "unenroll" then
               if Argument_Count < 3 then
                  Put_Line ("Error: 'device unenroll' requires vault path");
                  Put_Line ("Usage: sparkpass device unenroll <vault> [--confirm]");
                  return;
               end if;

               declare
                  Vault_Path : constant String := Argument (3);
                  Confirm : Boolean := False;
                  Verbose : Boolean := False;
               begin
                  for I in 4 .. Argument_Count loop
                     declare
                        Arg : constant String := Argument (I);
                     begin
                        if Arg = "--confirm" then
                           Confirm := True;
                        elsif Arg = "--verbose" or Arg = "-v" then
                           Verbose := True;
                        end if;
                     end;
                  end loop;

                  SparkPass.CLI.Device.Cmd_Unenroll_Touch_ID (Vault_Path, Confirm, Verbose);
               end;

            elsif SubCmd = "status" then
               if Argument_Count < 3 then
                  Put_Line ("Error: 'device status' requires vault path");
                  Put_Line ("Usage: sparkpass device status <vault> [--json]");
                  return;
               end if;

               declare
                  Vault_Path : constant String := Argument (3);
                  JSON : Boolean := False;
                  Verbose : Boolean := False;
               begin
                  for I in 4 .. Argument_Count loop
                     declare
                        Arg : constant String := Argument (I);
                     begin
                        if Arg = "--json" then
                           JSON := True;
                        elsif Arg = "--verbose" or Arg = "-v" then
                           Verbose := True;
                        end if;
                     end;
                  end loop;

                  SparkPass.CLI.Device.Cmd_Device_Status (Vault_Path, JSON, Verbose);
               end;

            else
               Put_Line ("Error: unknown device subcommand '" & SubCmd & "'");
               Put_Line ("");
               Put_Line ("Available subcommands:");
               Put_Line ("  enroll    - Enroll Touch ID for fast unlock");
               Put_Line ("  test      - Test Touch ID availability");
               Put_Line ("  unenroll  - Remove Touch ID enrollment");
               Put_Line ("  status    - Show device enrollment status");
            end if;
         end;

      elsif Cmd = "pqtest" or else Cmd = "self-test" then
        declare
           LibOQS_OK : constant Boolean := SparkPass.Crypto.LibOQS.Self_Test;
           Report    : SparkPass.Crypto.Self_Test.Report;
           Test_Mode : SparkPass.Crypto.Self_Test.Test_Mode := SparkPass.Crypto.Self_Test.Fast;
           Verbose   : Boolean := False;
           Json_Mode : Boolean := False;

           use type SparkPass.Crypto.Self_Test.Stage_Status;
           use type SparkPass.Crypto.Self_Test.Tamper_Status;

            function Bool_Icon (Value : Boolean) return String is
              (if Value then "✓" else "✗");

            function Stage_Icon (Status : SparkPass.Crypto.Self_Test.Stage_Status) return String is
              (if Status = SparkPass.Crypto.Self_Test.Succeeded then "✓"
               elsif Status = SparkPass.Crypto.Self_Test.Skipped then "⊙"
               else "✗");

            function Tamper_Image (Status : SparkPass.Crypto.Self_Test.Tamper_Status) return String is
              (if Status = SparkPass.Crypto.Self_Test.Detected then "✓ detected" else "✗ not detected");

            function Duration_Image (Value : Duration) return String is
               Raw   : constant String := Duration'Image (Value);
               First : Positive := Raw'First;
            begin
               while First <= Raw'Last and then Raw (First) = ' ' loop
                  First := First + 1;
               end loop;
               return Raw (First .. Raw'Last) & " s";
            end Duration_Image;
         begin
            --  Parse command-line flags
            for I in 2 .. Argument_Count loop
               declare
                  Arg : constant String := Argument (I);
               begin
                  if Arg = "--comprehensive" then
                     Test_Mode := SparkPass.Crypto.Self_Test.Comprehensive;
                  elsif Arg = "--benchmark" or Arg = "--bench" then
                     Test_Mode := SparkPass.Crypto.Self_Test.Benchmark;
                  elsif Arg = "--verbose" or Arg = "-v" then
                     Verbose := True;
                  elsif Arg = "--json" then
                     Json_Mode := True;
                  end if;
               end;
            end loop;

            --  Run tests
            SparkPass.Crypto.Self_Test.Run (Report, Test_Mode);

            --  Output results
            if Json_Mode then
               --  JSON output for CI/CD integration
               Put_Line ("{");
               Put_Line ("  ""sparkpass_version"": ""1.0.0"",");
               Put_Line ("  ""test_mode"": """ & SparkPass.Crypto.Self_Test.Test_Mode'Image (Test_Mode) & """,");
               Put_Line ("  ""timestamp"": """ & U64'Image (Timestamp) & """,");
               Put_Line ("  ""system"": {");
               Put_Line ("    ""os"": ""macOS"",");
               Put_Line ("    ""arch"": ""arm64""");
               Put_Line ("  },");
               Put_Line ("  ""results"": {");
               Put_Line ("    ""passed"": " & Boolean'Image (LibOQS_OK and SparkPass.Crypto.Self_Test.Passed (Report)) & ",");
               Put_Line ("    ""duration_seconds"": " & Duration_Image (Report.Total_Duration));
               Put_Line ("  },");
               Put_Line ("  ""tests"": {");
               Put_Line ("    ""liboqs"": " & Boolean'Image (LibOQS_OK) & ",");
               Put_Line ("    ""argon2id"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.Argon2_Status) & """,");
               Put_Line ("    ""hkdf"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.HKDF_Status) & """,");
               Put_Line ("    ""aes_gcm_siv"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.AES_Status) & """,");
               Put_Line ("    ""ml_kem"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.MLKEM_Status) & """,");
               Put_Line ("    ""ml_dsa"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.MLDSA_Status) & """,");
               Put_Line ("    ""random"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.Random_Status) & """,");
               Put_Line ("    ""shamir"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.Shamir_Status) & """,");
               Put_Line ("    ""reed_solomon"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.ReedSolomon_Status) & """,");
               Put_Line ("    ""nonce"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.Nonce_Status) & """,");
               Put_Line ("    ""wrapping"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.Wrapping_Status) & """,");
               Put_Line ("    ""zeroization"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.Zeroization_Status) & """,");
               Put_Line ("    ""key_arena"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.KeyArena_Status) & """,");
               Put_Line ("    ""policy"": """ & SparkPass.Crypto.Self_Test.Stage_Status'Image (Report.Policy_Status) & """");
               Put_Line ("  }");
               Put_Line ("}");
            else
               --  Human-readable output
               Put_Line ("");
               Put_Line ("SparkPass v1.0 Self-Test");
               Put_Line ("================================================================================");
               Put_Line ("");

               if LibOQS_OK and SparkPass.Crypto.Self_Test.Passed (Report) then
                  Put_Line ("✓ ALL TESTS PASSED");
               else
                  Put_Line ("✗ TESTS FAILED");
               end if;

               Put_Line ("");
               Put_Line ("[1/4] Cryptographic Primitives");
               Put_Line ("");
               Put_Line ("  " & Stage_Icon (Report.Argon2_Status) & " Argon2id KDF                [" &
                         Duration_Image (Report.Argon2_Duration) &
                         (if Report.Argon2_Used_Strong_Params then "" else ", reduced params") & "]");
               Put_Line ("  " & Stage_Icon (Report.HKDF_Status) & " HKDF-SHA-384");
               Put_Line ("  " & Stage_Icon (Report.AES_Status) & " AES-256-GCM-SIV");
               Put_Line ("  " & Stage_Icon (Report.MLKEM_Status) & " ML-KEM-1024");
               Put_Line ("  " & Stage_Icon (Report.MLDSA_Status) & " ML-DSA-87");
               Put_Line ("  " & Stage_Icon (Report.Random_Status) & " Random (CSPRNG)");
               Put_Line ("    Tamper detection: " & Tamper_Image (Report.MLDSA_Tamper));

               Put_Line ("");
               Put_Line ("[2/4] SparkPass Cryptography");
               Put_Line ("");
               Put_Line ("  " & Stage_Icon (Report.Shamir_Status) & " Shamir Secret Sharing");
               Put_Line ("  " & Stage_Icon (Report.ReedSolomon_Status) & " Reed-Solomon FEC");
               Put_Line ("  " & Stage_Icon (Report.Nonce_Status) & " Nonce Derivation");
               Put_Line ("  " & Stage_Icon (Report.Wrapping_Status) & " Key Wrapping");
               Put_Line ("  " & Stage_Icon (Report.Zeroization_Status) & " Zeroization");

               Put_Line ("");
               Put_Line ("[3/4] Vault Operations");
               Put_Line ("");
               Put_Line ("  " & Stage_Icon (Report.KeyArena_Status) & " Key-Arena");
               Put_Line ("  " & Stage_Icon (Report.Policy_Status) & " Policy Engine");

               Put_Line ("");
               Put_Line ("[4/4] Platform Integration");
               Put_Line ("");
               Put_Line ("  " & Stage_Icon (Report.Platform_Status) & " Platform-specific tests");

               Put_Line ("");
               Put_Line ("================================================================================");
               Put_Line ("Total Duration: " & Duration_Image (Report.Total_Duration));
               Put_Line ("LibOQS: " & Bool_Icon (LibOQS_OK));

               if Verbose then
                  Put_Line ("");
                  Put_Line ("Legend: ✓ Passed  ✗ Failed  ⊙ Skipped");
                  Put_Line ("");
                  Put_Line ("To run comprehensive tests (including Reed-Solomon):");
                  Put_Line ("  sparkpass self-test --comprehensive");
                  Put_Line ("");
                  Put_Line ("For JSON output (CI/CD integration):");
                  Put_Line ("  sparkpass self-test --json");
               end if;
            end if;

            --  Exit with failure code if tests didn't pass
            if not (LibOQS_OK and SparkPass.Crypto.Self_Test.Passed (Report)) then
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            end if;
         end;

      else
         Put_Line ("unknown command: " & Cmd);
         Usage;
      end if;
   end;

   --  Clean up heap-allocated vault buffer
   if Vault_Buffer /= null then
      SparkPass.Vault.Clear (Vault_Buffer.all);
      Free (Vault_Buffer);
   end if;

exception
   when others =>
      --  Ensure cleanup even on exceptions
      if Vault_Buffer /= null then
         SparkPass.Vault.Clear (Vault_Buffer.all);
         Free (Vault_Buffer);
      end if;
      raise;
end Sparkpass_Main;
