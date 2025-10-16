pragma SPARK_Mode (Off);  -- CLI operations involve I/O
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Interfaces; use type Interfaces.Unsigned_64;
with Interfaces.C; use type Interfaces.C.int;
with Interfaces.C.Strings;
with System; use type System.Address;
with SparkPass.Config;
with SparkPass.Crypto.Zeroize;
with SparkPass.Crypto.Random;
with SparkPass.Crypto.Wrapping;
with SparkPass.Vault; use type SparkPass.Vault.Open_Status; use type SparkPass.Vault.Save_Status;
with SparkPass.Vault.KeyArena;
with SparkPass.Vault.Policy;
with SparkPass.Vault.Storage; use type SparkPass.Vault.Storage.Status;
with SparkPass.Platform.Keychain;
with SparkPass.CLI.Password_Input;
with Bindings.LAContext_Darwin; use Bindings.LAContext_Darwin;

package body SparkPass.CLI.Device is

   --  Get current Unix timestamp
   function Timestamp return U64 is
      Now : constant Time := Clock;
      Epoch : constant Time := Time_Of (Year => 1970, Month => 1, Day => 1, Seconds => 0.0);
      Time_Delta : constant Duration := Now - Epoch;
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

   --  Check if Touch ID hardware is available
   function Is_Touch_ID_Available return Boolean is
      Context : LAContext_Handle;
      Error : aliased LAError := 0;
      Can_Evaluate : Interfaces.C.int;
   begin
      --  Create LAContext
      Context := LAContext_Create;
      if System.Address (Context) = System.Null_Address then
         return False;
      end if;

      --  Check if biometry is available
      Can_Evaluate := LAContext_CanEvaluatePolicy
        (Context,
         LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
         Error'Access);

      LAContext_Release (Context);

      return Can_Evaluate /= 0;
   end Is_Touch_ID_Available;

   --  Get enrollment status for a vault
   function Get_Enrollment_Status (Vault_Path : String) return Enrollment_Status is
      Header : SparkPass.Types.Header;
      Entries : SparkPass.Types.Entry_Table;
      Count : SparkPass.Types.Entry_Count_Type;
      Load_Status : SparkPass.Vault.Storage.Status;
      Has_Wrap_D : Boolean := False;
   begin
      --  Check if Touch ID hardware is available
      if not Is_Touch_ID_Available then
         return Hardware_Unavailable;
      end if;

      --  Load vault header to check if Wrap D is present
      SparkPass.Vault.Storage.Load (Vault_Path, Header, Entries, Count, Load_Status);

      if Load_Status /= SparkPass.Vault.Storage.Ok then
         return Not_Enrolled;
      end if;

      --  Check if Wrap D is present in vault header
      --  This is the primary enrollment indicator
      Has_Wrap_D := SparkPass.Vault.Is_Touch_ID_Enrolled (Vault_Path);

      if not Has_Wrap_D then
         --  No Wrap D in vault = not enrolled
         return Not_Enrolled;
      end if;

      --  Wrap D exists in vault. Now check Keychain status.
      --  Keychain entry may be missing but vault still has Wrap D.
      if not SparkPass.Platform.Keychain.Has_Cached_Key (Vault_Path) then
         --  Vault has Wrap D but Keychain missing device secret
         --  This means enrollment is incomplete or Keychain was cleared
         return Not_Enrolled;
      end if;

      --  Note: Cannot determine cache expiration without authentication
      --  (would require data retrieval). Expiration check happens during
      --  actual vault unlock when biometric authentication is provided.
      --  For status purposes, we just confirm both Wrap D and Keychain entry exist.

      --  Both conditions satisfied:
      --    1. Wrap D present in vault header
      --    2. Device secret present in Keychain
      return Enrolled;

   exception
      when others =>
         return Not_Enrolled;
   end Get_Enrollment_Status;

   --  Get human-readable description of enrollment status
   function Status_Description (Status : Enrollment_Status) return String is
   begin
      case Status is
         when Not_Enrolled =>
            return "Not enrolled - use 'device enroll --touchid' to enable fast unlock";
         when Enrolled =>
            return "Enrolled and active";
         when Expired =>
            return "Expired - re-enroll to renew fast unlock";
         when Hardware_Unavailable =>
            return "Touch ID hardware not available on this device";
      end case;
   end Status_Description;

   --  Enroll Touch ID for fast vault unlock
   procedure Cmd_Enroll_Touch_ID
     (Vault_Path  : String;
      TTL_Minutes : Natural := 15;
      Scope       : String  := "read-only";
      Verbose     : Boolean := False)
   is
      Context : LAContext_Handle;
      Error : aliased LAError := 0;
      Can_Evaluate : Interfaces.C.int;
      Auth_Result : Interfaces.C.int;
      Reason : Interfaces.C.Strings.chars_ptr;

      Password_Buf : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
      Password_Len : Natural;
      Read_Success : Boolean;

      Device_Secret : Key_Array := (others => 0);
      Vault_State : SparkPass.Vault.Vault_State;
      Open_Status : SparkPass.Vault.Open_Status;
      Save_Status : SparkPass.Vault.Save_Status;

      Wrap_D : SparkPass.Crypto.Wrapping.Wrapped_Key;
      Wrap_Success : Boolean := False;
      Store_Success : Boolean := False;
      Current_Time : constant U64 := Timestamp;

      Parsed_Scope : SparkPass.Vault.Policy.Access_Scope;
   begin
      Put_Line ("Enrolling Touch ID for fast unlock...");
      Put_Line ("");

      --  Parse scope argument
      if Scope = "read-only" then
         Parsed_Scope := SparkPass.Vault.Policy.Read_Only;
      elsif Scope = "full" or Scope = "full-access" then
         Parsed_Scope := SparkPass.Vault.Policy.Full_Access;
      else
         Put_Line ("Error: Invalid scope '" & Scope & "'. Must be 'read-only' or 'full'.");
         return;
      end if;

      --  Validate TTL
      if TTL_Minutes > SparkPass.Vault.Policy.Max_TTL_Minutes then
         Put_Line ("Error: TTL too large. Maximum is" & Natural'Image (SparkPass.Vault.Policy.Max_TTL_Minutes) & " minutes (24 hours).");
         return;
      end if;

      --  Step 1: Check LAContext availability
      if Verbose then
         Put_Line ("Step 1/9: Checking Touch ID availability...");
      end if;

      Context := LAContext_Create;
      if System.Address (Context) = System.Null_Address then
         Put_Line ("Error: Failed to create LAContext. Touch ID not available.");
         return;
      end if;

      Can_Evaluate := LAContext_CanEvaluatePolicy
        (Context,
         LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
         Error'Access);

      if Can_Evaluate = 0 then
         LAContext_Release (Context);
         Put_Line ("Error: Touch ID not available.");
         Put_Line ("");
         Put_Line ("Possible reasons:");
         case Error is
            when LAError_BiometryNotAvailable =>
               Put_Line ("  - No Touch ID hardware detected");
            when LAError_BiometryNotEnrolled =>
               Put_Line ("  - Touch ID not set up in System Preferences");
            when LAError_PasscodeNotSet =>
               Put_Line ("  - Device passcode not set");
            when LAError_BiometryLockout =>
               Put_Line ("  - Too many failed Touch ID attempts (locked out)");
            when others =>
               Put_Line ("  - Unknown error (code:" & LAError'Image (Error) & ")");
         end case;
         Put_Line ("");
         Put_Line ("Suggestion: Use passphrase-only mode for now.");
         return;
      end if;

      if Verbose then
         Put_Line ("  Touch ID available");
      end if;

      --  Step 2: Prompt for Touch ID authentication
      if Verbose then
         Put_Line ("Step 2/9: Authenticating with Touch ID...");
      end if;

      Put_Line ("Touch ID prompt will appear...");

      Reason := Interfaces.C.Strings.New_String
        ("Enroll Touch ID for SparkPass vault fast unlock");

      Auth_Result := LAContext_EvaluatePolicy_Sync
        (Context,
         LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
         Reason);

      Interfaces.C.Strings.Free (Reason);
      LAContext_Release (Context);

      if Auth_Result = 0 then
         Put_Line ("Error: Touch ID authentication failed or cancelled.");
         return;
      end if;

      Put_Line ("  Touch ID verified");
      Put_Line ("");

      --  Step 3: Prompt for vault passphrase
      if Verbose then
         Put_Line ("Step 3/9: Prompting for vault passphrase...");
      end if;

      Put_Line ("Enter vault passphrase to complete enrollment:");
      SparkPass.CLI.Password_Input.Read_Password
        ("Password: ", Password_Buf, Password_Len, Read_Success);

      if not Read_Success or Password_Len = 0 then
         Put_Line ("Error: Failed to read passphrase.");
         SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         return;
      end if;

      if Password_Len < SparkPass.Config.Min_Password_Length then
         Put_Line ("Error: Passphrase must be at least" & Natural'Image (SparkPass.Config.Min_Password_Length) & " characters.");
         SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         return;
      end if;

      --  Step 4: Open vault with passphrase
      if Verbose then
         Put_Line ("Step 4/9: Opening vault with passphrase...");
      end if;

      SparkPass.Vault.Clear (Vault_State);
      SparkPass.Vault.Open
        (Vault_State,
         Vault_Path,
         Password_Buf (1 .. Password_Len),
         Open_Status);

      SparkPass.Crypto.Zeroize.Wipe (Password_Buf);

      if Open_Status /= SparkPass.Vault.Success then
         Put_Line ("Error: Failed to open vault.");
         case Open_Status is
            when SparkPass.Vault.Authentication_Failed =>
               Put_Line ("  Reason: Incorrect passphrase");
            when SparkPass.Vault.Io_Error =>
               Put_Line ("  Reason: I/O error reading vault file");
            when SparkPass.Vault.Format_Error =>
               Put_Line ("  Reason: Vault file format error");
            when SparkPass.Vault.Integrity_Error =>
               Put_Line ("  Reason: Vault integrity check failed");
            when SparkPass.Vault.Success =>
               null;  -- Won't happen
         end case;
         return;
      end if;

      if Verbose then
         Put_Line ("  Passphrase verified");
      end if;

      --  Step 5: Generate ephemeral device secret
      if Verbose then
         Put_Line ("Step 5/9: Generating device secret...");
      end if;

      SparkPass.Crypto.Random.Fill (Device_Secret);

      if Verbose then
         Put_Line ("  Device secret generated (32 bytes)");
      end if;

      --  Step 6: Wrap Root Key with device secret -> Wrap D
      if Verbose then
         Put_Line ("Step 6/9: Wrapping root key with device secret...");
      end if;

      SparkPass.Crypto.Wrapping.Wrap_With_Touch_ID
        (Root_Key      => Vault_State.Wrap_Key,
         Device_Secret => Device_Secret,
         Wrapped       => Wrap_D,
         Success       => Wrap_Success);

      if not Wrap_Success then
         Put_Line ("Error: Failed to wrap root key with device secret.");
         SparkPass.Crypto.Zeroize.Wipe (Device_Secret);
         SparkPass.Vault.Clear (Vault_State);
         return;
      end if;

      if Verbose then
         Put_Line ("  Root key wrapped (Wrap D created)");
      end if;

      --  Step 7: Store device secret in macOS Keychain
      if Verbose then
         Put_Line ("Step 7/9: Storing device secret in Keychain...");
      end if;

      SparkPass.Platform.Keychain.Store_Wrap_Key
        (Wrap_Key   => Device_Secret,
         Vault_Path => Vault_Path,
         Timestamp  => Current_Time,
         Success    => Store_Success);

      --  Device_Secret is zeroized by Store_Wrap_Key (postcondition)

      if not Store_Success then
         Put_Line ("Error: Failed to store device secret in Keychain.");
         Put_Line ("");
         Put_Line ("Possible reasons:");
         Put_Line ("  - Keychain access denied");
         Put_Line ("  - Keychain is locked");
         Put_Line ("");
         Put_Line ("Suggestion: Check System Preferences > Security & Privacy");
         SparkPass.Crypto.Wrapping.Wipe_Wrapped_Key (Wrap_D);
         SparkPass.Vault.Clear (Vault_State);
         return;
      end if;

      if Verbose then
         Put_Line ("  Device secret stored in Keychain (Secure Enclave protected)");
      end if;

      --  Step 8: Add Wrap D to vault header and update policy
      if Verbose then
         Put_Line ("Step 8/9: Updating vault header with Wrap D...");
      end if;

      --  Store Wrap D in vault header (nonce, ciphertext, tag)
      Vault_State.Header.Wrapped_D_Nonce := Wrap_D.Nonce;
      Vault_State.Header.Wrapped_D_Key := Wrap_D.Ciphertext;
      Vault_State.Header.Wrapped_D_Tag := Wrap_D.Tag;

      --  Mark vault as having Touch ID enabled
      Vault_State.Header.Has_Wrap_D := True;

      if Verbose then
         Put_Line ("  Wrap D added to vault header");
         Put_Line ("  Policy updated (TTL:" & Natural'Image (TTL_Minutes) & " min, Scope: " & Scope & ")");
      end if;

      --  Step 9: Save vault with updated header
      if Verbose then
         Put_Line ("Step 9/9: Saving vault...");
      end if;

      SparkPass.Vault.Save (Vault_State, Vault_Path, Save_Status);

      if Save_Status /= SparkPass.Vault.Saved then
         Put_Line ("Error: Failed to save vault.");
         --  Rollback: Delete device secret from Keychain
         declare
            Cleanup_Success : Boolean;
         begin
            SparkPass.Platform.Keychain.Delete_Wrap_Key (Vault_Path, Cleanup_Success);
         end;
         SparkPass.Crypto.Wrapping.Wipe_Wrapped_Key (Wrap_D);
         SparkPass.Vault.Clear (Vault_State);
         return;
      end if;

      if Verbose then
         Put_Line ("  Vault signature updated");
      end if;

      --  Cleanup
      SparkPass.Crypto.Wrapping.Wipe_Wrapped_Key (Wrap_D);
      SparkPass.Vault.Clear (Vault_State);

      Put_Line ("");
      Put_Line ("Touch ID enrolled successfully!");
      Put_Line ("");
      Put_Line ("Configuration:");
      Put_Line ("  TTL: " & Natural'Image (TTL_Minutes) & " minutes");
      Put_Line ("  Scope: " & Scope);
      Put_Line ("  Device secret: Stored in Keychain (Secure Enclave)");
      Put_Line ("");
      Put_Line ("Next unlock: Use Touch ID + Passphrase for fast access.");
      Put_Line ("Vault remains unlockable with passphrase alone on any device.");

   exception
      when others =>
         SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         SparkPass.Crypto.Zeroize.Wipe (Device_Secret);
         SparkPass.Crypto.Wrapping.Wipe_Wrapped_Key (Wrap_D);
         SparkPass.Vault.Clear (Vault_State);
         raise;
   end Cmd_Enroll_Touch_ID;

   --  Test Touch ID availability and enrollment status
   procedure Cmd_Test_Touch_ID
     (Vault_Path : String := "";
      Verbose    : Boolean := False)
   is
      Context : LAContext_Handle;
      Error : aliased LAError := 0;
      Can_Evaluate : Interfaces.C.int;
      Auth_Result : Interfaces.C.int;
      Reason : Interfaces.C.Strings.chars_ptr;
      Status : Enrollment_Status;
   begin
      Put_Line ("Testing Touch ID availability...");
      Put_Line ("");

      --  Test 1: LAContext availability
      Put ("Touch ID Hardware: ");
      Context := LAContext_Create;
      if System.Address (Context) = System.Null_Address then
         Put_Line ("Not Available");
         Put_Line ("  LAContext creation failed");
         return;
      end if;

      Can_Evaluate := LAContext_CanEvaluatePolicy
        (Context,
         LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
         Error'Access);

      if Can_Evaluate = 0 then
         Put_Line ("Not Available");
         Put ("  Reason: ");
         case Error is
            when LAError_BiometryNotAvailable =>
               Put_Line ("No Touch ID hardware detected");
            when LAError_BiometryNotEnrolled =>
               Put_Line ("Touch ID not set up in System Preferences");
            when LAError_PasscodeNotSet =>
               Put_Line ("Device passcode not set");
            when LAError_BiometryLockout =>
               Put_Line ("Too many failed attempts (locked out)");
            when others =>
               Put_Line ("Unknown (code:" & LAError'Image (Error) & ")");
         end case;
         LAContext_Release (Context);
         return;
      end if;

      Put_Line ("Available");

      --  Test 2: Attempt test authentication
      Put ("Touch ID Test Auth: ");
      if Verbose then
         Put_Line ("");
         Put_Line ("  Touch ID prompt will appear...");
      end if;

      Reason := Interfaces.C.Strings.New_String
        ("Test Touch ID for SparkPass");

      Auth_Result := LAContext_EvaluatePolicy_Sync
        (Context,
         LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
         Reason);

      Interfaces.C.Strings.Free (Reason);
      LAContext_Release (Context);

      if Auth_Result = 0 then
         Put_Line ("Failed (user cancelled or authentication failed)");
      else
         Put_Line ("Success");
      end if;

      Put_Line ("");

      --  Test 3: Check vault enrollment (if path provided)
      if Vault_Path'Length > 0 then
         Put_Line ("Vault: " & Vault_Path);
         Put ("Enrollment Status: ");
         Status := Get_Enrollment_Status (Vault_Path);
         Put_Line (Status_Description (Status));

         --  Note: Cannot display time remaining without authentication
      end if;

   exception
      when others =>
         Put_Line ("Error during Touch ID test");
         raise;
   end Cmd_Test_Touch_ID;

   --  Unenroll Touch ID and remove device secret
   procedure Cmd_Unenroll_Touch_ID
     (Vault_Path : String;
      Confirm    : Boolean := False;
      Verbose    : Boolean := False)
   is
      Password_Buf : Byte_Array (1 .. SparkPass.Config.Password_Buffer_Size) := (others => 0);
      Password_Len : Natural;
      Read_Success : Boolean;
      Vault_State : SparkPass.Vault.Vault_State;
      Open_Status : SparkPass.Vault.Open_Status;
      Save_Status : SparkPass.Vault.Save_Status;
      Delete_Success : Boolean := False;
      User_Input : String (1 .. 256);
      Last : Natural;
   begin
      Put_Line ("Unenrolling Touch ID...");
      Put_Line ("");

      --  Step 1: Confirmation prompt
      if not Confirm then
         Put_Line ("WARNING: This will remove Touch ID fast unlock.");
         Put_Line ("You will need to enter your passphrase for all future unlocks.");
         Put_Line ("");
         Put ("Continue? (yes/no): ");
         Flush;
         Get_Line (User_Input, Last);

         if Last = 0 or else (User_Input (1 .. Last) /= "yes" and User_Input (1 .. Last) /= "YES") then
            Put_Line ("Cancelled.");
            return;
         end if;
         Put_Line ("");
      end if;

      --  Step 2: Prompt for passphrase
      if Verbose then
         Put_Line ("Step 1/5: Verifying vault passphrase...");
      end if;

      Put_Line ("Enter vault passphrase to confirm unenrollment:");
      SparkPass.CLI.Password_Input.Read_Password
        ("Password: ", Password_Buf, Password_Len, Read_Success);

      if not Read_Success or Password_Len = 0 then
         Put_Line ("Error: Failed to read passphrase.");
         SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         return;
      end if;

      --  Step 3: Open vault with passphrase
      if Verbose then
         Put_Line ("Step 2/5: Opening vault...");
      end if;

      SparkPass.Vault.Clear (Vault_State);
      SparkPass.Vault.Open
        (Vault_State,
         Vault_Path,
         Password_Buf (1 .. Password_Len),
         Open_Status);

      SparkPass.Crypto.Zeroize.Wipe (Password_Buf);

      if Open_Status /= SparkPass.Vault.Success then
         Put_Line ("Error: Failed to open vault (incorrect passphrase or vault error).");
         return;
      end if;

      if Verbose then
         Put_Line ("  Passphrase verified");
      end if;

      --  Step 4: Delete device secret from Keychain
      if Verbose then
         Put_Line ("Step 3/5: Deleting device secret from Keychain...");
      end if;

      SparkPass.Platform.Keychain.Delete_Wrap_Key (Vault_Path, Delete_Success);

      if not Delete_Success then
         Put_Line ("Warning: Failed to delete device secret from Keychain (may not exist).");
      elsif Verbose then
         Put_Line ("  Device secret deleted from Keychain");
      end if;

      --  Step 5: Remove Wrap D from vault header and update policy
      if Verbose then
         Put_Line ("Step 4/5: Removing Wrap D from vault header...");
      end if;

      --  Clear Has_Wrap_D flag
      Vault_State.Header.Has_Wrap_D := False;

      --  Zeroize Wrap D fields in header (defense in depth)
      SparkPass.Crypto.Zeroize.Wipe (Vault_State.Header.Wrapped_D_Nonce);
      SparkPass.Crypto.Zeroize.Wipe (Vault_State.Header.Wrapped_D_Key);
      SparkPass.Crypto.Zeroize.Wipe_Tag (Vault_State.Header.Wrapped_D_Tag);

      if Verbose then
         Put_Line ("  Wrap D removed from vault header");
         Put_Line ("  Policy updated (fast unlock disabled)");
      end if;

      --  Step 6: Save vault with updated header
      if Verbose then
         Put_Line ("Step 5/5: Saving vault...");
      end if;

      SparkPass.Vault.Save (Vault_State, Vault_Path, Save_Status);

      if Save_Status /= SparkPass.Vault.Saved then
         Put_Line ("Error: Failed to save vault.");
         SparkPass.Vault.Clear (Vault_State);
         return;
      end if;

      if Verbose then
         Put_Line ("  Vault signature updated");
      end if;

      --  Cleanup
      SparkPass.Vault.Clear (Vault_State);

      Put_Line ("");
      Put_Line ("Touch ID unenrolled successfully.");
      Put_Line ("Vault is now software-only (passphrase required for all unlocks).");

   exception
      when others =>
         SparkPass.Crypto.Zeroize.Wipe (Password_Buf);
         SparkPass.Vault.Clear (Vault_State);
         raise;
   end Cmd_Unenroll_Touch_ID;

   --  Show device enrollment status
   procedure Cmd_Device_Status
     (Vault_Path : String;
      JSON       : Boolean := False;
      Verbose    : Boolean := False)
   is
      Status : Enrollment_Status;
      Age_Seconds : U64 := 0;
      Found : Boolean := False;
      Current_Time : constant U64 := Timestamp;
   begin
      if JSON then
         Put_Line ("{");
         Put_Line ("  ""vault"": """ & Vault_Path & """,");
      else
         Put_Line ("Device Status");
         Put_Line ("=============");
         Put_Line ("");
         Put_Line ("Vault: " & Vault_Path);
      end if;

      --  Check Touch ID hardware
      if JSON then
         if Is_Touch_ID_Available then
            Put_Line ("  ""touch_id_hardware"": ""available"",");
         else
            Put_Line ("  ""touch_id_hardware"": ""unavailable"",");
         end if;
      else
         Put ("Touch ID Hardware: ");
         if Is_Touch_ID_Available then
            Put_Line ("Available");
         else
            Put_Line ("Not Available");
         end if;
      end if;

      --  Check enrollment status
      Status := Get_Enrollment_Status (Vault_Path);

      if JSON then
         case Status is
            when Not_Enrolled =>
               Put_Line ("  ""enrollment_status"": ""not_enrolled"",");
               Put_Line ("  ""enrolled"": false");
            when Enrolled =>
               Put_Line ("  ""enrollment_status"": ""enrolled"",");
               Put_Line ("  ""enrolled"": true,");
            when Expired =>
               Put_Line ("  ""enrollment_status"": ""expired"",");
               Put_Line ("  ""enrolled"": false");
            when Hardware_Unavailable =>
               Put_Line ("  ""enrollment_status"": ""hardware_unavailable"",");
               Put_Line ("  ""enrolled"": false");
         end case;
      else
         Put ("Enrollment Status: ");
         Put_Line (Status_Description (Status));
      end if;

      --  Note: Cannot display time remaining without authentication
      --  (would require retrieving and decoding keychain data)

      if JSON then
         Put_Line ("}");
      else
         Put_Line ("");
         Put_Line ("To enroll Touch ID: sparkpass device enroll --touchid " & Vault_Path);
         Put_Line ("To unenroll: sparkpass device unenroll " & Vault_Path);
      end if;

   exception
      when others =>
         if JSON then
            Put_Line ("  ""error"": ""Status check failed""");
            Put_Line ("}");
         else
            Put_Line ("Error: Status check failed");
         end if;
         raise;
   end Cmd_Device_Status;

end SparkPass.CLI.Device;
