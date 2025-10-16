pragma SPARK_Mode (Off);  -- FFI calls are not verifiable by SPARK
with Ada.Unchecked_Conversion;
with Interfaces; use type Interfaces.Unsigned_8; use type Interfaces.Unsigned_64;
with Interfaces.C; use type Interfaces.C.long; use type Interfaces.C.int;
with Interfaces.C.Strings;
with System; use type System.Address;
with Bindings.Keychain_Darwin; use Bindings.Keychain_Darwin;
with Bindings.LAContext_Darwin; use Bindings.LAContext_Darwin;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Platform.Keychain is

   Service_Name : constant String := "com.sparkpass.vault";

   --  Helper: Create keychain query dictionary for a given vault path
   function Create_Query_Dict (Vault_Path : String) return CFDictionaryRef is
      Dict : CFDictionaryRef;
      Service_C : Interfaces.C.Strings.chars_ptr;
      Account_C : Interfaces.C.Strings.chars_ptr;
      Service_CF : CFStringRef;
      Account_CF : CFStringRef;
   begin
      --  Create mutable dictionary
      Dict := CFDictionaryCreateMutable
        (System.Null_Address,  -- kCFAllocatorDefault
         0,  -- unlimited capacity
         kCFTypeDictionaryKeyCallBacks,
         kCFTypeDictionaryValueCallBacks);

      if CFDictionaryRef (Dict) = CFDictionaryRef (System.Null_Address) then
         return CFDictionaryRef (System.Null_Address);
      end if;

      --  Set item class = generic password
      CFDictionarySetValue
        (Dict,
         CFTypeRef (kSecClass),
         CFTypeRef (kSecClassGenericPassword));

      --  Set service name
      Service_C := Interfaces.C.Strings.New_String (Service_Name);
      Service_CF := CFStringCreateWithCString
        (System.Null_Address,
         Service_C,
         kCFStringEncodingUTF8);
      Interfaces.C.Strings.Free (Service_C);

      if CFStringRef (Service_CF) /= CFStringRef (System.Null_Address) then
         CFDictionarySetValue
           (Dict,
            CFTypeRef (kSecAttrService),
            CFTypeRef (Service_CF));
         CFRelease (CFTypeRef (Service_CF));
      end if;

      --  Set account name (vault path)
      Account_C := Interfaces.C.Strings.New_String (Vault_Path);
      Account_CF := CFStringCreateWithCString
        (System.Null_Address,
         Account_C,
         kCFStringEncodingUTF8);
      Interfaces.C.Strings.Free (Account_C);

      if CFStringRef (Account_CF) /= CFStringRef (System.Null_Address) then
         CFDictionarySetValue
           (Dict,
            CFTypeRef (kSecAttrAccount),
            CFTypeRef (Account_CF));
         CFRelease (CFTypeRef (Account_CF));
      end if;

      return Dict;
   end Create_Query_Dict;

   --  Helper: Create keychain data with wrap key + timestamp
   --  Format: 32 bytes wrap_key + 8 bytes timestamp (little-endian U64)
   function Create_Keychain_Data
     (Wrap_Key : Key_Array;
      Timestamp : U64) return CFDataRef
   is
      Data_Size : constant := 32 + 8;  -- wrap_key + timestamp
      Data : array (1 .. Data_Size) of U8 := (others => 0);
      Offset : Positive := 1;
      TS : U64 := Timestamp;
   begin
      --  Copy wrap key
      for I in Wrap_Key'Range loop
         Data (Offset) := Wrap_Key (I);
         Offset := Offset + 1;
      end loop;

      --  Append timestamp (little-endian)
      for I in 1 .. 8 loop
         Data (Offset) := U8 (TS mod 256);
         TS := TS / 256;
         Offset := Offset + 1;
      end loop;

      declare
         Result : constant CFDataRef := CFDataCreate
           (System.Null_Address,
            Data (Data'First)'Address,
            Interfaces.C.long (Data'Length));
      begin
         --  Zeroize local buffer
         for I in Data'Range loop
            Data (I) := 0;
         end loop;
         return Result;
      end;
   end Create_Keychain_Data;

   --  Helper: Extract wrap key and timestamp from keychain data
   procedure Extract_Keychain_Data
     (Data_Ref : CFDataRef;
      Wrap_Key : out Key_Array;
      Timestamp : out U64;
      Success : out Boolean)
   is
      Data_Ptr : System.Address;
      Data_Len : Interfaces.C.long;
      Expected_Size : constant := 32 + 8;
      Offset : Positive := 1;
      TS_Value : U64 := 0;
      TS_Multiplier : U64 := 1;

      type Byte_Array_Access is access all Byte_Array;
      pragma Warnings (Off, "possible aliasing problem");
      pragma Warnings (Off, "types for unchecked conversion have different sizes");
      function To_Byte_Array_Access is new Ada.Unchecked_Conversion
        (System.Address, Byte_Array_Access);
      pragma Warnings (On, "possible aliasing problem");
      pragma Warnings (On, "types for unchecked conversion have different sizes");
   begin
      Wrap_Key := (others => 0);
      Timestamp := 0;
      Success := False;

      if CFDataRef (Data_Ref) = CFDataRef (System.Null_Address) then
         return;
      end if;

      Data_Len := CFDataGetLength (Data_Ref);
      if Data_Len /= Expected_Size then
         return;  -- Wrong size
      end if;

      Data_Ptr := CFDataGetBytePtr (Data_Ref);
      if Data_Ptr = System.Null_Address then
         return;
      end if;

      declare
         Data : constant Byte_Array_Access := To_Byte_Array_Access (Data_Ptr);
      begin
         --  Extract wrap key (first 32 bytes)
         for I in Wrap_Key'Range loop
            Wrap_Key (I) := Data (Offset);
            Offset := Offset + 1;
         end loop;

         --  Extract timestamp (next 8 bytes, little-endian)
         for I in 1 .. 8 loop
            TS_Value := TS_Value + (U64 (Data (Offset)) * TS_Multiplier);
            TS_Multiplier := TS_Multiplier * 256;
            Offset := Offset + 1;
         end loop;

         Timestamp := TS_Value;
         Success := True;
      end;
   end Extract_Keychain_Data;

   procedure Store_Wrap_Key
     (Wrap_Key   : in out Key_Array;
      Vault_Path : String;
      Timestamp  : U64;
      Success    : out Boolean)
   is
      Attributes : CFDictionaryRef;
      Data_Ref : CFDataRef;
      Status : OSStatus;
      Result : aliased CFTypeRef := CFTypeRef (System.Null_Address);

      --  LAContext for biometric authentication
      Context : LAContext_Handle;
      Can_Use_Biometry : Interfaces.C.int;
      Biometry_Error : aliased LAError := 0;
      Auth_Result : Interfaces.C.int;
      Reason : Interfaces.C.Strings.chars_ptr;
   begin
      Success := False;

      --  Step 1: Check if Touch ID / Face ID is available
      Context := LAContext_Create;
      if System.Address (Context) = System.Null_Address then
         --  LAContext creation failed - fall back to no biometry
         SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);
         return;
      end if;

      Can_Use_Biometry := LAContext_CanEvaluatePolicy
        (Context,
         LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
         Biometry_Error'Access);

      if Can_Use_Biometry = 0 then
         --  Biometry not available (no Touch ID/Face ID enrolled)
         --  Fail silently - user will use password-only mode
         LAContext_Release (Context);
         SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);
         return;
      end if;

      --  Step 2: Prompt for biometric authentication
      Reason := Interfaces.C.Strings.New_String
        ("Authenticate to enable fast vault unlock for 7 days");

      Auth_Result := LAContext_EvaluatePolicy_Sync
        (Context,
         LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
         Reason);

      Interfaces.C.Strings.Free (Reason);
      LAContext_Release (Context);

      if Auth_Result = 0 then
         --  User cancelled or authentication failed
         SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);
         return;
      end if;

      --  Step 3: Biometric auth succeeded! Now store wrap_key in keychain
      --  NOTE: We do NOT use kSecAccessControlBiometryCurrentSet here
      --  because we already prompted for biometry above. This approach
      --  works in CLI tools without requiring provisioning profiles.

      --  Create keychain data (wrap key + timestamp)
      Data_Ref := Create_Keychain_Data (Wrap_Key, Timestamp);
      if CFDataRef (Data_Ref) = CFDataRef (System.Null_Address) then
         SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);
         return;
      end if;

      --  Build attributes dictionary
      Attributes := Create_Query_Dict (Vault_Path);
      if CFDictionaryRef (Attributes) = CFDictionaryRef (System.Null_Address) then
         CFRelease (CFTypeRef (Data_Ref));
         SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);
         return;
      end if;

      --  Add data
      CFDictionarySetValue
        (Attributes,
         CFTypeRef (kSecValueData),
         CFTypeRef (Data_Ref));

      --  Add accessibility (standard, no Secure Enclave protection)
      --  Key difference from previous approach: This works in CLI tools!
      CFDictionarySetValue
        (Attributes,
         CFTypeRef (kSecAttrAccessible),
         CFTypeRef (kSecAttrAccessibleWhenUnlockedThisDeviceOnly));

      --  Try to add to keychain
      Status := SecItemAdd (Attributes, Result'Access);

      --  If duplicate exists, delete old entry and retry
      if Status = errSecDuplicateItem then
         declare
            Query : constant CFDictionaryRef := Create_Query_Dict (Vault_Path);
         begin
            if CFDictionaryRef (Query) /= CFDictionaryRef (System.Null_Address) then
               Status := SecItemDelete (Query);
               CFRelease (CFTypeRef (Query));

               --  Retry add
               if Status = errSecSuccess or Status = errSecItemNotFound then
                  Status := SecItemAdd (Attributes, Result'Access);
               end if;
            end if;
         end;
      end if;

      --  Cleanup
      CFRelease (CFTypeRef (Attributes));
      CFRelease (CFTypeRef (Data_Ref));

      if CFTypeRef (Result) /= CFTypeRef (System.Null_Address) then
         CFRelease (Result);
      end if;

      SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);

      Success := (Status = errSecSuccess);
   end Store_Wrap_Key;

   procedure Retrieve_Wrap_Key
     (Wrap_Key     : out Key_Array;
      Vault_Path   : String;
      Current_Time : U64;
      Success      : out Boolean)
   is
      Query : CFDictionaryRef;
      Status : OSStatus;
      Result : aliased CFTypeRef := CFTypeRef (System.Null_Address);
      Stored_Time : U64 := 0;
      Extract_OK : Boolean := False;

      --  LAContext for biometric authentication
      Context : LAContext_Handle;
      Can_Use_Biometry : Interfaces.C.int;
      Biometry_Error : aliased LAError := 0;
      Auth_Result : Interfaces.C.int;
      Reason : Interfaces.C.Strings.chars_ptr;
   begin
      Wrap_Key := (others => 0);
      Success := False;

      --  Step 1: Check if Touch ID / Face ID is available
      Context := LAContext_Create;
      if System.Address (Context) = System.Null_Address then
         --  LAContext creation failed - can't use biometry
         return;
      end if;

      Can_Use_Biometry := LAContext_CanEvaluatePolicy
        (Context,
         LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
         Biometry_Error'Access);

      if Can_Use_Biometry = 0 then
         --  Biometry not available
         LAContext_Release (Context);
         return;
      end if;

      --  Step 2: Prompt for biometric authentication
      Reason := Interfaces.C.Strings.New_String
        ("Unlock SparkPass vault with Touch ID");

      Auth_Result := LAContext_EvaluatePolicy_Sync
        (Context,
         LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
         Reason);

      Interfaces.C.Strings.Free (Reason);
      LAContext_Release (Context);

      if Auth_Result = 0 then
         --  User cancelled or authentication failed
         return;
      end if;

      --  Step 3: Biometric auth succeeded! Now retrieve wrap_key from keychain
      --  Build query dictionary
      Query := Create_Query_Dict (Vault_Path);
      if CFDictionaryRef (Query) = CFDictionaryRef (System.Null_Address) then
         return;
      end if;

      --  Request data return
      CFDictionarySetValue
        (Query,
         CFTypeRef (kSecReturnData),
         kCFBooleanTrue);

      --  Limit to one result
      CFDictionarySetValue
        (Query,
         CFTypeRef (kSecMatchLimit),
         CFTypeRef (kSecMatchLimitOne));

      --  Retrieve from keychain (no biometric prompt needed - we already did it!)
      Status := SecItemCopyMatching (Query, Result'Access);
      CFRelease (CFTypeRef (Query));

      if Status /= errSecSuccess then
         if CFTypeRef (Result) /= CFTypeRef (System.Null_Address) then
            CFRelease (Result);
         end if;
         return;
      end if;

      --  Extract wrap key and timestamp from CFData
      Extract_Keychain_Data
        (CFDataRef (Result),
         Wrap_Key,
         Stored_Time,
         Extract_OK);

      CFRelease (Result);

      if not Extract_OK then
         SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);
         return;
      end if;

      --  Check cache age (7-day expiration)
      if Current_Time < Stored_Time then
         --  Clock skew detected - reject
         SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);
         return;
      end if;

      declare
         Age : constant U64 := Current_Time - Stored_Time;
      begin
         if Age > Cache_Max_Age then
            --  Cache expired
            SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);
            return;
         end if;
      end;

      Success := True;
   end Retrieve_Wrap_Key;

   procedure Delete_Wrap_Key
     (Vault_Path : String;
      Success    : out Boolean)
   is
      Query : CFDictionaryRef;
      Status : OSStatus;
   begin
      Success := False;

      Query := Create_Query_Dict (Vault_Path);
      if CFDictionaryRef (Query) = CFDictionaryRef (System.Null_Address) then
         return;
      end if;

      Status := SecItemDelete (Query);
      CFRelease (CFTypeRef (Query));

      --  Success if deleted or not found
      Success := (Status = errSecSuccess or Status = errSecItemNotFound);
   end Delete_Wrap_Key;

   function Has_Cached_Key (Vault_Path : String) return Boolean
   is
      Query : CFDictionaryRef;
      Status : OSStatus;
      Result : aliased CFTypeRef := CFTypeRef (System.Null_Address);
   begin
      --  Build query dictionary
      Query := Create_Query_Dict (Vault_Path);
      if CFDictionaryRef (Query) = CFDictionaryRef (System.Null_Address) then
         return False;
      end if;

      --  Just check existence - don't retrieve data
      --  This avoids authentication prompts entirely
      CFDictionarySetValue
        (Query,
         CFTypeRef (kSecMatchLimit),
         CFTypeRef (kSecMatchLimitOne));

      --  Query keychain (just checking existence, not retrieving data)
      Status := SecItemCopyMatching (Query, Result'Access);
      CFRelease (CFTypeRef (Query));

      if Status /= errSecSuccess then
         if CFTypeRef (Result) /= CFTypeRef (System.Null_Address) then
            CFRelease (Result);
         end if;
         return False;
      end if;

      if CFTypeRef (Result) /= CFTypeRef (System.Null_Address) then
         CFRelease (Result);
      end if;

      --  Item exists in keychain
      return True;
   end Has_Cached_Key;

end SparkPass.Platform.Keychain;
