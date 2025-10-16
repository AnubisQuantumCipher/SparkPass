pragma SPARK_Mode (Off);
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

--  macOS Security Framework bindings for keychain operations
--  Reference: https://developer.apple.com/documentation/security/keychain_services
package Bindings.Keychain_Darwin is
   pragma Preelaborate;

   --  Opaque CoreFoundation types
   type CFDictionaryRef is new System.Address;
   type CFTypeRef is new System.Address;
   type CFDataRef is new System.Address;
   type CFStringRef is new System.Address;

   --  OSStatus error codes
   type OSStatus is new Interfaces.Integer_32;
   errSecSuccess : constant OSStatus := 0;
   errSecItemNotFound : constant OSStatus := -25300;
   errSecDuplicateItem : constant OSStatus := -25299;
   errSecAuthFailed : constant OSStatus := -25293;
   errSecUserCanceled : constant OSStatus := -128;

   --  Keychain item class constants (CFStringRef values defined in C)
   --  We'll use C strings for these constants
   function kSecClass return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecClass";
   function kSecClassGenericPassword return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecClassGenericPassword";

   --  Keychain attribute keys
   function kSecAttrService return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecAttrService";
   function kSecAttrAccount return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecAttrAccount";
   function kSecValueData return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecValueData";
   function kSecReturnData return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecReturnData";
   function kSecMatchLimit return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecMatchLimit";
   function kSecMatchLimitOne return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecMatchLimitOne";

   --  Authentication UI control
   function kSecUseAuthenticationUI return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecUseAuthenticationUI";
   function kSecUseAuthenticationUIFail return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecUseAuthenticationUIFail";

   --  Biometric authentication keys
   function kSecAttrAccessControl return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecAttrAccessControl";
   function kSecAttrAccessible return CFStringRef
     with Import, Convention => C, External_Name => "sparkpass_kSecAttrAccessible";

   --  Access control flags for biometric authentication
   type SecAccessControlCreateFlags is new Interfaces.Unsigned_32;
   kSecAccessControlUserPresence : constant SecAccessControlCreateFlags := 1;
   kSecAccessControlBiometryAny : constant SecAccessControlCreateFlags := 2;
   kSecAccessControlBiometryCurrentSet : constant SecAccessControlCreateFlags := 8;

   --  Access control object (opaque)
   type SecAccessControlRef is new System.Address;

   --  Create access control object requiring biometric authentication
   --  SecAccessControlCreateWithFlags returns NULL on error
   function SecAccessControlCreateWithFlags
     (allocator : System.Address;  --  kCFAllocatorDefault (NULL)
      protection : CFTypeRef;        --  kSecAttrAccessibleWhenUnlockedThisDeviceOnly
      flags : SecAccessControlCreateFlags;
      error : access CFTypeRef) return SecAccessControlRef
     with Import, Convention => C, External_Name => "SecAccessControlCreateWithFlags";

   function kSecAttrAccessibleWhenUnlockedThisDeviceOnly return CFTypeRef
     with Import, Convention => C, External_Name => "sparkpass_kSecAttrAccessibleWhenUnlockedThisDeviceOnly";

   --  Keychain operations
   function SecItemAdd
     (attributes : CFDictionaryRef;
      result : access CFTypeRef) return OSStatus
     with Import, Convention => C, External_Name => "SecItemAdd";

   function SecItemCopyMatching
     (query : CFDictionaryRef;
      result : access CFTypeRef) return OSStatus
     with Import, Convention => C, External_Name => "SecItemCopyMatching";

   function SecItemDelete
     (query : CFDictionaryRef) return OSStatus
     with Import, Convention => C, External_Name => "SecItemDelete";

   --  CoreFoundation memory management
   procedure CFRelease (cf : CFTypeRef)
     with Import, Convention => C, External_Name => "CFRelease";

   --  CoreFoundation dictionary creation (mutable)
   function CFDictionaryCreateMutable
     (allocator : System.Address;
      capacity : Interfaces.C.long;
      keyCallBacks : System.Address;
      valueCallBacks : System.Address) return CFDictionaryRef
     with Import, Convention => C, External_Name => "CFDictionaryCreateMutable";

   --  Dictionary manipulation
   procedure CFDictionarySetValue
     (theDict : CFDictionaryRef;
      key : CFTypeRef;
      value : CFTypeRef)
     with Import, Convention => C, External_Name => "CFDictionarySetValue";

   --  String creation
   function CFStringCreateWithCString
     (allocator : System.Address;
      cStr : Interfaces.C.Strings.chars_ptr;
      encoding : Interfaces.Unsigned_32) return CFStringRef
     with Import, Convention => C, External_Name => "CFStringCreateWithCString";

   kCFStringEncodingUTF8 : constant Interfaces.Unsigned_32 := 16#08000100#;

   --  Data creation and access
   function CFDataCreate
     (allocator : System.Address;
      bytes : System.Address;
      length : Interfaces.C.long) return CFDataRef
     with Import, Convention => C, External_Name => "CFDataCreate";

   function CFDataGetBytePtr (theData : CFDataRef) return System.Address
     with Import, Convention => C, External_Name => "CFDataGetBytePtr";

   function CFDataGetLength (theData : CFDataRef) return Interfaces.C.long
     with Import, Convention => C, External_Name => "CFDataGetLength";

   --  Boolean constants
   function kCFBooleanTrue return CFTypeRef
     with Import, Convention => C, External_Name => "sparkpass_kCFBooleanTrue";

   --  Dictionary callback constants (for standard key-value behavior)
   function kCFTypeDictionaryKeyCallBacks return System.Address
     with Import, Convention => C, External_Name => "sparkpass_kCFTypeDictionaryKeyCallBacks";

   function kCFTypeDictionaryValueCallBacks return System.Address
     with Import, Convention => C, External_Name => "sparkpass_kCFTypeDictionaryValueCallBacks";

end Bindings.Keychain_Darwin;
