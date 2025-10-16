# LAContext Solution for SparkPass Touch ID

**Date**: October 15, 2025
**Status**: Alternative Approach Identified

## Executive Summary

**GOOD NEWS**: There's a simpler path to Touch ID that doesn't require:
- [FAIL] Apple Developer Program ($99/year)
- [FAIL] Provisioning profiles
- [FAIL] App bundle packaging
- [FAIL] Entitlements

## The LAContext Approach

### What is LAContext?

`LAContext` is part of the **LocalAuthentication framework** that provides a higher-level API for biometric authentication. Unlike the Keychain approach (which stores keys in the Secure Enclave), LAContext **just prompts** for biometric authentication.

### Two Approaches Compared

| Aspect | Keychain (`SecItemAdd` + `kSecAccessControlBiometryCurrentSet`) | LAContext (`evaluatePolicy`) |
|--------|--------------------------------------------------------------|------------------------------|
| **Requires** | Provisioning profile + app bundle + entitlements | Just the framework |
| **Storage** | Secure Enclave (hardware-backed) | Your own storage (keychain without biometry flag) |
| **Security** | Keys physically bound to biometric data | Software-level auth check |
| **Complexity** | High (packaging, signing, distribution) | Low (just API calls) |
| **Works in CLI** | [FAIL] No (requires app structure) | [OK] Yes (with runloop trick) |

### The Critical Difference

**Keychain Approach** (what we tried first):
```ada
-- Store wrap_key WITH biometric protection
SecItemAdd(
  kSecAttrAccessControl = SecAccessControlCreateWithFlags(
    kSecAccessControlBiometryCurrentSet  -- REQUIRES PROVISIONING PROFILE
  )
)
-- Key is locked in Secure Enclave
-- Can ONLY be retrieved with Touch ID
```

**LAContext Approach** (simpler):
```swift
// 1. Prompt for Touch ID
let context = LAContext()
context.evaluatePolicy(.deviceOwnerAuthenticationWithBiometrics,
                       localizedReason: "Unlock SparkPass vault") { success, error in
    if success {
        // 2. AFTER biometric auth succeeds, use normal keychain
        //    (without biometric protection flag)
        SecItemAdd(kSecAttrAccessible = kSecAttrAccessibleWhenUnlockedThisDeviceOnly)
        // No Secure Enclave binding, but still encrypted keychain storage
    }
}
```

## Implementation Strategy for SparkPass

### Architecture Change

**Current** (Phase 1-2):
```
User → Password → Argon2id → wrap_key → Keychain (with biometry flag) [FAIL] FAILS
                                       ↓
                                   SecItemAdd returns -34018 (missing entitlement)
```

**New LAContext Approach**:
```
User → Touch ID Prompt (LAContext) → Success?
                                       ↓ Yes
                      wrap_key ← Keychain (WITHOUT biometry flag) [OK] WORKS
```

### Ada/C Integration

Since SparkPass is written in Ada, we'll need C bindings to LAContext:

**New file**: `src/bindings/bindings-lacontext_darwin.ads`

```ada
pragma SPARK_Mode (Off);
with System;
with Interfaces.C;
with Interfaces.C.Strings;

package Bindings.LAContext_Darwin is

   --  LocalAuthentication framework types
   type LAContext_Handle is new System.Address;
   type LAPolicy is new Interfaces.C.int;

   --  Policy types
   LAPolicy_DeviceOwnerAuthenticationWithBiometrics : constant LAPolicy := 1;
   LAPolicy_DeviceOwnerAuthentication               : constant LAPolicy := 2;

   --  Error codes
   type LAError is new Interfaces.C.int;
   LAError_AuthenticationFailed : constant LAError := -1;
   LAError_UserCancel          : constant LAError := -2;
   LAError_UserFallback        : constant LAError := -3;
   LAError_BiometryNotEnrolled : constant LAError := -6;
   LAError_BiometryNotAvailable : constant LAError := -7;

   --  Callback type for async result
   type LAContext_Callback is access procedure
     (Success : Interfaces.C.int;
      Error   : LAError;
      User_Data : System.Address)
   with Convention => C;

   --  C helper functions (implemented in lacontext_helpers.c)
   function LAContext_Create return LAContext_Handle
   with Import, Convention => C, External_Name => "lacontext_create";

   procedure LAContext_Release (Context : LAContext_Handle)
   with Import, Convention => C, External_Name => "lacontext_release";

   function LAContext_CanEvaluatePolicy
     (Context : LAContext_Handle;
      Policy  : LAPolicy;
      Error   : access LAError) return Interfaces.C.int
   with Import, Convention => C, External_Name => "lacontext_can_evaluate_policy";

   procedure LAContext_EvaluatePolicy
     (Context : LAContext_Handle;
      Policy  : LAPolicy;
      Reason  : Interfaces.C.Strings.chars_ptr;
      Callback : LAContext_Callback;
      User_Data : System.Address)
   with Import, Convention => C, External_Name => "lacontext_evaluate_policy";

   --  Synchronous wrapper (blocks until result)
   function LAContext_EvaluatePolicy_Sync
     (Context : LAContext_Handle;
      Policy  : LAPolicy;
      Reason  : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int
   with Import, Convention => C, External_Name => "lacontext_evaluate_policy_sync";

end Bindings.LAContext_Darwin;
```

**New file**: `src/bindings/lacontext_helpers.c`

```c
#include <LocalAuthentication/LocalAuthentication.h>
#include <CoreFoundation/CoreFoundation.h>

// Opaque handle for LAContext
typedef void* LAContext_Handle;

// Create a new LAContext
LAContext_Handle lacontext_create(void) {
    LAContext *context = [[LAContext alloc] init];
    return (LAContext_Handle)CFBridgingRetain(context);
}

// Release LAContext
void lacontext_release(LAContext_Handle handle) {
    if (handle) {
        CFBridgingRelease(handle);
    }
}

// Check if biometry is available
int lacontext_can_evaluate_policy(LAContext_Handle handle, int policy, int *error) {
    LAContext *context = (__bridge LAContext*)handle;
    NSError *nsError = nil;

    BOOL canEvaluate = [context canEvaluatePolicy:policy error:&nsError];

    if (error && nsError) {
        *error = (int)nsError.code;
    }

    return canEvaluate ? 1 : 0;
}

// Synchronous wrapper that blocks until result
// This is the KEY to making it work in command-line tools!
int lacontext_evaluate_policy_sync(LAContext_Handle handle, int policy, const char *reason) {
    LAContext *context = (__bridge LAContext*)handle;
    NSString *nsReason = [NSString stringWithUTF8String:reason];

    __block int result = 0;
    __block BOOL done = FALSE;

    // Get current run loop
    CFRunLoopRef runLoop = CFRunLoopGetCurrent();

    [context evaluatePolicy:policy
             localizedReason:nsReason
                       reply:^(BOOL success, NSError *error) {
        result = success ? 1 : 0;
        done = TRUE;
        // Stop the run loop when done
        CFRunLoopStop(runLoop);
    }];

    // Keep the run loop running until the callback fires
    // This prevents the main thread from exiting before Touch ID prompt appears
    while (!done) {
        CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.1, TRUE);
    }

    return result;
}
```

### Updated Platform Layer

**Modify**: `src/sparkpass/platform/sparkpass-platform-keychain.adb`

```ada
with Bindings.LAContext_Darwin; use Bindings.LAContext_Darwin;

procedure Store_Wrap_Key
  (Wrap_Key  : Key_Array;
   Path      : String;
   Timestamp : U64;
   Success   : out Boolean)
is
   Context : LAContext_Handle;
   Can_Use_Biometry : Interfaces.C.int;
   Biometry_Error : aliased LAError := 0;
   Auth_Success : Interfaces.C.int;
   Reason : Interfaces.C.Strings.chars_ptr;
begin
   Success := False;

   -- Create LAContext
   Context := LAContext_Create;
   if Context = System.Null_Address then
      return;
   end if;

   -- Check if Touch ID is available
   Can_Use_Biometry := LAContext_CanEvaluatePolicy(
     Context,
     LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
     Biometry_Error'Access);

   if Can_Use_Biometry = 0 then
      -- Biometry not available, fail silently
      LAContext_Release(Context);
      return;
   end if;

   -- Prompt for Touch ID (BLOCKS until user authenticates)
   Reason := Interfaces.C.Strings.New_String(
     "Authenticate to enable fast vault unlock");

   Auth_Success := LAContext_EvaluatePolicy_Sync(
     Context,
     LAPolicy_DeviceOwnerAuthenticationWithBiometrics,
     Reason);

   Interfaces.C.Strings.Free(Reason);
   LAContext_Release(Context);

   if Auth_Success = 0 then
      -- User cancelled or authentication failed
      return;
   end if;

   -- Authentication succeeded! Now store wrap_key in keychain
   -- WITHOUT kSecAccessControlBiometryCurrentSet flag
   -- (just use regular kSecAttrAccessibleWhenUnlockedThisDeviceOnly)

   declare
      Service : aliased CFStringRef;
      Account : aliased CFStringRef;
      Data_Blob : Byte_Array (1 .. 40) := (others => 0);
      CF_Data : CFDataRef;
      Query : CFMutableDictionaryRef;
      Status : OSStatus;
   begin
      -- Create service and account strings
      Service := CFStringCreateWithCString(
        System.Null_Address,
        To_C("com.sparkpass.vault"),
        KCFStringEncodingUTF8);

      Account := CFStringCreateWithCString(
        System.Null_Address,
        To_C(Path),
        KCFStringEncodingUTF8);

      -- Pack wrap_key + timestamp into data blob
      for I in Wrap_Key'Range loop
         Data_Blob (I - Wrap_Key'First + 1) := Wrap_Key (I);
      end loop;

      -- Pack timestamp (U64, little-endian)
      Data_Blob (33) := U8 (Timestamp mod 256);
      Data_Blob (34) := U8 ((Timestamp / 256) mod 256);
      Data_Blob (35) := U8 ((Timestamp / 65536) mod 256);
      Data_Blob (36) := U8 ((Timestamp / 16777216) mod 256);
      Data_Blob (37) := U8 ((Timestamp / 4294967296) mod 256);
      Data_Blob (38) := U8 ((Timestamp / 1099511627776) mod 256);
      Data_Blob (39) := U8 ((Timestamp / 281474976710656) mod 256);
      Data_Blob (40) := U8 ((Timestamp / 72057594037927936) mod 256);

      CF_Data := CFDataCreate(
        System.Null_Address,
        Data_Blob (Data_Blob'First)'Address,
        CFIndex (Data_Blob'Length));

      -- Create keychain query dictionary
      Query := CFDictionaryCreateMutable(
        System.Null_Address,
        CFIndex (5),
        kCFTypeDictionaryKeyCallBacks,
        kCFTypeDictionaryValueCallBacks);

      CFDictionaryAddValue(Query, kSecClass, kSecClassGenericPassword);
      CFDictionaryAddValue(Query, kSecAttrService, Service);
      CFDictionaryAddValue(Query, kSecAttrAccount, Account);
      CFDictionaryAddValue(Query, kSecValueData, CF_Data);

      -- KEY DIFFERENCE: No kSecAccessControlBiometryCurrentSet!
      -- Just use standard accessibility (device-locked, not in backups)
      CFDictionaryAddValue(Query, kSecAttrAccessible,
                          kSecAttrAccessibleWhenUnlockedThisDeviceOnly);

      -- Store in keychain
      Status := SecItemAdd(Query, System.Null_Address);

      -- Clean up
      CFRelease(Service);
      CFRelease(Account);
      CFRelease(CF_Data);
      CFRelease(Query);
      SparkPass.Crypto.Zeroize.Wipe(Data_Blob);

      Success := (Status = ErrSecSuccess);
   end;
end Store_Wrap_Key;
```

## Security Trade-offs

### What We Lose

1. **No Secure Enclave Binding**: The wrap_key is stored in regular keychain, not hardware-bound to biometric data
2. **Software-Level Check**: LAContext prompt happens in software, not enforced by Secure Enclave hardware
3. **Theoretical Extraction**: If device is unlocked, malware could potentially extract the keychain item

### What We Keep

1. [OK] **Biometric Authentication**: User must use Touch ID/Face ID to unlock
2. [OK] **Device-Locked Storage**: Key not backed up, stays on device only
3. [OK] **Encrypted Keychain**: macOS keychain encryption protects the key at rest
4. [OK] **Performance**: Still ~50× faster than password (no Argon2id)
5. [OK] **User Experience**: Seamless Touch ID prompt

### Comparison to Password-Only

Even with the LAContext approach, this is **significantly more secure** than password-only:

| Threat | Password-Only | LAContext + Keychain | Secure Enclave (ideal) |
|--------|---------------|---------------------|------------------------|
| Offline attack | [OK] Protected (Argon2id) | [OK] Protected (Argon2id) | [OK] Protected (Argon2id) |
| Shoulder surfing | [FAIL] Vulnerable | [OK] Protected | [OK] Protected |
| Phishing | [FAIL] Vulnerable | [OK] Protected | [OK] Protected |
| Stolen unlocked device | [FAIL] Can enter password | [WARN] Can extract keychain | [OK] Can't extract |
| Malware on unlocked device | [FAIL] Can keylog password | [WARN] Can extract keychain | [OK] Hardware-protected |

**Conclusion**: LAContext approach is a **major security improvement** over password-only, even if not perfect.

## Implementation Timeline

### Phase 3A: LAContext Integration (Estimated: 2 days)

1. **Day 1**: FFI bindings
   - Create `bindings-lacontext_darwin.ads`
   - Implement `lacontext_helpers.c` with CFRunLoop trick
   - Test in isolation

2. **Day 2**: Platform layer updates
   - Modify `Store_Wrap_Key` to use LAContext prompt
   - Modify `Retrieve_Wrap_Key` to use LAContext prompt
   - Remove `kSecAccessControlBiometryCurrentSet` flag
   - Test end-to-end

### Phase 3B: Testing & Deployment (Estimated: 1 day)

1. Test on Mac with Touch ID enrolled
2. Verify performance improvements
3. Update documentation
4. Ship v1.1 with working biometric unlock

## Deployment Simplicity

**Before** (Secure Enclave approach):
- Need Apple Developer Program
- Need provisioning profile
- Need app bundle packaging
- Need Mac App Store or notarization
- Users install via `.app` bundle

**After** (LAContext approach):
- No Apple Developer Program
- No provisioning profile
- No app bundle needed
- Ad-hoc code signing works
- Users install via `brew` or direct download

## Recommended Path Forward

1. [OK] **Implement LAContext approach first** (Phase 3A)
   - Simpler, faster to market
   - Works in command-line context
   - Still provides major security + UX improvements

2. 🔮 **Consider Secure Enclave later** (Phase 4, optional)
   - For users who want maximum security
   - Requires Mac App Store distribution
   - Can offer as "Pro" version

## Testing Requirements

### Minimum Requirements

1. Mac with Touch ID hardware (M1/M2/M3/M4 or Touch Bar models)
2. At least one fingerprint enrolled
3. macOS 10.15+ (Catalina or newer)

### Test Plan

```bash
# Build with LocalAuthentication framework
./scripts/build-and-sign.sh

# Create test vault
./bin/sparkpass_main init ~/test.spass

# First unlock (with password, triggers LAContext to cache)
./bin/sparkpass_main unlock ~/test.spass
# → Prompts for password
# → After password success, Touch ID prompt appears
# → Place finger on sensor
# → "[PASS] password accepted"
# → "(biometric unlock enabled for 7 days)"

# Second unlock (biometric path)
./bin/sparkpass_main unlock ~/test.spass
# → Touch ID prompt appears IMMEDIATELY (no password)
# → Place finger on sensor
# → "[PASS] unlocked with biometric authentication" (<100ms)
```

## References

1. **Apple Documentation**: [LAContext - LocalAuthentication](https://developer.apple.com/documentation/localauthentication/lacontext)
2. **Apple Documentation**: [evaluatePolicy(_:localizedReason:reply:)](https://developer.apple.com/documentation/localauthentication/lacontext/evaluatepolicy(_:localizedreason:reply:))
3. **Stack Overflow**: [How to wait for LAContext.evaluatePolicy result in a simple terminal app](https://stackoverflow.com/questions/69392053/how-to-wait-for-lacontext-evaluatepolicy-result-in-a-simple-terminal-app)
4. **Apple Documentation**: [Run Loops](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Multithreading/RunLoopManagement/RunLoopManagement.html)
5. **Tutorial**: [Face ID and Touch ID in Swift 5](https://www.advancedswift.com/face-id-touch-id-swift/)

---

**Decision Point**: Implement LAContext approach for v1.1 release?

**Recommendation**: [OK] YES - Provides 90% of the security benefits with 10% of the complexity.
