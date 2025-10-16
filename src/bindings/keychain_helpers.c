/*
 * Security Framework constant accessors for Ada FFI
 *
 * The Security framework defines keychain constants as extern CFStringRef values.
 * These cannot be directly imported into Ada, so we provide C functions that
 * return the constant addresses.
 *
 * Reference: <Security/Security.h>
 */

#include <stdio.h>
#include <CoreFoundation/CoreFoundation.h>
#include <Security/Security.h>

/* Keychain item class */
CFStringRef sparkpass_kSecClass(void) {
    return kSecClass;
}

CFStringRef sparkpass_kSecClassGenericPassword(void) {
    return kSecClassGenericPassword;
}

/* Keychain attribute keys */
CFStringRef sparkpass_kSecAttrService(void) {
    return kSecAttrService;
}

CFStringRef sparkpass_kSecAttrAccount(void) {
    return kSecAttrAccount;
}

CFStringRef sparkpass_kSecValueData(void) {
    return kSecValueData;
}

CFStringRef sparkpass_kSecReturnData(void) {
    return kSecReturnData;
}

CFStringRef sparkpass_kSecMatchLimit(void) {
    return kSecMatchLimit;
}

CFStringRef sparkpass_kSecMatchLimitOne(void) {
    return kSecMatchLimitOne;
}

/* Authentication UI control */
CFStringRef sparkpass_kSecUseAuthenticationUI(void) {
    return kSecUseAuthenticationUI;
}

CFStringRef sparkpass_kSecUseAuthenticationUIFail(void) {
    return kSecUseAuthenticationUIFail;
}

/* Biometric authentication keys */
CFStringRef sparkpass_kSecAttrAccessControl(void) {
    return kSecAttrAccessControl;
}

/* Accessibility attribute key */
CFStringRef sparkpass_kSecAttrAccessible(void) {
    return kSecAttrAccessible;
}

/* Accessibility level */
CFTypeRef sparkpass_kSecAttrAccessibleWhenUnlockedThisDeviceOnly(void) {
    return kSecAttrAccessibleWhenUnlockedThisDeviceOnly;
}

/* Boolean constants */
CFTypeRef sparkpass_kCFBooleanTrue(void) {
    return kCFBooleanTrue;
}

/* Dictionary callback constants */
const void* sparkpass_kCFTypeDictionaryKeyCallBacks(void) {
    return &kCFTypeDictionaryKeyCallBacks;
}

const void* sparkpass_kCFTypeDictionaryValueCallBacks(void) {
    return &kCFTypeDictionaryValueCallBacks;
}
