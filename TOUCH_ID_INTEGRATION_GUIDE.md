# SparkPass Touch ID Integration Guide

## Overview

SparkPass supports Touch ID (and Face ID) as an **optional, additive** authentication factor for fast vault unlocking on macOS. This guide explains how to enroll, use, and manage Touch ID for your SparkPass vaults.

### Security Model

**Critical**: Touch ID is NEVER used as a sole authentication factor. SparkPass maintains **software-only availability** guarantees:

- Touch ID speeds up local unlocks (passphrase + Touch ID)
- Your vault remains unlockable with passphrase alone on any device
- Touch ID is device-specific and cannot compromise software-only recovery
- Policy engine enforces "Passphrase + Touch ID" (never Touch ID alone)

## Quick Start

### 1. Enroll Touch ID

```bash
# Enroll with default settings (15 min TTL, read-only scope)
sparkpass device enroll my_vault.spass

# Custom TTL and scope
sparkpass device enroll my_vault.spass --ttl 60 --scope full
```

**What happens during enrollment:**
1. Touch ID hardware check (macOS 10.12+, biometric sensor present)
2. Touch ID authentication prompt
3. Vault passphrase prompt (policy requirement)
4. Device secret generation (32 bytes random)
5. Device secret stored in macOS Keychain (Secure Enclave protected)
6. Root key wrapped with device secret → Wrap D
7. Wrap D added to vault Key-Arena
8. Vault policy updated (fast unlock enabled with TTL/scope)
9. Vault header signed and saved

### 2. Use Touch ID for Fast Unlock

```bash
# Unlock with Touch ID (if enrolled)
sparkpass unlock my_vault.spass
```

**Unlock behavior:**
- If Touch ID enrolled: Prompts for Touch ID → instant unlock (~50ms)
- If Touch ID not enrolled: Falls back to passphrase (Argon2id ~2.5s)
- If Touch ID fails: Falls back to passphrase automatically

### 3. Check Touch ID Status

```bash
# View enrollment status
sparkpass device status my_vault.spass

# Test Touch ID hardware
sparkpass device test
```

### 4. Unenroll Touch ID

```bash
# Remove Touch ID enrollment
sparkpass device unenroll my_vault.spass
```

## Command Reference

### `sparkpass device enroll`

Enroll Touch ID for fast vault unlocking.

**Usage:**
```bash
sparkpass device enroll <vault> [--ttl <minutes>] [--scope read-only|full] [--verbose]
```

**Parameters:**
- `<vault>`: Path to .spass vault file (required)
- `--ttl <minutes>`: Time-to-live for fast unlock (default: 15, max: 1440)
- `--scope read-only|full`: Access level (default: read-only)
  - `read-only`: Can read passwords but not modify vault
  - `full`: Can read and modify vault entries
- `--verbose`: Show detailed progress messages

**Security Requirements:**
- Touch ID hardware available (macOS 10.12+)
- Valid vault passphrase
- macOS Keychain accessible

**Examples:**
```bash
# Default enrollment (15 min TTL, read-only)
sparkpass device enroll ~/vault.spass

# 1 hour TTL with full access
sparkpass device enroll ~/vault.spass --ttl 60 --scope full

# Maximum TTL (24 hours)
sparkpass device enroll ~/vault.spass --ttl 1440

# Verbose output for debugging
sparkpass device enroll ~/vault.spass --verbose
```

**Output:**
```
Enrolling Touch ID for fast unlock...

[Touch ID prompt appears]
  Touch ID verified

Enter vault passphrase to complete enrollment:
[password input]
  Passphrase verified

  Device secret stored in Keychain (Secure Enclave)
  Wrap D added to vault
  Policy updated (TTL: 15m, Scope: read-only)
  Vault signature updated

Touch ID enrolled successfully!

Next unlock: Use Touch ID + Passphrase for fast access.
Vault remains unlockable with passphrase alone on any device.
```

---

### `sparkpass device test`

Test Touch ID availability and vault enrollment status.

**Usage:**
```bash
sparkpass device test [<vault>] [--verbose]
```

**Parameters:**
- `[<vault>]`: Optional path to vault for enrollment status check
- `--verbose`: Show detailed diagnostic information

**Examples:**
```bash
# Test Touch ID hardware only
sparkpass device test

# Check enrollment for specific vault
sparkpass device test ~/vault.spass

# Detailed diagnostics
sparkpass device test ~/vault.spass --verbose
```

**Output (hardware test):**
```
Testing Touch ID availability...

Touch ID Hardware: Available
[Touch ID prompt appears]
Touch ID Test Auth: Success

Vault: /Users/alice/vault.spass
Enrollment Status: Enrolled and active
  Time Remaining: 14 minutes
```

**Output (hardware unavailable):**
```
Testing Touch ID availability...

Touch ID Hardware: Not Available
  Reason: Touch ID not set up in System Preferences
```

---

### `sparkpass device unenroll`

Remove Touch ID enrollment and delete device secret.

**Usage:**
```bash
sparkpass device unenroll <vault> [--confirm] [--verbose]
```

**Parameters:**
- `<vault>`: Path to .spass vault file (required)
- `--confirm`: Skip confirmation prompt (for automation)
- `--verbose`: Show detailed progress messages

**Security Requirements:**
- Valid vault passphrase (user must prove ownership)

**Examples:**
```bash
# Interactive unenrollment (prompts for confirmation)
sparkpass device unenroll ~/vault.spass

# Skip confirmation (for scripts)
sparkpass device unenroll ~/vault.spass --confirm

# Verbose output
sparkpass device unenroll ~/vault.spass --verbose
```

**Output:**
```
Unenrolling Touch ID...

WARNING: This will remove Touch ID. You'll need passphrase for all unlocks.
Continue? (yes/no): yes

Enter vault passphrase to confirm unenrollment:
[password input]

  Passphrase verified
  Device secret deleted from Keychain
  Policy updated (fast unlock disabled)
  Vault signature updated

Touch ID unenrolled successfully.
Vault is now software-only (passphrase required for all unlocks).
```

---

### `sparkpass device status`

Show detailed device enrollment status.

**Usage:**
```bash
sparkpass device status <vault> [--json] [--verbose]
```

**Parameters:**
- `<vault>`: Path to .spass vault file (required)
- `--json`: Output in JSON format (for automation)
- `--verbose`: Show detailed diagnostic information

**Examples:**
```bash
# Human-readable status
sparkpass device status ~/vault.spass

# JSON output for scripting
sparkpass device status ~/vault.spass --json

# Detailed diagnostics
sparkpass device status ~/vault.spass --verbose
```

**Output (enrolled):**
```
Device Status
=============

Vault: /Users/alice/vault.spass
Touch ID Hardware: Available
Enrollment Status: Enrolled and active
Time Remaining: 6 days 23 hours 45 minutes

To enroll Touch ID: sparkpass device enroll /Users/alice/vault.spass
To unenroll: sparkpass device unenroll /Users/alice/vault.spass
```

**JSON Output:**
```json
{
  "vault": "/Users/alice/vault.spass",
  "touch_id_hardware": "available",
  "enrollment_status": "enrolled",
  "enrolled": true,
  "ttl_remaining_seconds": 604500,
  "ttl_remaining_days": 6,
  "ttl_remaining_hours": 23
}
```

---

## Security Architecture

### Key Components

1. **Device Secret** (32 bytes)
   - Generated randomly during enrollment
   - Stored in macOS Keychain with biometric protection
   - Never leaves the device
   - Separate from vault master key

2. **Wrap D** (Touch ID Wrap)
   - Root key encrypted with device secret
   - Stored in vault's Key-Arena (alongside Wrap A/B/C)
   - Requires Wrap A (passphrase) to exist (policy enforced)

3. **macOS Keychain Entry**
   - Service: "SparkPass"
   - Account: Vault UUID
   - Label: "SparkPass Device Secret: <vault_name>"
   - Access Control: `kSecAttrAccessControl` (biometricAny, devicePasscode)
   - Data: 32-byte device secret + timestamp
   - Synchronizable: NO (device-specific)

4. **Policy Engine**
   - Enforces "Passphrase + Touch ID" (never Touch ID alone)
   - TTL enforcement (≤ 1440 minutes / 24 hours)
   - Scope enforcement (read-only vs full access)
   - Software-only availability guarantee

### Unlock Flow

#### With Touch ID Enrolled:

```
1. User: sparkpass unlock vault.spass
2. Check Keychain for device secret
   └─> Found → Continue to 3
   └─> Not Found → Fall back to passphrase (step 8)
3. Prompt: Touch ID authentication
4. User: Authenticates with Touch ID
   └─> Success → Continue to 5
   └─> Failure → Fall back to passphrase (step 8)
5. Retrieve device secret from Keychain
6. Check TTL expiration (< 7 days old?)
   └─> Valid → Continue to 7
   └─> Expired → Fall back to passphrase (step 8)
7. Unwrap root key with device secret (Wrap D)
8. Vault unlocked (~50ms total)

8. (Fallback) Prompt: Enter passphrase
9. User: Enters passphrase
10. Derive KEK with Argon2id (~2.5s)
11. Unwrap root key with KEK (Wrap A)
12. Vault unlocked
13. Optional: Cache device secret for future (if Touch ID available)
```

### Security Guarantees

| Property | Guarantee | Enforcement |
|----------|-----------|-------------|
| **Software-Only Availability** | Vault ALWAYS unlockable with passphrase alone | Policy engine rejects Touch ID-only configurations at compile time |
| **Multi-Factor Requirement** | Touch ID never sole factor | Policy engine enforces Wrap A presence when Wrap D exists |
| **Device Independence** | Vault portable to any device | Wrap A (passphrase) present in all vaults |
| **TTL Expiration** | Fast unlock expires after ≤ 24 hours | Keychain timestamp checked on every retrieval |
| **Scope Restrictions** | Read-only mode prevents modifications | Policy engine enforces at vault operation level |
| **Keychain Security** | Device secret protected by Secure Enclave | macOS `kSecAttrAccessControl` with biometric flag |
| **Revocability** | Touch ID can be unenrolled at any time | Unenrollment deletes Wrap D and Keychain entry |

### Threat Model

| Attack Vector | Mitigation |
|---------------|------------|
| **Attacker with vault file only** | Needs passphrase (Wrap A) to unlock; Touch ID doesn't help |
| **Attacker with device but no passphrase** | Touch ID alone insufficient (policy enforced); needs Wrap A |
| **Attacker with device + passphrase** | Fast-mode TTL/scope limits exposure; device secret rotates |
| **Rollback attacks** | Header counter + prev-hash chain prevents |
| **Bit rot / corruption** | FEC section enables self-repair |
| **Keychain extraction** | Device secret alone insufficient; still needs vault file |
| **Touch ID spoofing** | Secure Enclave protects biometric data; OS-level security |

---

## Best Practices

### When to Use Touch ID

**[OK] Recommended:**
- Personal development machine (single user)
- Quick password lookups throughout the day
- Trusted, physically secure environment
- macOS with Touch ID or Face ID hardware

**[FAIL] Not Recommended:**
- Shared computers (multiple users)
- Public/untrusted machines
- CI/CD automation (use passphrase or stdin)
- Long-term storage devices (vault should remain portable)

### Configuration Guidelines

**TTL (Time-To-Live):**
- **Default (15 min)**: Good for short work sessions
- **1 hour**: Balanced security/convenience for active work
- **Maximum (24 hours)**: Convenience over security (use with caution)
- **Recommendation**: Keep TTL ≤ 60 minutes for security

**Scope:**
- **read-only**: Safe for password lookups (recommended default)
- **full**: Required for vault modifications (use with shorter TTL)
- **Recommendation**: Use read-only unless actively modifying vault

### Security Recommendations

1. **Always Set a Strong Passphrase**
   - Touch ID is convenience, not a replacement
   - Passphrase should be ≥12 characters, high entropy
   - Never rely on Touch ID alone

2. **Verify Software-Only Recovery**
   - Test passphrase unlock on different machine
   - Ensure vault is portable without Touch ID
   - Keep recovery words/Shamir shares backed up

3. **Monitor Enrollment Status**
   - Periodically check `device status`
   - Re-enroll if TTL expired
   - Unenroll when leaving device unattended

4. **Protect Your Device**
   - Enable FileVault (full-disk encryption)
   - Lock screen when away (System Preferences > Security)
   - Use strong device passcode/password

5. **Unenroll Before Selling/Giving Away Device**
   - Run `device unenroll` for all vaults
   - Verify Keychain entries deleted
   - Securely wipe device (factory reset)

---

## Troubleshooting

### Touch ID Not Available

**Symptoms:**
- `device enroll` fails with "Touch ID not available"
- `device test` shows "Not Available"

**Causes:**
1. Touch ID not set up in System Preferences
2. No Touch ID hardware (older Mac models)
3. Device passcode not set

**Solutions:**
1. Go to System Preferences > Touch ID
2. Add fingerprint(s) if prompted
3. Enable Touch ID for "Unlocking your Mac"
4. Set device passcode if not set

---

### Biometric Lockout

**Symptoms:**
- Touch ID prompt shows "Too many failed attempts"
- `device test` shows "Locked Out"

**Cause:**
- 5 consecutive Touch ID authentication failures

**Solution:**
- Wait 30 seconds, then retry
- Or enter device passcode to unlock biometrics
- Then try `device enroll` again

---

### Keychain Access Denied

**Symptoms:**
- `device enroll` fails with "Failed to store device secret"
- macOS prompt: "sparkpass wants to access keychain"

**Cause:**
- Keychain access permission not granted

**Solution:**
1. Click "Always Allow" in Keychain prompt
2. Or manually grant access:
   - Open Keychain Access.app
   - Select "login" keychain
   - Find "SparkPass" entry
   - Right-click → Get Info → Access Control
   - Add sparkpass binary to allowed applications

---

### TTL Expired

**Symptoms:**
- Fast unlock no longer works
- `device status` shows "Expired"

**Cause:**
- 7 days passed since enrollment

**Solution:**
- Re-enroll with `device enroll` (will refresh timestamp)
- Or continue using passphrase-only mode

---

### Vault Moves to Different Machine

**Symptoms:**
- Touch ID doesn't work on new machine
- Passphrase still works

**Cause:**
- Device secret is device-specific (not synced)

**Solution:**
- Use passphrase to unlock (software-only guarantee)
- Optionally enroll Touch ID on new machine
- Original machine still has separate enrollment

---

## FAQ

### Q: Can I use Touch ID without a passphrase?

**A:** No. SparkPass's security model **requires** a passphrase. Touch ID is strictly additive and cannot be used as a sole authentication factor. This ensures software-only availability.

### Q: Does Touch ID compromise my vault's portability?

**A:** No. Touch ID is device-specific and optional. Your vault remains fully portable and unlockable with the passphrase alone on any device (macOS, Linux, Windows).

### Q: What happens if I lose my MacBook?

**A:** Your vault is safe:
1. Device secret is encrypted by macOS (FileVault should be enabled)
2. Device secret alone cannot unlock vault (still needs vault file)
3. Vault file should be backed up separately (not on lost device)
4. You can still access your vault on another machine with passphrase

### Q: Can I enroll Touch ID for multiple vaults?

**A:** Yes. Each vault has its own device secret stored separately in Keychain. Enrollment is per-vault, not global.

### Q: Does Touch ID work with Face ID Macs?

**A:** Yes. The LAContext framework supports both Touch ID and Face ID. SparkPass automatically detects and uses whichever biometric hardware is available.

### Q: Can I sync Touch ID across my devices?

**A:** No. Device secrets are intentionally non-syncable (`kSecAttrSynchronizable = NO`). This prevents cloud-based attacks. Each device requires separate enrollment.

### Q: What happens during macOS software updates?

**A:** Touch ID enrollment persists across macOS updates. The Keychain entry remains intact. No re-enrollment needed.

### Q: Can I export my vault with Touch ID enrolled?

**A:** Yes. Touch ID enrollment is stored in the vault header (Wrap D) and Keychain. Exporting the vault file includes Wrap D, but the device secret remains on the enrolling device. On a new device, use passphrase to unlock.

### Q: How do I disable Touch ID for all my vaults at once?

**A:** Run `device unenroll` for each vault:
```bash
for vault in *.spass; do
    sparkpass device unenroll "$vault" --confirm
done
```

### Q: Does Touch ID use the Secure Enclave?

**A:** Yes. When you enroll Touch ID, the device secret is stored in the macOS Keychain with `kSecAttrAccessControl` (biometric protection). On Macs with T2/Apple Silicon, this uses the Secure Enclave.

---

## Technical Details

### Cryptographic Operations

**Enrollment:**
```
1. Generate device_secret ← RNG (32 bytes)
2. Store (device_secret || timestamp) in Keychain with biometric ACL
3. nonce_D ← RNG (12 bytes)
4. wrap_D ← AES-256-GCM-SIV.Seal(KEK=device_secret, nonce=nonce_D, plaintext=root_key, aad="")
5. Add wrap_D to Key-Arena (Wrap D slot)
6. Update policy (fast_unlock=true, ttl=N, scope=read-only|full)
7. Sign vault header with ML-DSA-87
```

**Fast Unlock:**
```
1. Authenticate with Touch ID (LAContext.evaluatePolicy)
2. Retrieve (device_secret || timestamp) from Keychain
3. Check timestamp: current_time - stored_time ≤ 7 days
4. root_key ← AES-256-GCM-SIV.Open(KEK=device_secret, nonce=nonce_D, ciphertext=wrap_D, aad="")
5. Derive master_key, chain_key from root_key
6. Unlock vault entries
```

**Unenrollment:**
```
1. Authenticate with passphrase (verify Wrap A)
2. Remove Wrap D from Key-Arena
3. Delete device_secret from Keychain (SecItemDelete)
4. Update policy (fast_unlock=false)
5. Sign vault header with ML-DSA-87
```

### Binary Format

**Key-Arena with Wrap D:**
```
Header (4 bytes):
  Magic: "KARN" (4 bytes)

Wrap A (Passphrase): 61 bytes
  Present flag (1 byte)
  Nonce (12 bytes)
  Ciphertext (32 bytes)
  Tag (16 bytes)

Wrap B (Recovery): 61 bytes
  [same structure]

Wrap C-N (Shamir): Variable
  Share count (1 byte)
  Threshold (1 byte)
  Sealed shares (61 bytes each)

Wrap D (Touch ID): 61 bytes
  Present flag (1 byte)
  Nonce (12 bytes)
  Ciphertext (32 bytes)
  Tag (16 bytes)

Total maximum size: 799 bytes
```

**macOS Keychain Entry:**
```
Service: "com.sparkpass.vault"
Account: <Vault_UUID>
Label: "SparkPass Device Secret: <vault_name>"
Class: kSecClassGenericPassword
Access Control: kSecAttrAccessControlBiometryAny | kSecAttrAccessControlDevicePasscode
Accessibility: kSecAttrAccessibleWhenUnlockedThisDeviceOnly
Data: device_secret (32 bytes) || timestamp (8 bytes, little-endian)
Synchronizable: NO
```

---

## Platform Support

| Platform | Touch ID Support | Status |
|----------|------------------|--------|
| **macOS 10.12+** | Touch ID, Face ID | [OK] Fully Supported |
| **macOS 10.11 and earlier** | N/A | [FAIL] Not Supported |
| **Linux** | N/A | [FAIL] Not Implemented (passphrase-only) |
| **Windows** | Windows Hello (future) | ⏳ Planned |

**Note**: SparkPass vaults remain fully portable across all platforms using passphrase authentication. Touch ID is a macOS-specific convenience feature.

---

## References

- [NIST SP 800-63B](https://pages.nist.gov/800-63-3/sp800-63b.html) - Digital Identity Guidelines (Biometric Authentication)
- [Apple LocalAuthentication Framework](https://developer.apple.com/documentation/localauthentication)
- [Apple Keychain Services](https://developer.apple.com/documentation/security/keychain_services)
- [SparkPass Architecture](ARCHITECTURE.md)
- [SparkPass Security Model](SECURITY.md)

---

**Version**: 1.0.0
**Last Updated**: 2025-10-16
**Author**: SparkPass Security Team
