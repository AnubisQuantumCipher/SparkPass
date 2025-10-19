# SparkPass Touch ID Troubleshooting Guide

This guide helps you diagnose and fix common Touch ID integration issues with SparkPass.

## Quick Diagnosis

Run the diagnostic command first:

```bash
sparkpass device test --verbose
```

This will show:
- Touch ID hardware status
- macOS LocalAuthentication framework availability
- Keychain accessibility
- Current enrollment status

---

## Common Issues

### 1. "Touch ID Not Available"

**Symptoms:**
```
Error: Touch ID not available.

Possible reasons:
  - No Touch ID hardware detected
```

**Diagnosis:**

```bash
# Check Touch ID hardware
sparkpass device test
```

**Causes & Solutions:**

#### Cause 1: Touch ID Not Configured

**Check:**
1. Open System Preferences → Touch ID
2. See if fingerprints are enrolled

**Solution:**
```
1. System Preferences → Touch ID
2. Click "Add Fingerprint..."
3. Follow on-screen instructions
4. Enable "Use Touch ID to unlock your Mac"
5. Retry: sparkpass device enroll vault.spass
```

#### Cause 2: No Touch ID Hardware

**Check:**
- MacBook Pro (2016 and later with Touch Bar)
- MacBook Air (2018 and later)
- Mac with Touch ID

**Solution:**
- Use passphrase-only mode (software-only availability)
- Touch ID not required for vault access
- Consider upgrading to compatible Mac

#### Cause 3: Device Passcode Not Set

**Check:**
1. System Preferences → Security & Privacy → General
2. Look for "Require password" setting

**Solution:**
```
1. System Preferences → Security & Privacy
2. Click lock icon to make changes
3. Enable "Require password [immediately] after sleep or screen saver begins"
4. Set a password if prompted
5. Retry Touch ID enrollment
```

---

### 2. "Biometry Not Enrolled"

**Symptoms:**
```
Error: Touch ID not available.

Possible reasons:
  - Touch ID not set up in System Preferences
```

**Diagnosis:**
```bash
system_profiler SPHardwareDataType | grep "Touch ID"
```

**Solution:**

1. **Add Fingerprint:**
   ```
   System Preferences → Touch ID
   Click "+" button
   Follow finger placement instructions
   Name your fingerprint (e.g., "Right Thumb")
   ```

2. **Enable Touch ID for Unlocking:**
   ```
   System Preferences → Touch ID
   Check "Unlocking your Mac"
   ```

3. **Test:**
   ```bash
   sparkpass device test
   # Should prompt for Touch ID
   ```

---

### 3. "Biometry Lockout"

**Symptoms:**
```
Error: Touch ID not available.

Possible reasons:
  - Too many failed Touch ID attempts (locked out)
```

**Diagnosis:**
```bash
sparkpass device test
# Shows "BiometryLockout" error
```

**Cause:**
- 5 consecutive failed Touch ID authentication attempts

**Solution:**

**Option 1: Wait and Retry**
```
1. Wait 30 seconds
2. Retry: sparkpass device test
3. If still locked, proceed to Option 2
```

**Option 2: Unlock with Passcode**
```
1. Lock your Mac (Cmd + Ctrl + Q)
2. Unlock using device password (NOT Touch ID)
3. This resets Touch ID lockout counter
4. Retry: sparkpass device enroll vault.spass
```

**Option 3: Restart Mac**
```
1. Restart your Mac
2. Log in with password
3. Touch ID will be re-enabled
4. Retry enrollment
```

---

### 4. "Failed to Store Device Secret in Keychain"

**Symptoms:**
```
Error: Failed to store device secret in Keychain.

Possible reasons:
  - Keychain access denied
  - Keychain is locked
```

**Diagnosis:**
```bash
# Check Keychain status
security show-keychain-info ~/Library/Keychains/login.keychain-db
```

**Causes & Solutions:**

#### Cause 1: Keychain Access Permission Denied

**Symptoms:**
- macOS prompts: "sparkpass wants to access keychain"
- User clicked "Deny"

**Solution:**
```
1. Retry enrollment: sparkpass device enroll vault.spass
2. When prompted "sparkpass wants to access keychain":
   Click "Always Allow" (recommended)
   OR "Allow" (will prompt each time)
3. Complete enrollment
```

#### Cause 2: Keychain Locked

**Check:**
```bash
security show-keychain-info
# Output shows "lock-on-sleep" or "timeout"
```

**Solution:**
```
1. Unlock Keychain:
   security unlock-keychain ~/Library/Keychains/login.keychain-db

2. Or via Keychain Access app:
   Applications → Utilities → Keychain Access
   Select "login" keychain
   File → Unlock Keychain "login"

3. Retry enrollment
```

#### Cause 3: Keychain Corrupted

**Symptoms:**
- `security` commands fail
- Keychain Access shows errors

**Solution (Advanced):**
```
WARNING: This resets your keychain. Back up first!

1. Quit all applications

2. Backup existing keychain:
   cp -R ~/Library/Keychains ~/Desktop/Keychain-Backup

3. Reset keychain:
   rm ~/Library/Keychains/login.keychain-db
   security create-keychain -p "" login.keychain-db

4. Log out and log back in

5. Re-add Touch ID fingerprints

6. Retry SparkPass enrollment
```

---

### 5. "Authentication Failed" During Enrollment

**Symptoms:**
```
Touch ID verified

Enter vault passphrase to complete enrollment:
[password input]
Error: Failed to open vault (incorrect passphrase or vault error).
```

**Diagnosis:**
```bash
# Test passphrase separately
sparkpass unlock vault.spass
```

**Causes & Solutions:**

#### Cause 1: Incorrect Passphrase

**Solution:**
```
1. Verify you know the correct passphrase
2. Try unlocking with passphrase only:
   sparkpass unlock vault.spass
3. If successful, retry enrollment:
   sparkpass device enroll vault.spass
```

#### Cause 2: Vault File Corrupted

**Check:**
```bash
sparkpass doctor vault.spass
```

**Solution:**
- If integrity check fails, restore from backup
- Or use recovery mechanism:
  ```bash
  sparkpass import vault.spass vault.spass.recovery
  ```

---

### 6. "Touch ID Doesn't Work After Enrollment"

**Symptoms:**
- Enrollment succeeds
- `unlock` still prompts for passphrase (no Touch ID)

**Diagnosis:**
```bash
sparkpass device status vault.spass
```

**Causes & Solutions:**

#### Cause 1: TTL Expired

**Check:**
```bash
sparkpass device status vault.spass
# Shows "Expired"
```

**Solution:**
```
Re-enroll to refresh timestamp:
sparkpass device enroll vault.spass
```

#### Cause 2: Keychain Entry Missing

**Check:**
```bash
security find-generic-password -s "com.sparkpass.vault" -a "$(basename vault.spass)"
```

**Solution:**
```
If "The specified item could not be found in the keychain":

1. Re-enroll Touch ID:
   sparkpass device enroll vault.spass

2. Verify Keychain entry created:
   security find-generic-password -s "com.sparkpass.vault"
```

#### Cause 3: macOS Updated and Reset Touch ID

**Symptoms:**
- Touch ID was working
- Stopped after macOS update

**Solution:**
```
1. Re-enable Touch ID in System Preferences:
   System Preferences → Touch ID
   Re-add fingerprints if needed

2. Re-enroll vault:
   sparkpass device enroll vault.spass
```

---

### 7. "Vault Moves to Different Machine - Touch ID Stops Working"

**Symptoms:**
- Vault file copied to new Mac
- Touch ID doesn't work
- Passphrase still works

**This is Expected Behavior:**

**Explanation:**
- Touch ID is device-specific (by design)
- Device secrets don't sync across machines
- This ensures security (no cloud-based attacks)

**Solution:**
```
1. On new machine, unlock with passphrase:
   sparkpass unlock vault.spass

2. Enroll Touch ID on new machine:
   sparkpass device enroll vault.spass

3. Original machine still has its own enrollment
   (both can coexist independently)
```

---

### 8. "LAContext Creation Failed"

**Symptoms:**
```
Error: Failed to create LAContext. Touch ID not available.
```

**Diagnosis:**
```bash
# Check LocalAuthentication framework
otool -L $(which sparkpass) | grep LocalAuthentication
```

**Causes & Solutions:**

#### Cause 1: macOS Too Old

**Check:**
```bash
sw_vers
# ProductVersion: 10.11.x or earlier
```

**Solution:**
- Touch ID requires macOS 10.12 (Sierra) or later
- Upgrade macOS, or use passphrase-only mode

#### Cause 2: Binary Not Signed

**Check:**
```bash
codesign -dv $(which sparkpass)
```

**Solution:**
```
Re-sign sparkpass binary:
codesign --sign - --force --deep $(which sparkpass)

# Or rebuild from source:
cd SparkPass
gprbuild sparkpass.gpr
```

---

### 9. "Touch ID Prompt Doesn't Appear"

**Symptoms:**
- `device enroll` runs
- No Touch ID prompt appears
- Command hangs or times out

**Diagnosis:**
```bash
# Test Touch ID in terminal
cat > test_touchid.sh << 'EOF'
#!/bin/bash
osascript -e 'tell app "System Events" to display dialog "Test Touch ID" buttons {"OK"}'
EOF
chmod +x test_touchid.sh
./test_touchid.sh
```

**Causes & Solutions:**

#### Cause 1: Touch ID Disabled for Terminal

**Check:**
```
System Preferences → Security & Privacy → Privacy → Automation
Look for Terminal.app or iTerm.app
```

**Solution:**
```
1. System Preferences → Security & Privacy → Privacy
2. Select "Automation" on left
3. Enable Touch ID for Terminal/iTerm
4. Retry enrollment
```

#### Cause 2: Running via SSH

**Symptoms:**
- Connected via SSH
- No physical console access

**Solution:**
- Touch ID requires local console (no SSH support)
- Log in locally and run command
- Or use passphrase-only mode

---

### 10. "Cache Expired - Re-enrollment Every 7 Days"

**Symptoms:**
```
sparkpass device status vault.spass
# Shows "Expired"
```

**This is Expected Behavior:**

**Explanation:**
- Touch ID cache expires after 7 days (security feature)
- Prevents stale device secrets
- Re-enrollment updates timestamp

**Solution:**

**Option 1: Re-enroll**
```bash
sparkpass device enroll vault.spass
# Refreshes 7-day window
```

**Option 2: Use Passphrase**
```bash
# Passphrase always works (software-only guarantee)
sparkpass unlock vault.spass
```

**Option 3: Disable Touch ID**
```bash
# If re-enrollment is too frequent
sparkpass device unenroll vault.spass
# Use passphrase-only mode
```

---

## Advanced Diagnostics

### Check LAContext Framework

```bash
# Test LAContext availability
swift -e 'import LocalAuthentication; print("LAContext available")'
```

Expected output: `LAContext available`

### Check Touch ID Hardware

```bash
# macOS system report
system_profiler SPiBridgeDataType
```

Look for "Touch ID" or "Biometric Coprocessor" sections.

### Inspect Keychain Entry

```bash
# Find SparkPass Keychain entries
security find-generic-password -s "com.sparkpass.vault"

# Dump all attributes
security find-generic-password -s "com.sparkpass.vault" -g

# Delete stale entry (use with caution)
security delete-generic-password -s "com.sparkpass.vault" -a "<vault_uuid>"
```

### Enable Debug Logging

```bash
# Set environment variable for verbose output
export SPARKPASS_DEBUG=1
sparkpass device enroll vault.spass --verbose
```

### Check File Permissions

```bash
# Vault file must be 0600
ls -l vault.spass
# Should show: -rw------- (owner read/write only)

# Fix if needed
chmod 600 vault.spass
```

---

## Error Code Reference

| Error Code | Meaning | Solution |
|------------|---------|----------|
| `LAError_AuthenticationFailed (-1)` | User failed Touch ID | Retry authentication |
| `LAError_UserCancel (-2)` | User cancelled Touch ID prompt | Retry enrollment |
| `LAError_PasscodeNotSet (-5)` | No device passcode | Set passcode in System Preferences |
| `LAError_BiometryNotAvailable (-6)` | No Touch ID hardware | Use passphrase-only mode |
| `LAError_BiometryNotEnrolled (-7)` | No fingerprints enrolled | Add fingerprints in System Preferences |
| `LAError_BiometryLockout (-8)` | Too many failed attempts | Wait 30s or unlock with passcode |
| `errSecDuplicateItem (-25299)` | Keychain entry exists | Re-enrollment overwrites automatically |
| `errSecItemNotFound (-25300)` | Keychain entry missing | Enroll Touch ID |
| `errSecAuthFailed (-25293)` | Keychain access denied | Allow keychain access when prompted |

---

## Platform-Specific Issues

### macOS 10.12 - 10.14 (Sierra/Mojave)

**Issue**: Keychain prompt doesn't show
**Solution**:
```
System Preferences → Security & Privacy → Privacy → Accessibility
Add Terminal.app
```

### macOS 10.15+ (Catalina and later)

**Issue**: "sparkpass cannot be opened because the developer cannot be verified"
**Solution**:
```bash
# Remove quarantine attribute
xattr -d com.apple.quarantine $(which sparkpass)

# Or allow in Security preferences
System Preferences → Security & Privacy → General
Click "Allow Anyway" next to sparkpass warning
```

### macOS 11+ (Big Sur and later)

**Issue**: Touch ID works but Face ID fails
**Solution**:
- Face ID requires camera privacy permissions
- System Preferences → Security & Privacy → Privacy → Camera
- Enable camera for sparkpass

### Apple Silicon Macs (M1/M2/M3)

**Issue**: Rosetta 2 compatibility
**Solution**:
- SparkPass should be compiled natively for ARM64
- Check architecture:
  ```bash
  file $(which sparkpass)
  # Should show: Mach-O 64-bit executable arm64
  ```
- If x86_64 (Intel): Rebuild for ARM64

---

## Getting Help

If you've tried all troubleshooting steps and still have issues:

1. **Collect Diagnostic Information:**
   ```bash
   sparkpass device test --verbose > diagnostics.txt
   sw_vers >> diagnostics.txt
   system_profiler SPHardwareDataType >> diagnostics.txt
   ```

2. **Check Existing Issues:**
   - GitHub Issues: https://github.com/sicarii/sparkpass/issues
   - Search for your error message

3. **File a Bug Report:**
   - Include diagnostic output
   - Describe expected vs actual behavior
   - Mention macOS version and Mac model

4. **Community Support:**
   - Discussions: https://github.com/sicarii/sparkpass/discussions
   - Ask questions, share solutions

---

## Fallback: Passphrase-Only Mode

**If Touch ID continues to fail**, SparkPass always works with passphrase alone:

```bash
# Unlock with passphrase (no Touch ID)
sparkpass unlock vault.spass

# Disable Touch ID entirely
sparkpass device unenroll vault.spass

# Continue using passphrase-only mode
# This is the software-only availability guarantee!
```

**Remember**: Touch ID is a convenience feature. Your vault is always accessible with the passphrase on any device.

---

## Preventive Measures

### Before macOS Updates

```bash
# Check Touch ID status
sparkpass device status vault.spass

# Note enrollment details for re-enrollment if needed
```

### Before Hardware Changes

```bash
# Unenroll before logic board replacement
sparkpass device unenroll vault.spass

# Re-enroll after hardware service
sparkpass device enroll vault.spass
```

### Regular Health Checks

```bash
# Weekly status check
sparkpass device test vault.spass

# Re-enroll if expiring soon (< 1 day remaining)
sparkpass device enroll vault.spass
```

---

**Version**: 1.0.0
**Last Updated**: 2025-10-16
**Maintained By**: SparkPass Security Team

For more information, see [TOUCH_ID_INTEGRATION_GUIDE.md](TOUCH_ID_INTEGRATION_GUIDE.md)
