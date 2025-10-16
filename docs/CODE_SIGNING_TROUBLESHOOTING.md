# SparkPass Code Signing and Notarization Troubleshooting

## Common Issues and Solutions

### Issue 1: "No signing identity found"

**Symptom:**
```
ERROR: No signing identity specified!
```

**Cause:** Developer ID certificate not installed or not accessible in keychain.

**Solution:**

1. **Check for certificates:**
   ```bash
   security find-identity -v -p codesigning
   ```

2. **If no certificates shown:**
   - Generate CSR: `./scripts/generate-csr.sh`
   - Request certificate from Apple Developer portal
   - Install certificate: `./scripts/install-certificate.sh path/to/cert.cer`

3. **If certificate exists but not found:**
   ```bash
   # Check which keychain contains the certificate
   security find-identity -v -p codesigning -s "Developer ID"

   # Unlock login keychain
   security unlock-keychain ~/Library/Keychains/login.keychain-db
   ```

4. **Set signing identity:**
   ```bash
   export SPARKPASS_SIGNING_IDENTITY="Developer ID Application: Your Name (TEAM12345)"
   ```

---

### Issue 2: Code signing fails with "errSecInternalComponent"

**Symptom:**
```
/Users/sicarii/SparkPass/bin/sparkpass_main: errSecInternalComponent
```

**Cause:** Keychain access issues or certificate private key not accessible.

**Solution:**

1. **Unlock keychain:**
   ```bash
   security unlock-keychain ~/Library/Keychains/login.keychain-db
   ```

2. **Allow codesign to access the key:**
   ```bash
   security set-key-partition-list -S apple-tool:,apple: \
       -s -k YOUR_KEYCHAIN_PASSWORD \
       ~/Library/Keychains/login.keychain-db
   ```

3. **Verify certificate has private key:**
   ```bash
   security find-certificate -c "Developer ID Application" -p | openssl x509 -noout -text
   ```

   Should show: `Subject: CN=Developer ID Application: Your Name (TEAM12345)`

---

### Issue 3: Notarization rejected - "Invalid Binary"

**Symptom:**
```
notarytool: status: Invalid
"issues": [{"severity": "error", "message": "The binary is not signed with a valid Developer ID certificate."}]
```

**Cause:** Binary not signed with Hardened Runtime, or signature invalid.

**Solution:**

1. **Verify signature:**
   ```bash
   codesign --verify --verbose=4 bin/sparkpass_main
   ```

2. **Check for Hardened Runtime:**
   ```bash
   codesign --display --verbose bin/sparkpass_main | grep runtime
   ```

   Should show: `flags=0x10000(runtime)`

3. **Re-sign with Hardened Runtime:**
   ```bash
   codesign --sign "$SIGNING_IDENTITY" \
       --options runtime \
       --entitlements signing/sparkpass.entitlements \
       --force \
       --timestamp \
       bin/sparkpass_main
   ```

---

### Issue 4: Notarization rejected - "Invalid Signature"

**Symptom:**
```
"issues": [{"severity": "error", "message": "The signature does not include a secure timestamp."}]
```

**Cause:** Missing `--timestamp` flag during signing.

**Solution:**

Always include `--timestamp` flag:

```bash
codesign --sign "$SIGNING_IDENTITY" \
    --options runtime \
    --entitlements signing/sparkpass.entitlements \
    --force \
    --timestamp \
    bin/sparkpass_main
```

**Troubleshooting timestamp server issues:**

If timestamp server is unreachable:
```bash
# Try again later or check network connection
# Apple's timestamp server: http://timestamp.apple.com/ts01
curl -I http://timestamp.apple.com/ts01
```

---

### Issue 5: Binary crashes with "killed: 9" (Code signature invalid)

**Symptom:**
```
$ ./sparkpass_main
Killed: 9
```

**Cause:** Code signature broken, often after modifying binary post-signing.

**Solution:**

1. **Check signature:**
   ```bash
   codesign --verify --verbose=4 bin/sparkpass_main
   ```

   If shows errors, re-sign:
   ```bash
   ./scripts/sign-and-notarize.sh
   ```

2. **Common causes of broken signatures:**
   - Running `strip` on signed binary
   - Modifying binary with `install_name_tool` after signing
   - Copying binary incorrectly (use `ditto` or `cp -p`)

**Prevention:** Always sign AFTER all binary modifications.

---

### Issue 6: "The executable requests the com.apple.security.cs.disable-library-validation entitlement"

**Symptom:**
Notarization log shows warning about library validation entitlement.

**Cause:** This is expected for SparkPass due to Homebrew dependencies.

**Solution:**

This is **not an error**, just a warning. Apple allows this entitlement for legitimate use cases.

**Why it's safe:**
- SparkPass loads specific Homebrew libraries (liboqs, openssl, libsodium)
- These are well-known, trusted libraries
- Users install via Homebrew (trusted source)

**If you want to eliminate the warning:**
- Bundle and sign all dependencies (see DISTRIBUTION_STRATEGIES.md)
- Use static linking (larger binary, more complex build)

For SparkPass, **keeping the entitlement is recommended** for simplicity.

---

### Issue 7: Gatekeeper blocks execution - "cannot be opened because the developer cannot be verified"

**Symptom:**
```
"sparkpass" cannot be opened because the developer cannot be verified.
```

**Cause:** Binary not notarized, or notarization ticket not available.

**Solution:**

1. **Verify notarization:**
   ```bash
   spctl --assess --verbose=4 bin/sparkpass_main
   ```

   Should show: `source=Notarized Developer ID`

2. **If not notarized:**
   ```bash
   ./scripts/sign-and-notarize.sh
   ```

3. **For testing only (bypass Gatekeeper):**
   ```bash
   # WARNING: Only for local testing!
   sudo spctl --master-disable  # Disable Gatekeeper globally (NOT RECOMMENDED)

   # Or allow specific binary:
   xattr -d com.apple.quarantine bin/sparkpass_main
   ```

4. **For distribution:** Always notarize before releasing.

---

### Issue 8: Notarization submission times out

**Symptom:**
```
notarytool: Waiting for notarization to complete...
Error: Timeout waiting for notarization
```

**Cause:** Apple's notarization service is slow or overloaded.

**Solution:**

1. **Use `--wait` flag with longer timeout:**
   ```bash
   xcrun notarytool submit sparkpass.zip \
       --keychain-profile sparkpass-notarization \
       --wait \
       --timeout 3600
   ```

2. **Check status manually:**
   ```bash
   # Get recent submissions
   xcrun notarytool history --keychain-profile sparkpass-notarization

   # Check specific submission
   xcrun notarytool info SUBMISSION_ID --keychain-profile sparkpass-notarization
   ```

3. **Typical wait times:**
   - Small binaries (<10MB): 2-5 minutes
   - Medium binaries (10-50MB): 5-15 minutes
   - Large packages: 15-60 minutes

---

### Issue 9: "Team ID in certificate does not match Team ID in notarization profile"

**Symptom:**
```
Error: The provided credentials do not match the Team ID in the certificate.
```

**Cause:** Notarization profile Team ID doesn't match certificate Team ID.

**Solution:**

1. **Check certificate Team ID:**
   ```bash
   security find-certificate -c "Developer ID Application" -p | \
       openssl x509 -noout -text | \
       grep "OU="
   ```

   Shows: `OU = ABCD123456` (this is your Team ID)

2. **Re-create notarization profile with correct Team ID:**
   ```bash
   ./scripts/setup-notarization.sh
   # Enter the correct Team ID when prompted
   ```

3. **Or specify Team ID explicitly:**
   ```bash
   xcrun notarytool submit sparkpass.zip \
       --apple-id "your@email.com" \
       --team-id "ABCD123456" \
       --password "xxxx-xxxx-xxxx-xxxx" \
       --wait
   ```

---

### Issue 10: Dynamic library not found at runtime

**Symptom:**
```
dyld[12345]: Library not loaded: /opt/homebrew/opt/openssl@3/lib/libssl.3.dylib
  Reason: tried: '/opt/homebrew/opt/openssl@3/lib/libssl.3.dylib' (no such file)
Abort trap: 6
```

**Cause:** Homebrew dependencies not installed on target system.

**Solution:**

1. **Install dependencies:**
   ```bash
   brew install liboqs openssl@3 libsodium
   ```

2. **For distribution:** Include dependency requirements in documentation:
   ```markdown
   ## Installation

   ### Prerequisites
   ```bash
   brew install liboqs openssl@3 libsodium
   ```

   ### Install SparkPass
   ```bash
   sudo cp sparkpass /usr/local/bin/
   ```
   ```

3. **Alternative:** Use Homebrew formula (handles dependencies automatically):
   ```bash
   brew tap yourusername/sparkpass
   brew install sparkpass  # Dependencies installed automatically
   ```

---

### Issue 11: Touch ID/Face ID prompts don't appear

**Symptom:**
SparkPass runs but never prompts for Touch ID/Face ID authentication.

**Cause:** Binary not signed with correct entitlements, or LocalAuthentication framework not linked.

**Solution:**

1. **Verify entitlements:**
   ```bash
   codesign --display --entitlements - bin/sparkpass_main
   ```

   Should include:
   ```xml
   <key>com.apple.security.cs.disable-library-validation</key>
   <true/>
   ```

2. **Verify LocalAuthentication framework is linked:**
   ```bash
   otool -L bin/sparkpass_main | grep LocalAuthentication
   ```

   Should show:
   ```
   /System/Library/Frameworks/LocalAuthentication.framework/Versions/A/LocalAuthentication
   ```

3. **Check System Preferences:**
   - Touch ID must be enrolled in System Preferences
   - Test with: `scripts/test_touchid_enroll.exp`

4. **Re-sign with entitlements:**
   ```bash
   codesign --sign "$SIGNING_IDENTITY" \
       --options runtime \
       --entitlements signing/sparkpass.entitlements \
       --force \
       --timestamp \
       bin/sparkpass_main
   ```

---

### Issue 12: App-specific password not working

**Symptom:**
```
Error: Unable to validate credentials. Username or password is incorrect.
```

**Cause:** Using Apple ID password instead of app-specific password.

**Solution:**

1. **Generate app-specific password:**
   - Go to: https://appleid.apple.com/account/manage
   - Sign in
   - Security → App-Specific Passwords → Click '+'
   - Name it "SparkPass Notarization"
   - Copy the password (format: `xxxx-xxxx-xxxx-xxxx`)

2. **DO NOT use:**
   - Your regular Apple ID password
   - Two-factor authentication code

3. **Re-create notarization profile:**
   ```bash
   ./scripts/setup-notarization.sh
   # Enter the app-specific password when prompted
   ```

---

### Issue 13: "The entitlements are invalid"

**Symptom:**
```
Error: The entitlements in the signature are invalid.
```

**Cause:** Malformed entitlements plist or using entitlements not allowed for Developer ID.

**Solution:**

1. **Verify entitlements file syntax:**
   ```bash
   plutil -lint signing/sparkpass.entitlements
   ```

   Should show: `OK`

2. **Check for invalid entitlements:**
   Developer ID allows limited entitlements. Ensure you're not using:
   - App Sandbox (`com.apple.security.app-sandbox` = true)
   - Push notifications
   - iCloud access
   - Other App Store-specific entitlements

3. **Use SparkPass's provided entitlements:**
   ```bash
   # The provided entitlements file is pre-configured correctly
   cat signing/sparkpass.entitlements
   ```

---

### Issue 14: Cannot create notarization profile

**Symptom:**
```
Error: Failed to store credentials in keychain.
```

**Cause:** Keychain access denied or locked.

**Solution:**

1. **Unlock keychain:**
   ```bash
   security unlock-keychain ~/Library/Keychains/login.keychain-db
   ```

2. **Manually create profile:**
   ```bash
   xcrun notarytool store-credentials "sparkpass-notarization" \
       --apple-id "your@email.com" \
       --team-id "TEAM12345" \
       --password "xxxx-xxxx-xxxx-xxxx"
   ```

3. **Verify profile:**
   ```bash
   xcrun notarytool history --keychain-profile sparkpass-notarization
   ```

---

## Debugging Workflow

When facing code signing or notarization issues, follow this diagnostic workflow:

### Step 1: Verify Build
```bash
./build.sh
file bin/sparkpass_main
otool -L bin/sparkpass_main
```

### Step 2: Verify Certificate
```bash
security find-identity -v -p codesigning
security find-certificate -c "Developer ID Application" -p | openssl x509 -noout -text
```

### Step 3: Verify Signing
```bash
codesign --verify --verbose=4 bin/sparkpass_main
codesign --display --verbose=4 bin/sparkpass_main
./scripts/verify-signature.sh
```

### Step 4: Verify Notarization Profile
```bash
xcrun notarytool history --keychain-profile sparkpass-notarization
```

### Step 5: Check Notarization Logs
```bash
# After submission
xcrun notarytool log SUBMISSION_ID --keychain-profile sparkpass-notarization
```

### Step 6: Test Gatekeeper
```bash
spctl --assess --type execute --verbose=4 bin/sparkpass_main
```

---

## Getting Help

### Apple Developer Support
- Developer ID issues: https://developer.apple.com/contact/
- Notarization status page: https://developer.apple.com/system-status/

### Useful Commands Reference

```bash
# List signing identities
security find-identity -v -p codesigning

# Verify signature
codesign --verify --verbose=4 /path/to/binary

# Display signature info
codesign --display --verbose=4 /path/to/binary

# Show entitlements
codesign --display --entitlements - /path/to/binary

# Test Gatekeeper
spctl --assess --type execute --verbose=4 /path/to/binary

# Notarization history
xcrun notarytool history --keychain-profile PROFILE_NAME

# Check notarization status
xcrun notarytool info SUBMISSION_ID --keychain-profile PROFILE_NAME

# Get notarization log
xcrun notarytool log SUBMISSION_ID --keychain-profile PROFILE_NAME output.json

# Remove quarantine (testing only)
xattr -d com.apple.quarantine /path/to/binary

# Add quarantine (testing)
xattr -w com.apple.quarantine "0081;$(date +%s);Safari" /path/to/binary
```

---

## Preventive Measures

### Before Releasing

1. **Test on clean system:**
   - Use a Mac without development tools
   - Install only Homebrew and dependencies
   - Download your distribution package
   - Verify it runs without warnings

2. **Verify signature and notarization:**
   ```bash
   ./scripts/verify-signature.sh dist/sparkpass
   spctl --assess --type execute --verbose=4 dist/sparkpass
   ```

3. **Test with quarantine bit:**
   ```bash
   xattr -w com.apple.quarantine "0081;$(date +%s);Safari" dist/sparkpass
   ./dist/sparkpass --help  # Should work without warnings
   ```

4. **Check dependencies:**
   ```bash
   otool -L dist/sparkpass
   # Verify all paths are correct for target system
   ```

### Maintenance

1. **Certificate expiration:**
   - Developer ID certificates expire every 5 years
   - Set a calendar reminder to renew before expiration
   - Keep backups of certificate and private key

2. **App-specific password rotation:**
   - If compromised, revoke and generate new one
   - Update notarization profile

3. **Monitor Apple changes:**
   - Subscribe to Apple Developer News
   - Test signing workflow after macOS updates

---

## Additional Resources

- [Apple Code Signing Guide](https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/Introduction/Introduction.html)
- [Notarizing macOS Software](https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution)
- [Hardened Runtime](https://developer.apple.com/documentation/security/hardened_runtime)
- [Entitlements](https://developer.apple.com/documentation/bundleresources/entitlements)
