# Apple Developer Program Enrollment Required

SparkPass is ready for code signing and notarization, but you need to enroll in the **Apple Developer Program** first.

## Current Status

- ❌ **Not enrolled in Apple Developer Program**
- ✅ Code signing infrastructure ready
- ✅ Notarization scripts ready
- ✅ GitHub Actions workflow ready
- ✅ Build system configured

## What You Need

**Apple Developer Program Membership**
- **Cost**: $99/year
- **Enroll at**: https://developer.apple.com/programs/enroll/
- **Processing time**: 24-48 hours

## After Enrollment (Once Approved)

### Step 1: Generate CSR

```bash
cd /Users/sicarii/SparkPass
cat > /tmp/csr.conf <<'EOF'
[ req ]
default_bits = 2048
distinguished_name = req_distinguished_name
prompt = no

[ req_distinguished_name ]
emailAddress = lynchmobb@pm.me
CN = AnubisQuantumCipher
EOF

openssl req -new -newkey rsa:2048 -nodes \
  -keyout /tmp/developerID.key \
  -out /tmp/developerID.csr \
  -config /tmp/csr.conf

# Copy CSR to clipboard
pbcopy < /tmp/developerID.csr
```

### Step 2: Create Developer ID Certificates

**Developer ID Application Certificate:**
1. Go to: https://developer.apple.com/account/resources/certificates/add
2. Select **"Developer ID Application"**
3. Upload `/tmp/developerID.csr`
4. Download certificate (save as `~/Downloads/developerID_application.cer`)

**Developer ID Installer Certificate:**
1. Go to: https://developer.apple.com/account/resources/certificates/add
2. Select **"Developer ID Installer"**
3. Upload the SAME `/tmp/developerID.csr`
4. Download certificate (save as `~/Downloads/developerID_installer.cer`)

### Step 3: Install Certificates

```bash
# Import private key to keychain
security import /tmp/developerID.key -k ~/Library/Keychains/login.keychain-db

# Install certificates
open ~/Downloads/developerID_application.cer
open ~/Downloads/developerID_installer.cer

# Verify installation
security find-identity -v -p codesigning
# Should show:
# "Developer ID Application: AnubisQuantumCipher (E9VB3VKZKH)"
# "Developer ID Installer: AnubisQuantumCipher (E9VB3VKZKH)"
```

### Step 4: Configure Notarization

**Option A: App-Specific Password (Easiest)**

1. Go to: https://appleid.apple.com
2. Navigate to **Security** → **App-Specific Passwords**
3. Generate password with label "SparkPass Notarization"
4. Copy the password

```bash
xcrun notarytool store-credentials anubis-notary \
  --apple-id lynchmobb@pm.me \
  --team-id E9VB3VKZKH \
  --password "xxxx-xxxx-xxxx-xxxx"
```

**Option B: API Key (Best for CI/CD)**

1. Go to: https://appstoreconnect.apple.com
2. Navigate to **Users and Access** → **Keys**
3. Generate key (download `AuthKey_XXXXXXXXXX.p8`)
4. Note **Key ID** and **Issuer ID**

```bash
mkdir -p ~/secrets
mv ~/Downloads/AuthKey_*.p8 ~/secrets/

xcrun notarytool store-credentials anubis-notary \
  --key ~/secrets/AuthKey_XXXXXXXXXX.p8 \
  --key-id XXXXXXXXXX \
  --issuer-id xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
```

### Step 5: Create Signed Release

```bash
cd /Users/sicarii/SparkPass
make release VER=1.0.0
```

This will:
1. Build SparkPass with Touch ID support
2. Sign with Developer ID + Hardened Runtime
3. Create ZIP distribution
4. Notarize ZIP with Apple (2-10 minutes)
5. Create PKG installer
6. Sign PKG with Developer ID Installer
7. Notarize PKG with Apple (2-10 minutes)
8. Staple notarization ticket
9. Verify all signatures

Output will be in `dist/`:
- `sparkpass-1.0.0-macos-arm64.zip` (notarized)
- `sparkpass-1.0.0-installer.pkg` (notarized + stapled)

### Step 6: Verify and Distribute

```bash
# Verify signatures
make verify

# Test installation locally
sudo installer -pkg dist/sparkpass-1.0.0-installer.pkg -target /

# Create GitHub release
gh release create v1.0.0 \
  --title "SparkPass v1.0.0" \
  --notes "Quantum-resistant password manager for macOS" \
  dist/sparkpass-1.0.0-macos-arm64.zip \
  dist/sparkpass-1.0.0-installer.pkg
```

## Distribution Without Code Signing (Free Alternative)

If you don't want to pay for Apple Developer Program, you can still distribute SparkPass:

### Create Unsigned Release

```bash
./build.sh
mkdir -p dist
zip dist/sparkpass-1.0.0-unsigned.zip bin/sparkpass_main
```

### Users Must Bypass Gatekeeper

Users will see a warning on first run. They must:

**Method 1: Right-click method**
1. Right-click `sparkpass_main`
2. Click **Open**
3. Click **Open** in the security dialog

**Method 2: Command line**
```bash
xattr -d com.apple.quarantine sparkpass_main
./sparkpass_main --version
```

### Documentation for Users

Add to your README:

```markdown
## Installation (Unsigned Build)

This build is not code signed. macOS will show a security warning.

### macOS Gatekeeper Bypass

After downloading, remove the quarantine flag:

\`\`\`bash
xattr -d com.apple.quarantine sparkpass_main
chmod +x sparkpass_main
./sparkpass_main --version
\`\`\`

Or right-click → Open → Click "Open" in the dialog.
```

## Comparison

| Feature | With Developer ID | Without Code Signing |
|---------|------------------|---------------------|
| Cost | $99/year | Free |
| User experience | No warnings | Security warnings |
| Distribution | Mac App Store, direct download | Direct download only |
| Notarization | Yes | No |
| Gatekeeper | Passes automatically | Users must bypass |
| Professional | Yes | No |
| Recommended for | Production releases | Development/testing |

## Next Steps

**For production distribution**, enroll in Apple Developer Program:
1. Go to: https://developer.apple.com/programs/enroll/
2. Pay $99 (annual)
3. Wait for approval (24-48 hours)
4. Follow steps above to generate certificates
5. Run `make release VER=1.0.0`

**For immediate testing/distribution**, use unsigned builds:
1. Run `./build.sh`
2. Distribute `bin/sparkpass_main` with bypass instructions
3. Document the quarantine removal process for users

## Support

If you encounter issues after enrollment:
- Check: `security find-identity -v -p codesigning`
- Check: `xcrun notarytool history --keychain-profile anubis-notary`
- See: `SIGNING_SETUP.md` for detailed troubleshooting

---

**Repository**: https://github.com/AnubisQuantumCipher/SparkPass
**Contact**: sic.tau@pm.me
