# SparkPass Code Signing Setup Guide

Quick setup guide for configuring Apple Developer ID code signing and notarization for SparkPass.

## Quick Start

If you want to get started immediately, follow these steps:

```bash
# 1. Verify you have Xcode Command Line Tools
xcode-select --install

# 2. Check for existing certificates
security find-identity -v -p codesigning

# 3. If certificates are missing, set them up (see below)
# 4. Configure notarization credentials (see below)

# 5. Build and release
make release VER=1.0.0
```

For detailed documentation, see [NOTARIZATION.md](NOTARIZATION.md).

---

## Prerequisites Checklist

Before you begin, ensure you have:

- [ ] **Apple Developer Program membership** ($99/year)
  - Account type: Individual or Organization
  - Team ID: `E9VB3VKZKH`
  - Organization: AnubisQuantumCipher

- [ ] **macOS 13.0 (Ventura) or later**
  - Check: `sw_vers -productVersion`

- [ ] **Xcode Command Line Tools installed**
  - Install: `xcode-select --install`
  - Verify: `xcode-select -p`

- [ ] **SparkPass builds successfully**
  - Test: `./build.sh` → Creates `bin/sparkpass_main`

---

## Step 1: Certificate Setup

You need two Developer ID certificates:
1. **Developer ID Application** - For signing binaries
2. **Developer ID Installer** - For signing PKG installers

### Option A: Automatic (via Xcode) - Recommended

This is the easiest method:

1. Open **Xcode**
2. Go to **Xcode** → **Preferences** (or **Settings** on newer versions)
3. Click **Accounts** tab
4. Add your Apple ID (lynchmobb@pm.me) if not present
5. Select your Apple ID → Click **Manage Certificates...**
6. Click **+** → Select **Developer ID Application**
7. Click **+** → Select **Developer ID Installer**
8. Close the dialog - certificates are now installed!

### Option B: Manual (via Apple Developer Portal)

If you prefer manual control:

#### 1. Generate Certificate Signing Request (CSR)

```bash
cd /Users/sicarii/SparkPass
./scripts/generate-csr.sh
```

This creates `DeveloperID_CSR.csr` and stores the private key in your keychain.

#### 2. Request Developer ID Application Certificate

1. Go to [Apple Developer Certificates](https://developer.apple.com/account/resources/certificates/list)
2. Click **+** to create a new certificate
3. Select **Developer ID Application**
4. Upload `DeveloperID_CSR.csr`
5. Download the certificate: `developerID_application.cer`

#### 3. Request Developer ID Installer Certificate

1. Click **+** again
2. Select **Developer ID Installer**
3. Upload the same `DeveloperID_CSR.csr`
4. Download the certificate: `developerID_installer.cer`

#### 4. Install Certificates

```bash
./scripts/install-certificate.sh ~/Downloads/developerID_application.cer
./scripts/install-certificate.sh ~/Downloads/developerID_installer.cer
```

Or double-click each `.cer` file to install manually.

### Verify Certificate Installation

```bash
security find-identity -v -p codesigning
```

**Expected output:**
```
1) ABC123... "Developer ID Application: AnubisQuantumCipher (E9VB3VKZKH)"
2) DEF456... "Developer ID Installer: AnubisQuantumCipher (E9VB3VKZKH)"
```

If you see both certificates, you're ready to proceed!

---

## Step 2: Notarization Credentials

Apple requires notarization for all software distributed outside the Mac App Store.

### Option A: App-Specific Password (Easiest for Local Development)

#### 1. Generate App-Specific Password

1. Go to [appleid.apple.com](https://appleid.apple.com)
2. Sign in with your Apple ID: lynchmobb@pm.me
3. Navigate to **Security** → **App-Specific Passwords**
4. Click **+** or **Generate an app-specific password**
5. Label: "SparkPass Notarization"
6. Copy the password (format: `xxxx-xxxx-xxxx-xxxx`)

**Important**: Store this password securely. You'll need it in the next step.

#### 2. Configure Notarization Profile

```bash
xcrun notarytool store-credentials anubis-notary \
  --apple-id lynchmobb@pm.me \
  --team-id E9VB3VKZKH \
  --password "xxxx-xxxx-xxxx-xxxx"
```

Replace `xxxx-xxxx-xxxx-xxxx` with your actual app-specific password.

#### 3. Verify Configuration

```bash
xcrun notarytool history --keychain-profile anubis-notary
```

If you see a list (even if empty), the profile is configured correctly!

### Option B: App Store Connect API Key (Best for CI/CD)

#### 1. Create API Key

1. Go to [App Store Connect](https://appstoreconnect.apple.com)
2. Click **Users and Access**
3. Click **Keys** tab (under Integrations)
4. Click **+** to generate a new key
5. Name: "SparkPass Notarization"
6. Access: **Developer** (minimum)
7. Click **Generate**
8. Download the key: `AuthKey_XXXXXXXXXX.p8`
9. Note the **Key ID** and **Issuer ID**

**Important**: The `.p8` file can only be downloaded once. Store it securely!

#### 2. Configure API Key Profile

```bash
xcrun notarytool store-credentials anubis-notary \
  --key ~/Downloads/AuthKey_XXXXXXXXXX.p8 \
  --key-id XXXXXXXXXX \
  --issuer-id xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
```

Replace:
- `AuthKey_XXXXXXXXXX.p8` - Your downloaded API key file
- `XXXXXXXXXX` - Your Key ID (shown in App Store Connect)
- `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` - Your Issuer ID (shown in App Store Connect)

#### 3. Verify Configuration

```bash
xcrun notarytool history --keychain-profile anubis-notary
```

---

## Step 3: Test Local Signing

Before creating a release, test that code signing works:

```bash
# Build SparkPass
make build

# Sign binary (no notarization)
make sign

# Verify signature
codesign --verify --deep --strict bin/sparkpass_main
```

**Expected output:**
```
bin/sparkpass_main: valid on disk
bin/sparkpass_main: satisfies its Designated Requirement
```

If you see this, code signing is working!

---

## Step 4: Create Your First Release

Now you're ready to create a signed, notarized release:

```bash
# Full release workflow (build + sign + notarize + staple)
make release VER=1.0.0
```

This will:
1. Build `bin/sparkpass_main`
2. Sign with Hardened Runtime
3. Create ZIP: `dist/sparkpass-1.0.0-macos-arm64.zip`
4. Notarize ZIP (waits for Apple approval: 2-10 minutes)
5. Create PKG: `dist/sparkpass-1.0.0-installer.pkg`
6. Sign PKG with Developer ID Installer
7. Notarize PKG (waits for Apple approval: 2-10 minutes)
8. Staple notarization ticket to PKG
9. Verify all signatures

**Expected duration**: 5-20 minutes (depends on Apple's notarization service)

### Verify Release

```bash
# Verify signatures and notarization
make verify
```

**Expected output:**
```
[PASS] Signature is valid
[PASS] Hardened Runtime is ENABLED
[PASS] Gatekeeper ACCEPTS this binary
[PASS] PKG signature valid
[PASS] The staple and validate action worked
```

---

## Step 5: Test Installation

Test the PKG installer locally:

```bash
# Install (requires sudo)
sudo installer -pkg dist/sparkpass-1.0.0-installer.pkg -target /

# Verify installation
which sparkpass
# Expected: /usr/local/bin/sparkpass

sparkpass --version
# Expected: SparkPass version 1.0.0

sparkpass --help
```

If this works, your release is ready for distribution!

---

## Step 6: Upload to GitHub Release

### Option A: GitHub CLI (Recommended)

```bash
# Create GitHub release
gh release create v1.0.0 \
  --title "SparkPass v1.0.0" \
  --notes "Quantum-resistant password manager for macOS" \
  dist/sparkpass-1.0.0-macos-arm64.zip \
  dist/sparkpass-1.0.0-installer.pkg \
  dist/checksums.txt
```

### Option B: GitHub Web Interface

1. Go to your repository on GitHub
2. Click **Releases** → **Draft a new release**
3. Tag: `v1.0.0`
4. Title: `SparkPass v1.0.0`
5. Attach files:
   - `dist/sparkpass-1.0.0-macos-arm64.zip`
   - `dist/sparkpass-1.0.0-installer.pkg`
   - `dist/checksums.txt`
6. Click **Publish release**

---

## Step 7: CI/CD Setup (Optional)

To automate releases with GitHub Actions:

### 1. Export Certificates for CI/CD

```bash
# Export Developer ID Application certificate
security export -k login.keychain-db \
  -t identities \
  -f pkcs12 \
  -P "" \
  -o /tmp/devid_app.p12 \
  "Developer ID Application: AnubisQuantumCipher (E9VB3VKZKH)"

# Export Developer ID Installer certificate
security export -k login.keychain-db \
  -t identities \
  -f pkcs12 \
  -P "" \
  -o /tmp/devid_installer.p12 \
  "Developer ID Installer: AnubisQuantumCipher (E9VB3VKZKH)"

# Encode to base64 for GitHub Secrets
base64 -i /tmp/devid_app.p12 | pbcopy
# Paste this as MACOS_CERTIFICATE_APP in GitHub Secrets

base64 -i /tmp/devid_installer.p12 | pbcopy
# Paste this as MACOS_CERTIFICATE_INSTALLER in GitHub Secrets

# Clean up
rm /tmp/devid_app.p12 /tmp/devid_installer.p12
```

### 2. Add GitHub Secrets

Go to your repository → **Settings** → **Secrets and variables** → **Actions** → **New repository secret**

**For App-Specific Password:**
- `MACOS_CERTIFICATE_APP` - Base64-encoded Developer ID Application .p12
- `MACOS_CERTIFICATE_INSTALLER` - Base64-encoded Developer ID Installer .p12
- `MACOS_NOTARIZATION_APPLE_ID` = `lynchmobb@pm.me`
- `MACOS_NOTARIZATION_TEAM_ID` = `E9VB3VKZKH`
- `MACOS_NOTARIZATION_PASSWORD` = Your app-specific password

**For API Key:**
- `MACOS_CERTIFICATE_APP` - Base64-encoded Developer ID Application .p12
- `MACOS_CERTIFICATE_INSTALLER` - Base64-encoded Developer ID Installer .p12
- `MACOS_NOTARIZATION_API_KEY` - Contents of `AuthKey_XXXXXXXXXX.p8` (paste raw text)
- `MACOS_NOTARIZATION_KEY_ID` = Your Key ID
- `MACOS_NOTARIZATION_ISSUER_ID` = Your Issuer ID

### 3. Trigger a Release

```bash
# Via GitHub web interface:
# Go to Actions → Release → Run workflow
# Enter version: 1.0.0

# Or via GitHub CLI:
gh workflow run release.yml -f version=1.0.0
```

The workflow will automatically:
- Build SparkPass
- Sign with Developer ID
- Notarize with Apple
- Staple tickets
- Create GitHub Release
- Upload artifacts

---

## Troubleshooting

### "Developer ID certificate not found"

**Solution:**
```bash
# List installed certificates
security find-identity -v -p codesigning

# If empty, follow Step 1 again
```

### "Notarization profile not found"

**Solution:**
```bash
# Reconfigure notarization credentials
xcrun notarytool store-credentials anubis-notary \
  --apple-id lynchmobb@pm.me \
  --team-id E9VB3VKZKH \
  --password "YOUR_APP_SPECIFIC_PASSWORD"
```

### Notarization fails with "Invalid"

**Solution:**
```bash
# Check notarization log
xcrun notarytool history --keychain-profile anubis-notary
xcrun notarytool log SUBMISSION_ID --keychain-profile anubis-notary

# Common issues:
# - Missing Hardened Runtime (ensure --options runtime is used)
# - Invalid entitlements (check signing/sparkpass.entitlements)
# - Unsigned libraries (all dependencies must be signed)
```

### "cannot be opened because the developer cannot be verified"

**Solution:**
1. Verify notarization: `spctl -a -vvv bin/sparkpass_main`
2. If rejected, re-notarize: `make release VER=1.0.0`
3. If accepted, remove quarantine: `xattr -d com.apple.quarantine bin/sparkpass_main`

For more troubleshooting, see [NOTARIZATION.md](NOTARIZATION.md#troubleshooting).

---

## Quick Reference

### Daily Development Commands

```bash
# Build and test locally
make build
./bin/sparkpass_main --help

# Sign for local testing
make sign

# Run test suite
make test

# Install locally (unsigned)
make install
```

### Release Commands

```bash
# Create signed and notarized release
make release VER=1.0.0

# Verify signatures
make verify

# Generate checksums
cd dist && shasum -a 256 *.zip *.pkg > checksums.txt

# Upload to GitHub
gh release create v1.0.0 dist/*.zip dist/*.pkg dist/checksums.txt
```

### Verification Commands

```bash
# Verify binary signature
codesign --verify --deep --strict bin/sparkpass_main

# Verify PKG signature
pkgutil --check-signature dist/sparkpass-1.0.0-installer.pkg

# Verify stapled ticket
xcrun stapler validate dist/sparkpass-1.0.0-installer.pkg

# Check Gatekeeper acceptance
spctl -a -vvv -t install dist/sparkpass-1.0.0-installer.pkg
```

---

## Resources

- **Detailed Documentation**: [NOTARIZATION.md](NOTARIZATION.md)
- **Apple Developer Portal**: https://developer.apple.com/account/
- **App Store Connect**: https://appstoreconnect.apple.com
- **Notarization Guide**: https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution

---

## Support

If you encounter issues:

1. Check [NOTARIZATION.md](NOTARIZATION.md#troubleshooting) for detailed troubleshooting
2. Review verification script output: `./scripts/verify-signature.sh bin/sparkpass_main`
3. Check Apple System Status: https://developer.apple.com/system-status/
4. Open an issue: https://github.com/AnubisQuantumCipher/SparkPass/issues

---

**Last Updated**: 2025-10-16
**SparkPass Version**: 1.0.0
