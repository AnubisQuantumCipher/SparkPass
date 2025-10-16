# Apple Developer ID Code Signing and Notarization Guide

Complete guide for signing, notarizing, and distributing SparkPass on macOS with Apple Developer ID certificates.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [One-Time Setup](#one-time-setup)
3. [Certificate Setup](#certificate-setup)
4. [Notarization Credentials](#notarization-credentials)
5. [Build and Release Workflow](#build-and-release-workflow)
6. [Verification](#verification)
7. [Installation Testing](#installation-testing)
8. [CI/CD with GitHub Actions](#cicd-with-github-actions)
9. [Homebrew Distribution](#homebrew-distribution)
10. [Universal Binaries](#universal-binaries)
11. [Troubleshooting](#troubleshooting)
12. [Quick Reference](#quick-reference)

---

## Prerequisites

### Required Software

- **macOS**: 13.0 (Ventura) or later
- **Xcode Command Line Tools**: Install with `xcode-select --install`
- **Apple Developer Program**: $99/year individual or organization membership
- **Build Dependencies**: Alire, GNAT, GPRbuild, liboqs, OpenSSL 3.x, libsodium

### Verify Xcode Tools Installation

```bash
# Check if Xcode Command Line Tools are installed
xcode-select -p
# Expected: /Library/Developer/CommandLineTools or /Applications/Xcode.app/Contents/Developer

# Verify codesign and notarytool are available
which codesign
which xcrun
xcrun notarytool --version
```

### Apple Developer Account Requirements

- **Team ID**: E9VB3VKZKH (AnubisQuantumCipher)
- **Contact Email**: lynchmobb@pm.me
- **Account Type**: Individual or Organization (required for Developer ID certificates)

---

## One-Time Setup

### 1. Install Xcode Command Line Tools

```bash
xcode-select --install
```

If already installed, update to the latest version:

```bash
sudo rm -rf /Library/Developer/CommandLineTools
xcode-select --install
```

### 2. Verify Build System

```bash
cd /Users/sicarii/SparkPass
./build.sh
```

Expected output: `bin/sparkpass_main` (approximately 4.5MB)

### 3. Test Binary Locally

```bash
# Test the unsigned binary locally
./bin/sparkpass_main --help
./bin/sparkpass_main --version
```

---

## Certificate Setup

You need two Developer ID certificates:

1. **Developer ID Application** - For signing binaries and apps
2. **Developer ID Installer** - For signing PKG installers

### Option A: Generate Certificates via Xcode (Easiest)

1. Open **Xcode**
2. Go to **Preferences** → **Accounts**
3. Add your Apple ID if not already present
4. Select your Apple ID → Click **Manage Certificates**
5. Click **+** → Select **Developer ID Application**
6. Click **+** → Select **Developer ID Installer**
7. Certificates will be automatically installed in your keychain

### Option B: Generate Certificates via Apple Developer Portal (Manual)

#### Step 1: Generate Certificate Signing Request (CSR)

Use the provided script:

```bash
cd /Users/sicarii/SparkPass
./scripts/generate-csr.sh
```

This creates:
- `DeveloperID_CSR.csr` - Submit this to Apple
- Private key stored in Keychain (name: "SparkPass Developer ID")

Or generate manually:

```bash
# Open Keychain Access
open "/Applications/Utilities/Keychain Access.app"

# Menu: Keychain Access → Certificate Assistant → Request a Certificate from a Certificate Authority
# Fill in:
#   User Email Address: lynchmobb@pm.me
#   Common Name: AnubisQuantumCipher
#   CA Email: Leave empty
#   Request: "Saved to disk"
#   Let me specify key pair information: Yes
#   Key Size: 2048 bits
#   Algorithm: RSA
```

#### Step 2: Request Certificates from Apple

1. Go to [Apple Developer Certificates](https://developer.apple.com/account/resources/certificates/list)
2. Click **+** to create a new certificate

**For Developer ID Application Certificate:**
- Select **Developer ID Application**
- Upload your CSR file
- Download the certificate (e.g., `developerID_application.cer`)

**For Developer ID Installer Certificate:**
- Repeat the process
- Select **Developer ID Installer**
- Upload the same CSR file
- Download the certificate (e.g., `developerID_installer.cer`)

#### Step 3: Install Certificates

```bash
# Install both certificates
./scripts/install-certificate.sh /path/to/developerID_application.cer
./scripts/install-certificate.sh /path/to/developerID_installer.cer
```

Or install manually by double-clicking each `.cer` file.

### Verify Certificate Installation

```bash
# List all code signing identities
security find-identity -v -p codesigning

# You should see both certificates:
# 1) ABC123... "Developer ID Application: AnubisQuantumCipher (E9VB3VKZKH)"
# 2) DEF456... "Developer ID Installer: AnubisQuantumCipher (E9VB3VKZKH)"
```

**Important**: If you see "not valid before" or "not valid after" errors, your system clock may be wrong or the certificate has expired. Developer ID certificates are valid for 5 years.

---

## Notarization Credentials

Apple requires notarization for all software distributed outside the Mac App Store. You need to configure credentials for `xcrun notarytool`.

### Option A: App-Specific Password (Recommended for Individuals)

#### Step 1: Generate App-Specific Password

1. Go to [appleid.apple.com](https://appleid.apple.com)
2. Sign in with your Apple ID
3. Navigate to **Security** → **App-Specific Passwords**
4. Click **Generate an app-specific password**
5. Label: "SparkPass Notarization"
6. Copy the generated password (format: `xxxx-xxxx-xxxx-xxxx`)

#### Step 2: Store Credentials in Keychain

```bash
xcrun notarytool store-credentials anubis-notary \
  --apple-id lynchmobb@pm.me \
  --team-id E9VB3VKZKH \
  --password "xxxx-xxxx-xxxx-xxxx"
```

Replace `xxxx-xxxx-xxxx-xxxx` with your actual app-specific password.

**Profile name**: `anubis-notary` (hardcoded in scripts)

#### Step 3: Verify Credentials

```bash
xcrun notarytool history --keychain-profile anubis-notary
```

Expected: List of recent notarization submissions (empty if first time).

### Option B: App Store Connect API Key (Recommended for CI/CD)

#### Step 1: Create API Key

1. Go to [App Store Connect](https://appstoreconnect.apple.com)
2. Navigate to **Users and Access** → **Keys** (under Integrations)
3. Click **+** to generate a new key
4. Name: "SparkPass Notarization"
5. Access: **Developer** (minimum required)
6. Click **Generate**
7. Download the key file: `AuthKey_XXXXXXXXXX.p8`
8. Note the **Key ID** and **Issuer ID**

**Important**: The `.p8` file can only be downloaded once. Store it securely.

#### Step 2: Store API Key Credentials

```bash
xcrun notarytool store-credentials anubis-notary \
  --key /path/to/AuthKey_XXXXXXXXXX.p8 \
  --key-id XXXXXXXXXX \
  --issuer-id xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
```

Replace:
- `/path/to/AuthKey_XXXXXXXXXX.p8` - Path to your downloaded API key
- `XXXXXXXXXX` - Your Key ID (10 characters)
- `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` - Your Issuer ID (UUID format)

#### Step 3: Verify API Key

```bash
xcrun notarytool history --keychain-profile anubis-notary
```

---

## Build and Release Workflow

### Local Development: Quick Sign (No Notarization)

For local testing, you can sign without notarization:

```bash
# Build and sign binary
make sign

# Verify signature
codesign --verify --deep --strict bin/sparkpass_main
```

The signed binary will work on your Mac but will be blocked by Gatekeeper on other Macs.

### Production Release: Full Notarization

This is the complete workflow for distributing SparkPass:

#### Step 1: Build and Test

```bash
# Build the binary
make build

# Run the complete test suite
make test

# Expected: All tests pass
```

#### Step 2: Sign, Notarize, and Staple

```bash
# Create signed and notarized distribution packages
make release VER=1.0.0

# This will:
# 1. Build bin/sparkpass_main
# 2. Sign binary with Hardened Runtime
# 3. Create ZIP: dist/sparkpass-1.0.0-macos-arm64.zip
# 4. Notarize ZIP (wait for Apple approval: 2-10 minutes)
# 5. Create PKG: dist/sparkpass-1.0.0-installer.pkg
# 6. Sign PKG with Developer ID Installer
# 7. Notarize PKG (wait for Apple approval: 2-10 minutes)
# 8. Staple notarization ticket to PKG
# 9. Verify all signatures
```

**Architecture Detection**: The Makefile auto-detects your architecture (`arm64` or `x86_64`). Override with:

```bash
make release VER=1.0.0 ARCH=x86_64
```

#### Step 3: Verify Signatures

```bash
# Verify binary and PKG signatures
make verify

# Manual verification
codesign --verify --deep --strict bin/sparkpass_main
pkgutil --check-signature dist/sparkpass-1.0.0-installer.pkg
xcrun stapler validate dist/sparkpass-1.0.0-installer.pkg
spctl -a -vvv -t install dist/sparkpass-1.0.0-installer.pkg
```

Expected output:
- `bin/sparkpass_main: valid on disk`
- `Package "dist/sparkpass-1.0.0-installer.pkg":
   Status: signed by a developer certificate issued by Apple for distribution
   Signed with a trusted timestamp on: [timestamp]`
- `The staple and validate action worked!`
- `dist/sparkpass-1.0.0-installer.pkg: accepted
   source=Notarized Developer ID`

#### Step 4: Generate Checksums

```bash
cd dist
shasum -a 256 sparkpass-1.0.0-macos-arm64.zip sparkpass-1.0.0-installer.pkg > checksums.txt
cat checksums.txt
```

#### Step 5: Upload to GitHub Release

```bash
# Create GitHub release
gh release create v1.0.0 \
  --title "SparkPass v1.0.0" \
  --notes "Quantum-resistant password manager for macOS" \
  dist/sparkpass-1.0.0-macos-arm64.zip \
  dist/sparkpass-1.0.0-installer.pkg \
  dist/checksums.txt
```

Or upload manually via GitHub web interface:
1. Go to your repository
2. Click **Releases** → **Draft a new release**
3. Tag: `v1.0.0`
4. Attach: `sparkpass-1.0.0-macos-arm64.zip`, `sparkpass-1.0.0-installer.pkg`, `checksums.txt`
5. Publish release

---

## Verification

### Local Verification (Before Distribution)

```bash
# Complete verification suite
make verify

# Individual checks
./scripts/verify-signature.sh bin/sparkpass_main
```

### Remote Verification (On Another Mac)

To simulate a real user downloading and running SparkPass:

```bash
# Copy signed binary to another Mac
scp dist/sparkpass-1.0.0-macos-arm64.zip user@othermac:/tmp/

# On the other Mac:
cd /tmp
unzip sparkpass-1.0.0-macos-arm64.zip

# Set quarantine bit (simulates download from internet)
xattr -w com.apple.quarantine "0081;$(date +%s);Safari" sparkpass

# Try to run - macOS should accept without warnings
./sparkpass --help
```

Expected: No Gatekeeper warning. The app runs immediately.

If you see "cannot be opened because the developer cannot be verified", the notarization failed or wasn't stapled correctly.

---

## Installation Testing

### Test PKG Installation

```bash
# Install via PKG (requires sudo)
sudo installer -pkg dist/sparkpass-1.0.0-installer.pkg -target /

# Verify installation
which sparkpass
# Expected: /usr/local/bin/sparkpass

sparkpass --version
# Expected: SparkPass version 1.0.0

# Test functionality
sparkpass --help
```

### Test ZIP Installation

```bash
# Extract ZIP
cd dist
unzip sparkpass-1.0.0-macos-arm64.zip

# Copy to user bin directory
mkdir -p ~/bin
cp sparkpass ~/bin/
chmod +x ~/bin/sparkpass

# Add to PATH if needed
echo 'export PATH="$HOME/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# Test
sparkpass --version
```

### Uninstall

```bash
# Remove installed binary
sudo rm /usr/local/bin/sparkpass

# Or if installed in ~/bin
rm ~/bin/sparkpass
```

---

## CI/CD with GitHub Actions

Automate the entire build, sign, notarize, and release process with GitHub Actions.

### Setup: Add Secrets to GitHub Repository

1. Go to your repository on GitHub
2. Click **Settings** → **Secrets and variables** → **Actions**
3. Click **New repository secret**

Add the following secrets:

#### Certificates (Base64-encoded)

```bash
# Export certificates from keychain
security find-identity -v -p codesigning

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

# Encode to base64
base64 -i /tmp/devid_app.p12 | pbcopy
# Paste this as MACOS_CERTIFICATE_APP in GitHub Secrets

base64 -i /tmp/devid_installer.p12 | pbcopy
# Paste this as MACOS_CERTIFICATE_INSTALLER in GitHub Secrets

# Clean up
rm /tmp/devid_app.p12 /tmp/devid_installer.p12
```

**GitHub Secrets to Add:**
- `MACOS_CERTIFICATE_APP` - Base64-encoded Developer ID Application .p12
- `MACOS_CERTIFICATE_INSTALLER` - Base64-encoded Developer ID Installer .p12

#### Notarization Credentials

**Option A: App-Specific Password**

Add these secrets:
- `MACOS_NOTARIZATION_APPLE_ID` = `lynchmobb@pm.me`
- `MACOS_NOTARIZATION_TEAM_ID` = `E9VB3VKZKH`
- `MACOS_NOTARIZATION_PASSWORD` = Your app-specific password (from appleid.apple.com)

**Option B: API Key** (Recommended)

Add these secrets:
- `MACOS_NOTARIZATION_API_KEY` - Contents of `AuthKey_XXXXXXXXXX.p8` file (raw text)
- `MACOS_NOTARIZATION_KEY_ID` = Key ID (e.g., `ABC123DEFG`)
- `MACOS_NOTARIZATION_ISSUER_ID` = Issuer ID (UUID format)

### GitHub Actions Workflow

The workflow file `.github/workflows/release.yml` is already configured. See next section for details.

### Trigger a Release

```bash
# Via GitHub web interface:
# 1. Go to Actions → Release → Run workflow
# 2. Enter version: 1.0.0
# 3. Click Run workflow

# Or via GitHub CLI:
gh workflow run release.yml -f version=1.0.0
```

The workflow will:
1. Check out code
2. Install Alire and dependencies
3. Import certificates into keychain
4. Configure notarization credentials
5. Build SparkPass
6. Sign, notarize, and staple
7. Upload artifacts to GitHub Release

---

## Homebrew Distribution

Once you have notarized releases, you can distribute via Homebrew.

### Create Homebrew Tap

```bash
# Create a new GitHub repository: homebrew-sparkpass
# https://github.com/AnubisQuantumCipher/homebrew-sparkpass

# Clone locally
git clone https://github.com/AnubisQuantumCipher/homebrew-sparkpass.git
cd homebrew-sparkpass

# Create formula
mkdir -p Formula
```

### Homebrew Formula Template

Create `Formula/sparkpass.rb`:

```ruby
class Sparkpass < Formula
  desc "Quantum-resistant password manager with ML-KEM and ML-DSA"
  homepage "https://github.com/AnubisQuantumCipher/SparkPass"
  url "https://github.com/AnubisQuantumCipher/SparkPass/releases/download/v1.0.0/sparkpass-1.0.0-macos-arm64.zip"
  sha256 "YOUR_SHA256_CHECKSUM_HERE"
  license "MIT"
  version "1.0.0"

  depends_on :macos
  depends_on arch: :arm64

  def install
    bin.install "sparkpass"
  end

  test do
    assert_match "SparkPass version", shell_output("#{bin}/sparkpass --version")
  end
end
```

**Important**: Replace `YOUR_SHA256_CHECKSUM_HERE` with the actual SHA256 from `dist/checksums.txt`.

### Generate SHA256 for Formula

```bash
shasum -a 256 dist/sparkpass-1.0.0-macos-arm64.zip
```

### Publish Formula

```bash
cd homebrew-sparkpass
git add Formula/sparkpass.rb
git commit -m "Add SparkPass v1.0.0"
git push origin main
```

### Install via Homebrew

```bash
# Users can now install with:
brew tap anubisquantumcipher/sparkpass
brew install sparkpass

# Verify
sparkpass --version
```

### Update Formula for New Releases

```bash
# Update version, URL, and SHA256 in Formula/sparkpass.rb
# Then:
git add Formula/sparkpass.rb
git commit -m "Update SparkPass to v1.1.0"
git push origin main

# Users update with:
brew update
brew upgrade sparkpass
```

---

## Universal Binaries

To support both Apple Silicon (arm64) and Intel (x86_64) Macs, create a universal binary.

### Option A: Build on Two Machines

**On Apple Silicon Mac (arm64):**

```bash
make release VER=1.0.0 ARCH=arm64
cp dist/sparkpass-1.0.0-macos-arm64.zip dist/sparkpass-arm64.zip
unzip dist/sparkpass-arm64.zip
mv sparkpass sparkpass-arm64
```

**On Intel Mac (x86_64):**

```bash
make release VER=1.0.0 ARCH=x86_64
cp dist/sparkpass-1.0.0-macos-x86_64.zip dist/sparkpass-x86_64.zip
unzip dist/sparkpass-x86_64.zip
mv sparkpass sparkpass-x86_64
```

**Combine into Universal Binary:**

```bash
# Transfer sparkpass-x86_64 to the Apple Silicon Mac, then:
lipo -create sparkpass-arm64 sparkpass-x86_64 -output sparkpass-universal

# Verify
lipo -info sparkpass-universal
# Expected: Architectures in the fat file: sparkpass-universal are: x86_64 arm64

# Sign the universal binary
codesign --force \
  --timestamp \
  --options runtime \
  --entitlements signing/sparkpass.entitlements \
  --sign "Developer ID Application: AnubisQuantumCipher (E9VB3VKZKH)" \
  sparkpass-universal

# Create universal ZIP
ditto -c -k --keepParent sparkpass-universal sparkpass-1.0.0-macos-universal.zip

# Notarize
xcrun notarytool submit sparkpass-1.0.0-macos-universal.zip \
  --keychain-profile anubis-notary \
  --wait
```

### Option B: Use GitHub Actions Matrix

See `.github/workflows/release.yml` for an automated approach using multiple runners.

---

## Troubleshooting

### Issue: "Developer ID certificate not found"

**Symptoms:**
```
ERROR: Developer ID Application certificate not found
```

**Solution:**
1. Verify certificates are installed:
   ```bash
   security find-identity -v -p codesigning
   ```
2. If missing, follow [Certificate Setup](#certificate-setup) again
3. Ensure certificate names match exactly:
   - `Developer ID Application: AnubisQuantumCipher (E9VB3VKZKH)`
   - `Developer ID Installer: AnubisQuantumCipher (E9VB3VKZKH)`

### Issue: "Notarization profile not found"

**Symptoms:**
```
ERROR: Notarization profile 'anubis-notary' not found
```

**Solution:**
```bash
# Reconfigure notarization credentials
xcrun notarytool store-credentials anubis-notary \
  --apple-id lynchmobb@pm.me \
  --team-id E9VB3VKZKH \
  --password "YOUR_APP_SPECIFIC_PASSWORD"

# Verify
xcrun notarytool history --keychain-profile anubis-notary
```

### Issue: Notarization fails with "Invalid"

**Symptoms:**
```
status: Invalid
```

**Solution:**
1. Check notarization log:
   ```bash
   xcrun notarytool log SUBMISSION_ID --keychain-profile anubis-notary
   ```
2. Common reasons:
   - **Missing Hardened Runtime**: Ensure `--options runtime` is used
   - **Invalid entitlements**: Check `signing/sparkpass.entitlements` syntax
   - **Unsigned libraries**: All linked libraries must be signed
   - **Incorrect bundle identifier**: Must be unique reverse-DNS (e.g., `com.anubisquantumcipher.sparkpass`)

### Issue: Gatekeeper blocks execution

**Symptoms:**
```
"sparkpass" cannot be opened because the developer cannot be verified
```

**Solution:**
1. Verify notarization:
   ```bash
   spctl -a -vvv sparkpass
   ```
2. If rejected, re-notarize:
   ```bash
   make release VER=1.0.0
   ```
3. If accepted but still blocked, remove quarantine:
   ```bash
   xattr -d com.apple.quarantine sparkpass
   ```

### Issue: "The timestamp authority for the signature is not a trusted timestamp authority"

**Symptoms:**
```
codesign: The timestamp authority for the signature is not a trusted timestamp authority
```

**Solution:**
This warning is usually harmless. Verify signature still works:
```bash
codesign --verify --deep --strict sparkpass
```

If verification fails:
1. Check system date/time is correct
2. Ensure internet connection is stable (for timestamping)
3. Re-sign with timestamp:
   ```bash
   make sign
   ```

### Issue: Build fails with "ld: library not found"

**Symptoms:**
```
ld: library not found for -loqs
```

**Solution:**
1. Verify dependencies are installed:
   ```bash
   brew list liboqs openssl@3 libsodium
   ```
2. Check library paths in `build.sh`:
   ```bash
   pkg-config --libs liboqs openssl libsodium
   ```
3. Reinstall dependencies:
   ```bash
   brew reinstall liboqs openssl@3 libsodium
   ```

### Issue: Certificate expired

**Symptoms:**
```
ERROR: certificate has expired
```

**Solution:**
Developer ID certificates are valid for 5 years. To renew:
1. Go to [Apple Developer Certificates](https://developer.apple.com/account/resources/certificates/list)
2. Revoke the expired certificate
3. Generate a new CSR (see [Certificate Setup](#certificate-setup))
4. Create new Developer ID Application and Installer certificates
5. Install new certificates
6. Re-sign and notarize all binaries

### Issue: Notarization takes too long (>1 hour)

**Symptoms:**
```
xcrun notarytool submit ... --wait
(hangs for >1 hour)
```

**Solution:**
1. Check Apple System Status: https://developer.apple.com/system-status/
2. Cancel and retry:
   ```bash
   # Cancel current submission (Ctrl+C)
   # Check history
   xcrun notarytool history --keychain-profile anubis-notary
   # Retry submission
   make release VER=1.0.0
   ```
3. If repeatedly slow, use API key instead of app-specific password

### Issue: GitHub Actions fails with "Certificate not trusted"

**Symptoms:**
```
security: SecKeychainItemImport: A certificate in the chain was not trusted.
```

**Solution:**
Export certificates without password protection:
```bash
# Re-export with no password
security export -k login.keychain-db \
  -t identities \
  -f pkcs12 \
  -P "" \
  -o /tmp/devid_app.p12 \
  "Developer ID Application: AnubisQuantumCipher (E9VB3VKZKH)"

# Update GitHub Secret with new base64-encoded value
base64 -i /tmp/devid_app.p12 | pbcopy
```

---

## Quick Reference

### Daily Development Workflow

```bash
# Build and test locally (unsigned)
make build
./bin/sparkpass_main --help

# Run tests
make test

# Install locally for testing
make install
sparkpass --help
```

### Release Workflow (Production)

```bash
# Complete release with notarization
make release VER=1.0.0

# Verify signatures
make verify

# Generate checksums
cd dist
shasum -a 256 *.zip *.pkg > checksums.txt

# Create GitHub release
gh release create v1.0.0 \
  dist/sparkpass-1.0.0-macos-arm64.zip \
  dist/sparkpass-1.0.0-installer.pkg \
  dist/checksums.txt
```

### Makefile Targets

| Target | Description |
|--------|-------------|
| `make build` | Build SparkPass binary |
| `make sign` | Sign binary with Hardened Runtime (no notarization) |
| `make release VER=1.0.0` | Build, sign, notarize, staple (complete workflow) |
| `make verify` | Verify signatures and notarization |
| `make clean` | Remove build artifacts |
| `make clean-dist` | Remove distribution directory |
| `make install` | Install unsigned binary to /usr/local/bin (dev only) |
| `make test` | Run complete test suite |
| `make help` | Display all available targets |

### Key Commands

```bash
# List signing identities
security find-identity -v -p codesigning

# Verify code signature
codesign --verify --deep --strict --verbose=2 <binary>

# Display signature details
codesign -dv --verbose=4 <binary>

# Check Gatekeeper acceptance
spctl -a -vvv <binary>

# Verify PKG signature
pkgutil --check-signature <pkg-file>

# Validate stapled ticket
xcrun stapler validate <pkg-file>

# Submit for notarization
xcrun notarytool submit <file> --keychain-profile anubis-notary --wait

# Check notarization history
xcrun notarytool history --keychain-profile anubis-notary

# Get notarization log
xcrun notarytool log <submission-id> --keychain-profile anubis-notary
```

### File Locations

| File | Purpose |
|------|---------|
| `/Users/sicarii/SparkPass/bin/sparkpass_main` | Built binary (before distribution) |
| `/Users/sicarii/SparkPass/signing/sparkpass.entitlements` | Entitlements for Hardened Runtime |
| `/Users/sicarii/SparkPass/dist/*.zip` | Signed and notarized ZIP distribution |
| `/Users/sicarii/SparkPass/dist/*.pkg` | Signed, notarized, and stapled PKG installer |
| `/usr/local/bin/sparkpass` | Installed location (after PKG installation) |

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `VER` | Version number for release | `1.0.0` |
| `ARCH` | Target architecture (arm64, x86_64) | Auto-detected with `uname -m` |
| `SPARKPASS_ARCH` | Override architecture in scripts | Auto-detected |

### Apple Developer Resources

- **Developer Portal**: https://developer.apple.com/account/
- **Certificates**: https://developer.apple.com/account/resources/certificates/list
- **App Store Connect**: https://appstoreconnect.apple.com
- **System Status**: https://developer.apple.com/system-status/
- **Notarization Guide**: https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution

---

## Support and Resources

### Official Documentation

- [Apple Code Signing Guide](https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/)
- [Notarizing macOS Software](https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution)
- [Hardened Runtime](https://developer.apple.com/documentation/security/hardened_runtime)

### SparkPass Resources

- **GitHub Repository**: https://github.com/AnubisQuantumCipher/SparkPass
- **Issue Tracker**: https://github.com/AnubisQuantumCipher/SparkPass/issues
- **Security**: See `SECURITY.md` for vulnerability reporting

### Community

- **Discussions**: https://github.com/AnubisQuantumCipher/SparkPass/discussions
- **Email**: lynchmobb@pm.me

---

## License

This documentation is part of SparkPass and is licensed under the same terms as the main project.

---

**Last Updated**: 2025-10-16
**SparkPass Version**: 1.0.0
