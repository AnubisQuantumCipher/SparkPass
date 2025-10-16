# SparkPass Code Signing Quick Start Guide

## TL;DR - Complete Workflow

For experienced developers who want to get started immediately:

```bash
# 1. Generate CSR and get certificate from Apple
./scripts/generate-csr.sh  # OR use Keychain Access GUI
# → Upload CSR to https://developer.apple.com/account/resources/certificates/add
# → Download certificate and run: ./scripts/install-certificate.sh cert.cer

# 2. Set up notarization credentials
./scripts/setup-notarization.sh
# → Uses app-specific password from https://appleid.apple.com/account/manage

# 3. Configure signing identity
export SPARKPASS_SIGNING_IDENTITY="Developer ID Application: Your Name (TEAM12345)"

# 4. Build, sign, and notarize
./scripts/build-and-sign.sh

# 5. Distribute
# Files will be in dist/ directory
```

---

## One-Time Setup (15 minutes)

### Prerequisites
- Apple Developer Program membership ($99/year)
- Xcode Command Line Tools installed
- Homebrew installed

### Step 1: Create Developer ID Certificate (5 min)

**Option A: Using Keychain Access (Recommended for first-time)**
1. Open Keychain Access
2. Menu: Keychain Access → Certificate Assistant → Request Certificate from CA
3. Enter your email, name, save to disk
4. Go to https://developer.apple.com/account/resources/certificates/add
5. Select "Developer ID Application"
6. Upload CSR, download certificate
7. Double-click certificate to install

**Option B: Using Script**
```bash
./scripts/generate-csr.sh
# Follow prompts, upload CSR to Apple, download certificate
./scripts/install-certificate.sh ~/Downloads/developerID_application.cer
```

**Verify:**
```bash
security find-identity -v -p codesigning
# Copy the identity string for next steps
```

### Step 2: Set Up Notarization (5 min)

1. Generate app-specific password: https://appleid.apple.com/account/manage
2. Run setup script:
   ```bash
   ./scripts/setup-notarization.sh
   # Enter Apple ID, app-specific password, Team ID
   ```

**Verify:**
```bash
xcrun notarytool history --keychain-profile sparkpass-notarization
```

### Step 3: Configure Environment (1 min)

Add to `~/.zshrc` or `~/.bash_profile`:

```bash
export SPARKPASS_SIGNING_IDENTITY="Developer ID Application: John Doe (ABCD123456)"
```

Reload shell:
```bash
source ~/.zshrc
```

---

## Regular Build & Release Workflow

### Development Build (Unsigned)

```bash
./build.sh
# Binary at: bin/sparkpass_main
```

### Release Build (Signed & Notarized)

```bash
# Full automated workflow
./scripts/build-and-sign.sh

# Or step-by-step:
./build.sh                              # Build
./scripts/sign-and-notarize.sh          # Sign and notarize
./scripts/create-distribution.sh        # Create distribution packages

# Or using Makefile:
make release
```

**Output:** Distribution files in `dist/` directory

---

## Makefile Targets

```bash
make build      # Build binary
make clean      # Clean artifacts
make sign       # Sign only (no notarization)
make notarize   # Build + sign + notarize
make release    # Interactive full workflow
make install    # Install to /usr/local/bin (dev only, unsigned)
```

---

## Verification

### Verify Signature
```bash
./scripts/verify-signature.sh bin/sparkpass_main
```

### Verify Gatekeeper Acceptance
```bash
spctl --assess --type execute --verbose bin/sparkpass_main
# Should show: accepted
```

### Test on Another Mac (Simulates End-User Experience)
```bash
# On build Mac:
cd dist
python3 -m http.server 8000

# On test Mac:
curl -O http://buildmac.local:8000/sparkpass-VERSION.tar.gz
tar -xzf sparkpass-VERSION.tar.gz
xattr -w com.apple.quarantine "0081;$(date +%s);Safari" sparkpass-VERSION/sparkpass
./sparkpass-VERSION/sparkpass --help
# Should run without warnings
```

---

## Common Commands Reference

### Code Signing

```bash
# Sign with Hardened Runtime
codesign --sign "$SPARKPASS_SIGNING_IDENTITY" \
    --options runtime \
    --entitlements signing/sparkpass.entitlements \
    --force \
    --timestamp \
    bin/sparkpass_main

# Verify signature
codesign --verify --verbose=4 bin/sparkpass_main

# Display signature details
codesign --display --verbose=4 bin/sparkpass_main

# Show entitlements
codesign --display --entitlements - bin/sparkpass_main
```

### Notarization

```bash
# Submit for notarization
xcrun notarytool submit dist/sparkpass.zip \
    --keychain-profile sparkpass-notarization \
    --wait

# Check history
xcrun notarytool history --keychain-profile sparkpass-notarization

# Get submission info
xcrun notarytool info SUBMISSION_ID --keychain-profile sparkpass-notarization

# Get detailed log
xcrun notarytool log SUBMISSION_ID \
    --keychain-profile sparkpass-notarization \
    output.json
```

### Gatekeeper

```bash
# Test Gatekeeper acceptance
spctl --assess --type execute --verbose=4 bin/sparkpass_main

# Remove quarantine (dev testing only)
xattr -d com.apple.quarantine bin/sparkpass_main

# Add quarantine (test distribution)
xattr -w com.apple.quarantine "0081;$(date +%s);Safari" bin/sparkpass_main
```

---

## File Structure

```
SparkPass/
├── bin/
│   └── sparkpass_main          # Built binary
├── signing/
│   ├── sparkpass.entitlements  # Entitlements configuration
│   └── sparkpass-developer-id.certSigningRequest  # Generated CSR
├── dist/                       # Distribution packages (generated)
│   ├── sparkpass-VERSION-macos-arm64.tar.gz
│   ├── sparkpass-VERSION-macos-arm64.zip
│   ├── sparkpass-VERSION-checksums.txt
│   └── notarization-log.json
├── scripts/
│   ├── generate-csr.sh         # Generate certificate signing request
│   ├── install-certificate.sh  # Install Developer ID certificate
│   ├── setup-notarization.sh   # Configure notarization credentials
│   ├── sign-and-notarize.sh    # Complete signing workflow
│   ├── build-and-sign.sh       # Build + sign + notarize
│   ├── verify-signature.sh     # Verify signature and notarization
│   └── create-distribution.sh  # Create distribution packages
├── Formula/
│   └── sparkpass.rb            # Homebrew formula template
├── docs/
│   ├── CODE_SIGNING_QUICKSTART.md        # This file
│   ├── CODE_SIGNING_TROUBLESHOOTING.md   # Detailed troubleshooting
│   └── DISTRIBUTION_STRATEGIES.md        # Distribution options
└── Makefile                    # Build automation
```

---

## Distribution Options

### Option 1: Homebrew Tap (Recommended)

**Setup:**
```bash
# Create tap repository
gh repo create homebrew-sparkpass --public

# Push formula
cp Formula/sparkpass.rb /path/to/homebrew-sparkpass/Formula/
cd /path/to/homebrew-sparkpass
git add Formula/sparkpass.rb
git commit -m "Add SparkPass formula"
git push
```

**User Installation:**
```bash
brew tap yourusername/sparkpass
brew install sparkpass
```

### Option 2: GitHub Releases

**Create Release:**
```bash
# After building and signing
cd dist
gh release create v1.0.0 \
    sparkpass-1.0.0-macos-arm64.tar.gz \
    sparkpass-1.0.0-checksums.txt \
    --title "SparkPass v1.0.0" \
    --notes "Release notes here"
```

**User Installation:**
```bash
curl -LO https://github.com/yourusername/sparkpass/releases/download/v1.0.0/sparkpass-1.0.0-macos-arm64.tar.gz
tar -xzf sparkpass-1.0.0-macos-arm64.tar.gz
sudo cp sparkpass-1.0.0/sparkpass /usr/local/bin/
```

### Option 3: Direct Download

Host on your server or CDN:
```bash
scp dist/sparkpass-1.0.0-macos-arm64.tar.gz user@server:/var/www/downloads/
```

---

## Environment Variables

```bash
# Required for signing
export SPARKPASS_SIGNING_IDENTITY="Developer ID Application: Your Name (TEAM12345)"

# Optional: Version for distribution packages
export SPARKPASS_VERSION="1.0.0"
```

---

## Troubleshooting Quick Fixes

### "No signing identity found"
```bash
security find-identity -v -p codesigning
export SPARKPASS_SIGNING_IDENTITY="<identity from above>"
```

### "Notarization failed"
```bash
# Get detailed log
xcrun notarytool history --keychain-profile sparkpass-notarization
xcrun notarytool log <SUBMISSION_ID> --keychain-profile sparkpass-notarization log.json
cat log.json | jq '.issues'
```

### "Gatekeeper rejects binary"
```bash
# Verify notarization
spctl --assess --verbose=4 bin/sparkpass_main
# If fails, re-notarize:
./scripts/sign-and-notarize.sh
```

### "Library not found" at runtime
```bash
# Install Homebrew dependencies
brew install liboqs openssl@3 libsodium
```

---

## CI/CD Integration

For automated builds in GitHub Actions, see the example workflow:

```yaml
# .github/workflows/release.yml
name: Release

on:
  push:
    tags:
      - 'v*'

jobs:
  build-and-release:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install dependencies
        run: brew install liboqs openssl@3 libsodium

      - name: Build
        run: ./build.sh

      - name: Import certificate
        env:
          CERTIFICATE_BASE64: ${{ secrets.DEVELOPER_ID_CERTIFICATE_BASE64 }}
          CERTIFICATE_PASSWORD: ${{ secrets.CERTIFICATE_PASSWORD }}
        run: |
          echo "$CERTIFICATE_BASE64" | base64 --decode > certificate.p12
          security create-keychain -p actions temp.keychain
          security default-keychain -s temp.keychain
          security unlock-keychain -p actions temp.keychain
          security import certificate.p12 -k temp.keychain -P "$CERTIFICATE_PASSWORD" -T /usr/bin/codesign
          security set-key-partition-list -S apple-tool:,apple: -s -k actions temp.keychain

      - name: Sign and Notarize
        env:
          SPARKPASS_SIGNING_IDENTITY: ${{ secrets.SIGNING_IDENTITY }}
          NOTARIZATION_APPLE_ID: ${{ secrets.NOTARIZATION_APPLE_ID }}
          NOTARIZATION_PASSWORD: ${{ secrets.NOTARIZATION_PASSWORD }}
          NOTARIZATION_TEAM_ID: ${{ secrets.NOTARIZATION_TEAM_ID }}
        run: ./scripts/sign-and-notarize.sh

      - name: Create Release
        uses: softprops/action-gh-release@v1
        with:
          files: dist/*
```

**Required Secrets:**
- `DEVELOPER_ID_CERTIFICATE_BASE64`: Certificate exported as base64
- `CERTIFICATE_PASSWORD`: P12 export password
- `SIGNING_IDENTITY`: Full identity string
- `NOTARIZATION_APPLE_ID`: Apple ID email
- `NOTARIZATION_PASSWORD`: App-specific password
- `NOTARIZATION_TEAM_ID`: Team ID

---

## Next Steps

1. **Complete setup:** Follow "One-Time Setup" section
2. **Test workflow:** Run `./scripts/build-and-sign.sh`
3. **Create release:** Build and upload distribution packages
4. **Set up Homebrew tap:** Create tap repository for easy user installation
5. **Automate CI/CD:** Configure GitHub Actions for automated releases

---

## Support

- **Detailed troubleshooting:** See `docs/CODE_SIGNING_TROUBLESHOOTING.md`
- **Distribution strategies:** See `docs/DISTRIBUTION_STRATEGIES.md`
- **Apple Developer Support:** https://developer.apple.com/contact/
