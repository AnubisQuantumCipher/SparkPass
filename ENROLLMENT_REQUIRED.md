# Apple Developer Code Signing Status

**Project**: SparkPass v1.0.0
**Distribution**: Unsigned Build
**Date**: 2025-10-16

---

## Current Distribution Status

SparkPass v1.0.0 is **distributed as an unsigned build** for the following reasons:

1. **No Apple Developer Program membership required** - Free to distribute
2. **Functionally identical** to signed versions - All features work the same
3. **No notarization delay** - Releases can be published immediately
4. **Simple workflow** - Build, zip, release

---

## User Experience

### What Users See

When downloading SparkPass, users will encounter macOS Gatekeeper warnings:

**Warning message**: "sparkpass_main cannot be opened because the developer cannot be verified"

### How Users Install

**Method 1: Command line (Recommended)**
```bash
curl -LO https://github.com/AnubisQuantumCipher/SparkPass/releases/download/v1.0.0/sparkpass-1.0.0-macos-arm64-unsigned.zip
unzip sparkpass-1.0.0-macos-arm64-unsigned.zip
xattr -d com.apple.quarantine sparkpass_main
chmod +x sparkpass_main
./sparkpass_main --version
```

**Method 2: Right-click**
1. Download and extract ZIP
2. Right-click `sparkpass_main`
3. Click "Open"
4. Click "Open" in the security dialog

---

## Comparison: Signed vs Unsigned

| Feature | Signed Build | Unsigned Build (Current) |
|---------|--------------|--------------------------|
| Cost | $99/year | Free |
| User warnings | None | One-time warning |
| Installation | Drag and drop | Remove quarantine flag |
| Security | Same | Same |
| Functionality | Same | Same |
| Distribution | Mac App Store + direct | Direct download only |
| Notarization time | 2-10 minutes | None |
| Certificate management | Required | None |

---

## Future Code Signing (Optional)

If you want to sign SparkPass releases in the future, you'll need:

### Requirements

1. **Apple Developer Program** membership ($99/year)
   - Individual or Organization account
   - Team ID: E9VB3VKZKH (example)
   - Processing time: 24-48 hours after enrollment

2. **Developer ID Certificates**
   - Developer ID Application (for binaries)
   - Developer ID Installer (for PKG files)

3. **Notarization Credentials**
   - App-specific password OR App Store Connect API key

### Setup Steps (High Level)

1. Enroll at https://developer.apple.com/programs/enroll/
2. Wait for approval (24-48 hours)
3. Generate Certificate Signing Request (CSR)
4. Request Developer ID certificates from Apple
5. Install certificates in macOS Keychain
6. Configure notarization credentials
7. Update build scripts for signing

### Benefits of Code Signing

- **No user warnings** - macOS accepts the app immediately
- **Professional appearance** - Shows verified developer
- **Mac App Store eligible** - Can distribute via App Store
- **Enterprise distribution** - Easier deployment in organizations

### Drawbacks of Code Signing

- **Annual cost** - $99/year membership fee
- **Notarization time** - 2-10 minute delay per release
- **Certificate management** - Renewal every 5 years
- **Setup complexity** - More moving parts

---

## Current Recommendation

**For SparkPass v1.0.0**: Continue with unsigned distribution.

**Reasons**:
- Binary is functionally identical
- Users are technical enough to handle quarantine removal
- No membership cost
- Faster release workflow
- Security is not compromised

**When to consider signing**:
- User base grows beyond technical users
- Distributing to enterprises
- Want Mac App Store distribution
- Have budget for $99/year membership

---

## Technical Notes

### Why Unsigned is Safe

SparkPass is safe to distribute unsigned because:

1. **Source code is public** - Anyone can audit: https://github.com/AnubisQuantumCipher/SparkPass
2. **Checksum verification** - SHA256 checksums provided: `dist/checksums.txt`
3. **Static binary** - No remote code execution or updates
4. **Local-only** - No network communication
5. **SPARK verified** - Memory safety formally proven

### Verification for Users

Users can verify the download integrity:

```bash
# Download checksum file
curl -LO https://github.com/AnubisQuantumCipher/SparkPass/releases/download/v1.0.0/checksums.txt

# Verify download
shasum -a 256 -c checksums.txt
```

Expected output:
```
sparkpass-1.0.0-macos-arm64-unsigned.zip: OK
```

---

## Documentation Status

**Removed documentation** (no longer relevant for unsigned distribution):
- `SIGNING_SETUP.md` - Certificate setup guide
- `SIGNING_IMPLEMENTATION_COMPLETE.md` - Signing workflow summary
- `NOTARIZATION.md` - Complete notarization reference
- `docs/CODE_SIGNING_QUICKSTART.md` - Quick signing guide
- `docs/CODE_SIGNING_TROUBLESHOOTING.md` - Signing troubleshooting
- `docs/DISTRIBUTION_STRATEGIES.md` - Distribution options

**Current documentation** (relevant for unsigned distribution):
- `README.md` - Installation instructions with quarantine removal
- `DEPLOYMENT.md` - Complete deployment guide
- `ENROLLMENT_REQUIRED.md` - This file (status and future options)

---

## Contact

**Security**: sic.tau@pm.me
**Repository**: https://github.com/AnubisQuantumCipher/SparkPass
**Issues**: https://github.com/AnubisQuantumCipher/SparkPass/issues

---

**Last Updated**: 2025-10-16
**SparkPass Version**: 1.0.0
