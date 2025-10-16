# Code Signing Configuration

This directory contains configuration files for Apple Developer ID code signing and notarization.

## Files

### sparkpass.entitlements

Hardened Runtime entitlements for SparkPass CLI tool.

**Configuration**:
- Hardened Runtime enabled
- JIT compilation disabled
- Unsigned executable memory disabled
- DYLD environment variables disabled
- Library validation enabled
- No app sandbox (CLI tools don't use sandbox)

**Usage**:
This file is automatically used by `codesign` during the signing process.

```bash
codesign --force \
  --timestamp \
  --options runtime \
  --entitlements signing/sparkpass.entitlements \
  --sign "Developer ID Application: AnubisQuantumCipher (E9VB3VKZKH)" \
  bin/sparkpass_main
```

## Documentation

For complete signing and notarization documentation, see:

- **Quick Setup**: [../SIGNING_SETUP.md](../SIGNING_SETUP.md)
- **Complete Guide**: [../NOTARIZATION.md](../NOTARIZATION.md)
- **Implementation Summary**: [../SIGNING_IMPLEMENTATION_COMPLETE.md](../SIGNING_IMPLEMENTATION_COMPLETE.md)

## Quick Reference

### Sign Binary

```bash
make sign
```

### Create Notarized Release

```bash
make release VER=1.0.0
```

### Verify Signatures

```bash
make verify
```

## Security Notes

The entitlements file configures SparkPass with maximum security:

1. **Hardened Runtime**: Enforces runtime protections against code injection
2. **No JIT**: Prevents runtime code generation (not needed for CLI tools)
3. **No Unsigned Memory**: Blocks unsigned executable memory pages
4. **No DYLD Variables**: Prevents library hijacking via environment variables
5. **Library Validation**: All loaded libraries must be signed by same Team ID or Apple

These settings ensure SparkPass meets Apple's security requirements for notarization while maintaining compatibility with macOS frameworks (LocalAuthentication, Security, Foundation).

## Modification

To modify entitlements:

1. Edit `sparkpass.entitlements` (XML plist format)
2. Validate syntax: `plutil -lint sparkpass.entitlements`
3. Re-sign binary: `make sign`
4. Verify: `codesign --display --entitlements - bin/sparkpass_main`

**Warning**: Incorrect entitlements can cause notarization to fail or break functionality. Test thoroughly after changes.

## References

- [Hardened Runtime Documentation](https://developer.apple.com/documentation/security/hardened_runtime)
- [Entitlements Reference](https://developer.apple.com/documentation/bundleresources/entitlements)
- [Notarization Requirements](https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution)
