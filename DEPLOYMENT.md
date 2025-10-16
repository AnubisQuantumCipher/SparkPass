##  SparkPass Deployment Guide

**Version**: 1.0.0
**Date**: October 15, 2025
**Platform**: macOS (ARM64 and x86_64)
**Status**: Production Ready (Password-Only Mode)

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [System Requirements](#system-requirements)
3. [Building from Source](#building-from-source)
4. [Testing](#testing)
5. [Installation](#installation)
6. [Usage Examples](#usage-examples)
7. [Troubleshooting](#troubleshooting)
8. [Performance](#performance)
9. [Security Considerations](#security-considerations)
10. [Roadmap](#roadmap)

---

## Quick Start

```bash
# Build and sign
./scripts/build-and-sign.sh

# Create a vault
./bin/sparkpass_main init ~/my_vault.spass

# Unlock (test password)
./bin/sparkpass_main unlock ~/my_vault.spass

# Add a password
./bin/sparkpass_main add ~/my_vault.spass github

# Retrieve password
./bin/sparkpass_main get ~/my_vault.spass github

# List all entries
./bin/sparkpass_main ls ~/my_vault.spass
```

---

## System Requirements

### Minimum Requirements

- **OS**: macOS 10.15 (Catalina) or later
- **Architecture**: ARM64 (Apple Silicon) or x86_64 (Intel)
- **RAM**: 2 GB free (for Argon2id with 1 GiB memory)
- **Disk**: 10 MB for binary, ~20 KB per vault

### Build Requirements

- **Alire**: Ada package manager
- **GNAT**: 14.2.1 or later (via Alire)
- **gprbuild**: 24.0.1 or later (via Alire)
- **Xcode Command Line Tools**: For code signing
- **Dependencies**: OpenSSL 3.x, libsodium, liboqs

### Runtime Dependencies

Installed via Homebrew:
```bash
brew install openssl@3 libsodium liboqs
```

---

## Building from Source

### 1. Install Prerequisites

```bash
# Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Alire
brew install alire

# Install Ada toolchain
alr toolchain --select gnat_native gprbuild

# Install cryptographic libraries
brew install openssl@3 libsodium liboqs

# Install Xcode Command Line Tools
xcode-select --install
```

### 2. Clone Repository

```bash
cd ~/code
git clone https://github.com/sicarii/sparkpass
cd sparkpass
```

### 3. Build

```bash
# Automated build + sign
./scripts/build-and-sign.sh

# Manual build (if needed)
PATH="$HOME/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:$HOME/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:/usr/bin:/bin:$PATH" \
  gprbuild -p -P sparkpass.gpr

# Sign binary
codesign --force --identifier "com.sparkpass.cli" --sign - ./bin/sparkpass_main
```

### 4. Verify Build

```bash
# Check binary
ls -lh ./bin/sparkpass_main
file ./bin/sparkpass_main

# Verify signature
codesign -dv --verbose=4 ./bin/sparkpass_main

# Test version
./bin/sparkpass_main --version
```

Expected output:
```
SparkPass version 1.0.0
Post-quantum hybrid password vault
Cryptography: ML-KEM-1024, ML-DSA-87, AES-256-GCM-SIV, Argon2id
```

---

## Testing

### Quick Test (Manual)

```bash
# Create test vault
./bin/sparkpass_main init ./vaults/test.spass
# Enter password: test_password_12345
# Confirm password: test_password_12345

# Unlock test
./bin/sparkpass_main unlock ./vaults/test.spass
# Enter password: test_password_12345
# Expected: [PASS] password accepted (~2.5 seconds)

# Add entry
./bin/sparkpass_main add ./vaults/test.spass github
# Enter secret: ghp_test_token
# Enter password: test_password_12345

# List entries
./bin/sparkpass_main ls ./vaults/test.spass

# Clean up
rm -f ./vaults/test.spass
```

### Automated Test (Expect)

```bash
# Run expect-based tests (requires expect installed)
brew install expect

./test/test_simple_biometric.exp
```

### Keychain Inspection

```bash
# List cached vaults
./scripts/keychain-helper.sh list

# Show cache details
./scripts/keychain-helper.sh show ~/my_vault.spass

# Verify cache exists
./scripts/keychain-helper.sh verify ~/my_vault.spass
```

---

## Installation

### Option 1: Local Installation (Recommended for Development)

```bash
# Add to PATH in ~/.zshrc or ~/.bashrc
echo 'export PATH="$HOME/code/sparkpass/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# Test
sparkpass_main --version
```

### Option 2: System-Wide Installation

```bash
# Copy binary to /usr/local/bin
sudo cp ./bin/sparkpass_main /usr/local/bin/sparkpass

# Verify
sparkpass --version
```

### Option 3: Homebrew Tap (Future)

```bash
# Not yet available - roadmap item
brew tap sicarii/sparkpass
brew install sparkpass
```

---

## Usage Examples

### Basic Vault Operations

```bash
# Create vault
sparkpass_main init ~/Secure/personal.spass
# Use a strong password (≥12 characters)

# Unlock vault (test password)
sparkpass_main unlock ~/Secure/personal.spass

# Doctor (inspect vault metadata)
sparkpass_main doctor ~/Secure/personal.spass
```

### Managing Passwords

```bash
# Add GitHub token
sparkpass_main add ~/Secure/personal.spass github
# Enter secret: ghp_xxxxxxxxxxxxxxxxxxxx
# Enter password: (your vault password)

# Add AWS credentials
sparkpass_main add ~/Secure/personal.spass aws_access_key
# Enter secret: AKIAIOSFODNN7EXAMPLE

# Retrieve GitHub token
sparkpass_main get ~/Secure/personal.spass github
# WARNING: Secret will be printed to stdout. Continue? (y/N): y
# [PASS] ghp_xxxxxxxxxxxxxxxxxxxx

# List all entries
sparkpass_main ls ~/Secure/personal.spass
# Entries:  2
#   - github [PASSWORD]
#   - aws_access_key [PASSWORD]

# Remove entry
sparkpass_main rm ~/Secure/personal.spass aws_access_key
```

### Key Rotation

```bash
# Rotate master key (forward secrecy)
sparkpass_main rotate ~/Secure/personal.spass
# Enter password: (your vault password)
# Rotating master key...
#   - Generating new master key... [PASS]
#   - Re-encrypting all entries (2)... [PASS]
#   - Updating chain key... [PASS]
#   - Signing vault... [PASS]
# [PASS] Master key rotated
```

### Recovery Operations

```bash
# Export recovery key (ML-KEM-1024 public key)
sparkpass_main export ~/Secure/personal.spass
# Creates: ~/Secure/personal.spass.recovery

# Import from recovery file (if vault password lost)
sparkpass_main import ~/Secure/personal.spass ~/Secure/personal.spass.recovery
# Enter recovery password: (different from vault password)
```

---

## Troubleshooting

### Build Errors

#### "gprbuild: command not found"

**Solution**:
```bash
alr toolchain --select gnat_native gprbuild
export PATH="$HOME/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:$HOME/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:$PATH"
```

#### "library not found for -loqs"

**Solution**:
```bash
brew install liboqs
# If still fails, add to sparkpass.gpr:
#   -L/opt/homebrew/Cellar/liboqs/0.14.0/lib
```

### Runtime Errors

#### "failed to read password"

**Cause**: Password input requires a TTY (terminal).

**Solution**: Run directly in terminal, not via pipe or non-interactive context.

#### "vault file has insecure permissions"

**Cause**: Vault permissions must be 0600 (owner read/write only).

**Solution**:
```bash
chmod 600 ~/Secure/personal.spass
```

#### "authentication failed" (correct password)

**Possible causes**:
1. Wrong password (most common)
2. Vault file corrupted
3. Argon2id parameters changed between versions

**Solution**:
```bash
# Verify vault integrity
sparkpass_main doctor ~/Secure/personal.spass

# If corrupted, restore from backup
cp ~/Secure/personal.spass.backup ~/Secure/personal.spass
```

### Performance Issues

#### "Unlock takes >5 seconds"

**Expected**: ~2.5 seconds is normal (Argon2id with 1 GiB memory, 4 iterations).

**If slower**:
- Check available RAM: Argon2id needs 1 GiB free
- Check CPU load: Argon2id uses 4 parallel threads
- macOS may throttle under battery saver mode

**Workaround**: None in v1.0 (biometric unlock in v1.1 will fix this).

---

## Performance

### Benchmark Results (M4 MacBook Pro)

| Operation | Time | Notes |
|-----------|------|-------|
| Create vault | ~2.5s | Argon2id key derivation |
| Unlock (password) | ~2.5s | Argon2id key derivation |
| Add entry | ~2.6s | Unlock + AES-256-GCM-SIV + ML-DSA-87 sign |
| Get entry | ~2.6s | Unlock + AES-256-GCM-SIV + ML-DSA-87 verify |
| List entries | ~2.5s | Unlock only (entry count is in header) |
| Remove entry | ~2.6s | Unlock + re-sign vault |
| Rotate key | ~2.5s + 0.1s/entry | Re-encrypt all entries |

**Bottleneck**: Argon2id password derivation (intentional for security).

**v1.1 Improvement** (with biometric unlock):
- Unlock: ~50ms (~50× faster)
- Add/Get/List/Remove: ~100ms

### Memory Usage

| Component | Memory |
|-----------|--------|
| Binary size | 4.4 MB |
| Vault file | ~17 KB (empty), +1 KB per entry |
| Argon2id | 1 GiB (temporary, released after derivation) |
| Runtime | <10 MB |

---

## Security Considerations

### Cryptographic Primitives

1. **Password Derivation**: Argon2id
   - Memory: 1 GiB
   - Iterations: 4
   - Parallelism: 4 threads
   - Salt: 32 bytes (random per vault)

2. **Symmetric Encryption**: AES-256-GCM-SIV
   - Key: 256 bits
   - Nonce: 96 bits (unique per encryption)
   - Tag: 128 bits (authentication)

3. **Key Derivation**: HKDF-SHA512
   - Used for per-entry key derivation from master key

4. **Post-Quantum Signature**: ML-DSA-87 (Dilithium)
   - Public key: 2,592 bytes
   - Secret key: 4,896 bytes
   - Signature: 4,627 bytes

5. **Post-Quantum KEM**: ML-KEM-1024 (Kyber)
   - Public key: 1,568 bytes
   - Secret key: 3,168 bytes
   - Ciphertext: 1,568 bytes

### Threat Model

#### What SparkPass Protects Against

[OK] **Offline password attacks**: Argon2id makes brute-force infeasible
[OK] **Vault file theft**: Encrypted with strong symmetric encryption
[OK] **Man-in-the-middle**: Not applicable (local-only tool)
[OK] **Key compromise**: Forward secrecy via key rotation
[OK] **Quantum attacks**: ML-DSA-87 signatures, ML-KEM-1024 recovery

#### What SparkPass Does NOT Protect Against

[FAIL] **Keyloggers**: If malware logs your password, vault can be decrypted
[FAIL] **Screen recording**: Password input might be visible
[FAIL] **Physical access to unlocked device**: Vault only locked by password
[FAIL] **Side-channel attacks**: Not hardened against timing/power analysis
[FAIL] **Social engineering**: User can be tricked into revealing password

### Best Practices

1. **Use strong passwords**: Minimum 12 characters, mix of upper/lower/digits/symbols
2. **Rotate keys regularly**: Run `rotate` command every 90 days
3. **Back up recovery keys**: Store `*.recovery` files in separate location
4. **Set proper permissions**: Vault files must be 0600
5. **Don't reuse passwords**: Use SparkPass to generate unique passwords
6. **Verify integrity**: Run `doctor` command periodically
7. **Keep software updated**: Security patches released as needed

---

## Roadmap

### v1.1 (Next Release - Q1 2026)

**Feature**: LAContext Biometric Unlock

- [OK] Touch ID / Face ID authentication (macOS)
- [OK] ~50ms unlock (50× faster than password)
- [OK] 7-day cache expiration
- [OK] Automatic fallback to password
- [OK] No Apple Developer Program required
- [OK] Works in command-line tools

**Implementation**: See `LACONTEXT_SOLUTION.md` for technical details.

**Status**: Design complete, implementation in progress.

### v1.2 (Q2 2026)

**Feature**: Secure Enclave Integration (Optional)

- Hardware-backed biometric storage
- Requires Mac App Store distribution
- Maximum security for high-value secrets

### v2.0 (Q3 2026)

**Features**:
- Windows Hello support
- Linux PAM integration
- Browser extension (Chrome, Firefox, Safari)
- TOTP code generation
- Vault sharing (encrypted)
- Cloud backup (encrypted)

### v3.0 (Q4 2026)

**Features**:
- Team vaults (multi-user)
- Audit logs
- SSO integration
- Hardware key support (YubiKey, etc.)
- Mobile apps (iOS, Android)

---

## Support

- **Documentation**: https://github.com/sicarii/sparkpass/wiki
- **Issues**: https://github.com/sicarii/sparkpass/issues
- **Security**: sic.tau@pm.me
- **Discussions**: https://github.com/sicarii/sparkpass/discussions

---

## License

SparkPass is open-source software licensed under the MIT License.

See `LICENSE` file for details.

---

## Credits

**Organization**: AnubisQuantumCipher
**Project**: SparkPass - Quantum-Resistant Password Manager
**Cryptography**: NIST FIPS 203 (ML-KEM), NIST FIPS 204 (ML-DSA), NIST SP 800-38D (AES-GCM-SIV), RFC 7914 (Argon2)

**Dependencies**:
- liboqs (Open Quantum Safe)
- OpenSSL 3.x
- libsodium
- GNAT Ada Compiler

---

**Last Updated**: October 15, 2025
