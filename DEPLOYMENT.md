# SparkPass Deployment Guide

**Version**: 1.0.0
**Date**: 2025-10-16
**Platform**: macOS (ARM64)
**Status**: Production Ready (Unsigned Build)

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [System Requirements](#system-requirements)
3. [Installation Methods](#installation-methods)
4. [Building from Source](#building-from-source)
5. [Testing](#testing)
6. [Usage Examples](#usage-examples)
7. [Troubleshooting](#troubleshooting)
8. [Performance](#performance)
9. [Security Considerations](#security-considerations)

---

## Quick Start

### Download Pre-Built Binary

```bash
# Download from GitHub Release
curl -LO https://github.com/AnubisQuantumCipher/SparkPass/releases/download/v1.0.0/sparkpass-1.0.0-macos-arm64-unsigned.zip

# Extract
unzip sparkpass-1.0.0-macos-arm64-unsigned.zip

# Remove macOS quarantine (required for unsigned builds)
xattr -d com.apple.quarantine sparkpass_main
chmod +x sparkpass_main

# Verify
./sparkpass_main --version
```

### Create Your First Vault

```bash
# Create a vault
./sparkpass_main init ~/my_vault.spass

# Add a password
./sparkpass_main add ~/my_vault.spass github

# Retrieve password
./sparkpass_main get ~/my_vault.spass github

# List all entries
./sparkpass_main ls ~/my_vault.spass
```

---

## System Requirements

### Minimum Requirements

- **OS**: macOS 10.15 (Catalina) or later
- **Architecture**: ARM64 (Apple Silicon) recommended
- **RAM**: 2 GB free (for Argon2id with 1 GiB memory)
- **Disk**: 10 MB for binary, ~20 KB per vault

### Build Requirements (Source Only)

- **Alire**: Ada package manager
- **GNAT**: 14.2.1 or later (via Alire)
- **gprbuild**: 24.0.1 or later (via Alire)
- **Xcode Command Line Tools**: For compiling
- **Dependencies**: OpenSSL 3.x, libsodium, liboqs

### Runtime Dependencies

SparkPass binary is statically linked and has no external dependencies.

---

## Installation Methods

### Option 1: Direct Installation (Recommended)

```bash
# Download binary
curl -LO https://github.com/AnubisQuantumCipher/SparkPass/releases/download/v1.0.0/sparkpass-1.0.0-macos-arm64-unsigned.zip
unzip sparkpass-1.0.0-macos-arm64-unsigned.zip

# Remove quarantine flag
xattr -d com.apple.quarantine sparkpass_main
chmod +x sparkpass_main

# Move to PATH
mkdir -p ~/bin
mv sparkpass_main ~/bin/sparkpass
echo 'export PATH="$HOME/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# Test
sparkpass --version
```

### Option 2: System-Wide Installation

```bash
# After downloading and removing quarantine
sudo mv sparkpass_main /usr/local/bin/sparkpass

# Verify
sparkpass --version
```

### Option 3: Build from Source

See [Building from Source](#building-from-source) section below.

---

## Building from Source

### 1. Install Prerequisites

```bash
# Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Alire
brew install alire

# Install cryptographic libraries
brew install openssl@3 libsodium liboqs

# Install Xcode Command Line Tools
xcode-select --install
```

### 2. Clone Repository

```bash
git clone https://github.com/AnubisQuantumCipher/SparkPass.git
cd SparkPass
```

### 3. Build

```bash
# Automated build script
./build.sh
```

The build script:
- Selects GNAT toolchain via Alire
- Compiles Ada sources with GNAT/GPRbuild
- Compiles Objective-C LAContext helpers (for Touch ID)
- Links with macOS frameworks
- Produces: `bin/sparkpass_main` (~4.5MB)

### 4. Verify Build

```bash
# Check binary
ls -lh ./bin/sparkpass_main
file ./bin/sparkpass_main

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
./bin/sparkpass_main init /tmp/test.spass
# Enter password: test_password_12345
# Confirm password: test_password_12345

# Unlock test
./bin/sparkpass_main unlock /tmp/test.spass
# Enter password: test_password_12345
# Expected: authentication successful (~2.5 seconds)

# Add entry
./bin/sparkpass_main add /tmp/test.spass github
# Enter secret: test_secret
# Enter password: test_password_12345

# Retrieve entry
./bin/sparkpass_main get /tmp/test.spass github
# Enter password: test_password_12345
# Output: test_secret

# List entries
./bin/sparkpass_main ls /tmp/test.spass
# Enter password: test_password_12345

# Clean up
rm -f /tmp/test.spass
```

### Cryptographic Self-Tests

```bash
./bin/sparkpass_main pqtest
```

Expected output:
```
PQ stack self-test passed
  liboqs      : passed
  argon2id    : passed [2.5s]
  hkdf        : passed
  aes-gcm-siv : passed
  ml-kem      : passed
  ml-dsa      : passed
  tamper      : detected
  zeroization : passed
```

---

## Usage Examples

### Basic Vault Operations

```bash
# Create vault
sparkpass init ~/Secure/personal.spass

# Unlock vault (test password)
sparkpass unlock ~/Secure/personal.spass

# Doctor (inspect vault metadata without unlocking)
sparkpass doctor ~/Secure/personal.spass
```

### Managing Passwords

```bash
# Add GitHub token
sparkpass add ~/Secure/personal.spass github
# Enter secret: ghp_xxxxxxxxxxxxxxxxxxxx
# Enter password: (your vault password)

# Add AWS credentials
sparkpass add ~/Secure/personal.spass aws_access_key
# Enter secret: AKIAIOSFODNN7EXAMPLE

# Retrieve GitHub token
sparkpass get ~/Secure/personal.spass github
# WARNING: Secret will be printed to stdout. Continue? (y/N): y

# List all entries
sparkpass ls ~/Secure/personal.spass
# Entries:  2
#   - github [PASSWORD]
#   - aws_access_key [PASSWORD]

# Remove entry
sparkpass rm ~/Secure/personal.spass aws_access_key
```

### Key Rotation

```bash
# Rotate master key (forward secrecy)
sparkpass rotate ~/Secure/personal.spass
# Enter password: (your vault password)
# Rotating master key...
# [PASS] Master key rotated
```

### Recovery Operations

```bash
# Export recovery file (ML-KEM-1024 wrapped keys)
sparkpass export ~/Secure/personal.spass
# Creates: ~/Secure/personal.spass.recovery

# Import from recovery file
sparkpass import ~/Secure/personal.spass ~/Secure/personal.spass.recovery
# Enter password: (your vault password)
```

---

## Troubleshooting

### macOS Gatekeeper Warnings

**Symptom**: macOS blocks execution with "cannot be opened because the developer cannot be verified"

**Solution**:
```bash
# Remove quarantine flag
xattr -d com.apple.quarantine sparkpass_main

# Or right-click → Open → Click "Open" in dialog
```

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
brew install liboqs openssl@3 libsodium
```

### Runtime Errors

#### "failed to read password"

**Cause**: Password input requires a TTY (terminal).

**Solution**: Run directly in terminal, not via pipe or non-interactive context. For non-interactive usage, use stdin or environment variable methods (see `NON_INTERACTIVE_USAGE.md`).

#### "vault file has insecure permissions"

**Cause**: Vault permissions must be 0600 (owner read/write only).

**Solution**:
```bash
chmod 600 ~/Secure/personal.spass
```

#### "authentication failed" (correct password entered)

**Possible causes**:
1. Wrong password (most common)
2. Vault file corrupted
3. Argon2id parameters changed between versions

**Solution**:
```bash
# Verify vault integrity
sparkpass doctor ~/Secure/personal.spass

# If corrupted, restore from backup
cp ~/Secure/personal.spass.backup ~/Secure/personal.spass
```

---

## Performance

### Benchmark Results (M1 MacBook Pro, 2021)

| Operation | Time | Notes |
|-----------|------|-------|
| Create vault | 2.48s | Argon2id key derivation |
| Unlock (password) | 2.50s | Argon2id key derivation |
| Add entry | 2.88s | Unlock + encrypt + sign |
| Get entry | 2.90s | Unlock + decrypt + verify |
| List entries | 2.87s | Unlock only |
| Remove entry | 2.92s | Unlock + re-sign vault |
| Rotate key | ~3s + 0.1s/entry | Re-encrypt all entries |

**Bottleneck**: Argon2id password derivation (intentional for security).

### Memory Usage

| Component | Memory |
|-----------|--------|
| Binary size | 4.5 MB |
| Vault file (empty) | ~11 KB |
| Vault file (100 entries) | ~21 KB |
| Argon2id (temporary) | 1 GiB |
| Runtime overhead | ~30 MB |

---

## Security Considerations

### Cryptographic Primitives

1. **Password Derivation**: Argon2id
   - Memory: 1 GiB
   - Iterations: 4
   - Parallelism: 1
   - Salt: 32 bytes (random per vault)
   - Time: ~2.5s (brute-force resistance: 480 trillion years with 1000 GPUs)

2. **Symmetric Encryption**: AES-256-GCM-SIV
   - Key: 256 bits
   - Nonce: 96 bits (deterministically derived)
   - Tag: 128 bits (authentication)
   - Nonce-misuse resistant

3. **Key Derivation**: HKDF-SHA-384
   - Used for per-entry key derivation from master key
   - Includes domain separation

4. **Post-Quantum Signature**: ML-DSA-87 (NIST FIPS 204)
   - Public key: 2,592 bytes
   - Secret key: 4,864 bytes
   - Signature: 4,627 bytes
   - Security level: NIST Level 5 (192-bit quantum security)

5. **Post-Quantum KEM**: ML-KEM-1024 (NIST FIPS 203)
   - Public key: 1,568 bytes
   - Secret key: 3,168 bytes
   - Ciphertext: 1,568 bytes
   - Security level: NIST Level 5 (256-bit quantum security)

### Threat Model

#### What SparkPass Protects Against

- [OK] **Offline password attacks**: Argon2id makes brute-force infeasible
- [OK] **Vault file theft**: Encrypted with strong symmetric encryption
- [OK] **Key compromise**: Forward secrecy via key rotation
- [OK] **Quantum attacks**: ML-DSA-87 signatures, ML-KEM-1024 recovery
- [OK] **Timing attacks**: Argon2id dominates timing (~2.5s)
- [OK] **Memory dumps**: Sensitive data zeroized on all paths

#### What SparkPass Does NOT Protect Against

- [FAIL] **Keyloggers**: If malware logs your password, vault can be decrypted
- [FAIL] **Screen recording**: Password might be visible during input
- [FAIL] **Physical access to unlocked device**: No protection while vault is unlocked
- [FAIL] **Social engineering**: User can be tricked into revealing password
- [FAIL] **Compromised system**: If OS is compromised, no password manager is safe

### Best Practices

1. **Use strong passwords**: Minimum 12 characters, mix of character types
2. **Rotate keys regularly**: Run `rotate` command every 90 days
3. **Back up recovery keys**: Store `*.recovery` files in separate location
4. **Set proper permissions**: Vault files must be 0600
5. **Keep software updated**: Security patches released as needed
6. **Verify downloads**: Check SHA256 checksums from `checksums.txt`
7. **Keep backups**: Regular vault backups to separate media

---

## Distribution

SparkPass v1.0.0 is distributed as an unsigned build. This means:

- **macOS will show a security warning** on first run
- **Users must manually approve** the application
- **Functionally identical** to signed versions
- **No Apple Developer Program required** for distribution

### For Users

To run SparkPass after download:

**Method 1: Command line**
```bash
xattr -d com.apple.quarantine sparkpass_main
./sparkpass_main --version
```

**Method 2: Right-click**
1. Right-click `sparkpass_main`
2. Click "Open"
3. Click "Open" in the security dialog

### For Developers

To create releases:

```bash
# Build binary
./build.sh

# Create distribution ZIP
mkdir -p dist
ditto -c -k --keepParent bin/sparkpass_main dist/sparkpass-1.0.0-macos-arm64-unsigned.zip

# Generate checksum
shasum -a 256 dist/sparkpass-1.0.0-macos-arm64-unsigned.zip > dist/checksums.txt

# Create GitHub release
gh release create v1.0.0 \
  dist/sparkpass-1.0.0-macos-arm64-unsigned.zip \
  dist/checksums.txt
```

---

## Support

- **GitHub Repository**: https://github.com/AnubisQuantumCipher/SparkPass
- **Issues**: https://github.com/AnubisQuantumCipher/SparkPass/issues
- **Security**: sic.tau@pm.me
- **Documentation**: See README.md and other docs in repository

---

## License

SparkPass is open-source software licensed under the MIT License.

See `LICENSE` file for details.

---

**Last Updated**: 2025-10-16
**SparkPass Version**: 1.0.0
