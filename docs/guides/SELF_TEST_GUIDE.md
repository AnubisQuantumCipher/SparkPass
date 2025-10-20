# SparkPass Self-Test Guide

## Overview

SparkPass includes a comprehensive self-test suite that validates all cryptographic operations and system components. This guide explains what each test validates, how to run tests, and how to interpret results.

## Table of Contents

1. [Running Self-Tests](#running-self-tests)
2. [Test Layers](#test-layers)
3. [Individual Test Descriptions](#individual-test-descriptions)
4. [Output Formats](#output-formats)
5. [CI/CD Integration](#cicd-integration)
6. [Troubleshooting](#troubleshooting)

---

## Running Self-Tests

### Basic Command

Run essential tests (completes in < 5 seconds):

```bash
sparkpass self-test
# or use the legacy alias:
sparkpass pqtest
```

### Comprehensive Mode

Run all tests including slower operations (completes in < 15 seconds):

```bash
sparkpass self-test --comprehensive
```

### Verbose Mode

Show detailed test information and usage hints:

```bash
sparkpass self-test --verbose
```

### JSON Output

Output machine-readable JSON for CI/CD integration:

```bash
sparkpass self-test --json
```

### Combined Flags

```bash
sparkpass self-test --comprehensive --verbose
sparkpass self-test --comprehensive --json > test_results.json
```

---

## Test Layers

SparkPass tests are organized into four layers, from low-level cryptographic primitives to high-level platform integration:

### Layer 1: Cryptographic Primitives

Core cryptographic operations from liboqs, OpenSSL, and libsodium:

- **Argon2id**: Password-based key derivation (1 GiB memory, 4 iterations)
- **HKDF-SHA-384**: HMAC-based key derivation
- **AES-256-GCM-SIV**: Authenticated encryption with nonce misuse resistance
- **ML-KEM-1024**: Post-quantum key encapsulation mechanism
- **ML-DSA-87**: Post-quantum digital signatures
- **Random (CSPRNG)**: Cryptographically secure random number generation

### Layer 2: SparkPass Cryptography

SparkPass-specific cryptographic constructions:

- **Shamir Secret Sharing**: k-of-n threshold secret splitting (GF(256))
- **Reed-Solomon FEC**: RS(255, 223) error correction for bit rot protection
- **Nonce Derivation**: Deterministic, injective nonce generation for AEAD
- **Key Wrapping**: AES-GCM-SIV based key encapsulation
- **Zeroization**: Secure memory wiping with verification

### Layer 3: Vault Operations

High-level vault components:

- **Key-Arena**: Multi-wrap root key storage (Wrap A/B/C/D serialization)
- **Policy Engine**: Unlock policy validation and enforcement

### Layer 4: Platform Integration

Platform-specific features (may be skipped on some systems):

- **LAContext** (macOS): Touch ID availability and policy evaluation
- **Keychain** (macOS): Secure storage for biometric unlock cache
- **File Permissions**: Vault file security (0600 enforcement)
- **Termios** (POSIX): Secure password input without echo

---

## Individual Test Descriptions

### 1. Argon2id KDF

**Purpose**: Validates password-based key derivation using Argon2id.

**What it tests**:
- Derives a 32-byte key from a 16-byte password
- Uses 1 GiB memory (or 64 MiB fallback if system has insufficient RAM)
- Runs 4 iterations (or 1 iteration in fallback mode)
- Verifies output is deterministic (same input → same output)

**Why it matters**: Argon2id protects against brute-force attacks by requiring significant memory and time to derive keys. This is the slowest operation in the test suite (2-4 seconds).

**Failure modes**:
- Memory allocation failure (system has < 64 MiB available)
- liboqs bindings error
- Non-deterministic output (indicates RNG or binding corruption)

---

### 2. HKDF-SHA-384

**Purpose**: Validates HMAC-based key derivation function.

**What it tests**:
- Derives a 32-byte key from Argon2id output
- Uses 16-byte salt and 8-byte info string
- Verifies output is deterministic

**Why it matters**: HKDF is used throughout SparkPass to derive domain-separated keys (e.g., for nonce generation, key wrapping).

**Failure modes**:
- OpenSSL bindings error
- SHA-384 hash function failure

---

### 3. AES-256-GCM-SIV

**Purpose**: Validates authenticated encryption with nonce misuse resistance.

**What it tests**:
- Encrypts 128 bytes of plaintext with a 32-byte key
- Uses 12-byte nonce and 16-byte salt as AAD
- Decrypts ciphertext and verifies plaintext matches original
- Verifies authentication tag prevents tampering

**Why it matters**: AES-GCM-SIV is used for all vault encryption. The SIV (Synthetic IV) mode provides critical nonce misuse resistance.

**Failure modes**:
- OpenSSL bindings error
- Encryption/decryption failure
- Authentication tag mismatch

---

### 4. ML-KEM-1024 (Post-Quantum KEM)

**Purpose**: Validates post-quantum key encapsulation mechanism.

**What it tests**:
- Generates ML-KEM-1024 keypair (1568-byte public key, 3168-byte secret key)
- Encapsulates a shared secret using public key (produces 1568-byte ciphertext)
- Decapsulates shared secret using secret key
- Verifies encapsulated and decapsulated secrets match

**Why it matters**: ML-KEM-1024 provides quantum-resistant key exchange for recovery operations (Wrap B).

**Failure modes**:
- liboqs bindings error
- Keypair generation failure
- Encapsulation/decapsulation mismatch

---

### 5. ML-DSA-87 (Post-Quantum Signatures)

**Purpose**: Validates post-quantum digital signatures.

**What it tests**:
- Generates ML-DSA-87 keypair (2592-byte public key, 4864-byte secret key)
- Signs a 64-byte message (produces 4627-byte signature)
- Verifies signature is valid for original message
- **Tamper detection**: Flips one bit in signature, verifies it's rejected

**Why it matters**: ML-DSA-87 signs every vault header and entry to prevent tampering and rollback attacks.

**Failure modes**:
- liboqs bindings error
- Keypair generation failure
- Signature creation/verification failure
- **CRITICAL**: Tamper detection failure (indicates broken signature verification)

---

### 6. Random (CSPRNG)

**Purpose**: Validates cryptographically secure random number generation.

**What it tests**:
- Fills 32-byte buffer with random data
- Verifies buffer is not all zeros (broken RNG)
- Fills a second 32-byte buffer
- Verifies buffers are different (deterministic RNG)

**Why it matters**: All cryptographic operations depend on secure randomness (salts, nonces, keys, IVs).

**Failure modes**:
- `/dev/urandom` read failure (POSIX)
- `getrandom()` syscall failure (Linux)
- `arc4random_buf()` failure (BSD/macOS)
- Weak RNG (outputs all zeros or deterministic values)

---

### 7. Shamir Secret Sharing

**Purpose**: Validates k-of-n threshold secret splitting using Shamir's scheme.

**What it tests**:
- **2-of-2 split**: Splits 32-byte root key into 2 shares, reconstructs from both
- **3-of-5 split**: Splits root key into 5 shares, reconstructs from any 3
- Verifies reconstructed key matches original in both cases
- Validates share structure (33 bytes: 1 x-coordinate + 32 y-coordinates)

**Why it matters**: Shamir sharing enables distributed custody (Wrap C-N), allowing recovery without single point of failure.

**Failure modes**:
- GF(256) arithmetic error
- Polynomial interpolation failure
- Share structure corruption
- Reconstruction produces wrong key

---

### 8. Reed-Solomon FEC (Comprehensive mode only)

**Purpose**: Validates RS(255, 223) forward error correction for bit rot protection.

**What it tests**:
- Encodes 223-byte data block into 32 parity bytes
- Verifies syndrome computation detects no errors in clean codeword
- Introduces 8 random errors (within t=16 correction capacity)
- Corrects all 8 errors and verifies codeword matches original

**Why it matters**: Reed-Solomon FEC protects against bit rot, media degradation, and cosmic ray flips (up to 16 byte errors per 255-byte block).

**Why skipped in fast mode**: RS decoding is computationally intensive (~50ms per block). Skipped to keep fast tests under 5 seconds.

**Failure modes**:
- GF(256) log/antilog table corruption
- Syndrome computation error
- Berlekamp-Massey algorithm failure (error locator polynomial)
- Chien search failure (error locations)
- Forney algorithm failure (error magnitudes)
- Correction produces wrong data

---

### 9. Nonce Derivation

**Purpose**: Validates deterministic, injective nonce generation for AEAD operations.

**What it tests**:
- **Determinism**: Same (counter, entry_id, domain) → same nonce
- **Counter injectivity**: Different counters → different nonces
- **Entry_ID injectivity**: Different entry IDs → different nonces
- **Domain separation**: Different domains → different nonces

**Why it matters**: AEAD security critically depends on nonce uniqueness. SparkPass uses deterministic nonces derived from (counter, entry_id, domain) to guarantee uniqueness without storing nonces.

**Failure modes**:
- HKDF derivation failure
- Non-deterministic nonces (RNG leak)
- Collision (different inputs produce same nonce) — **CRITICAL SECURITY FAILURE**

---

### 10. Key Wrapping

**Purpose**: Validates AES-GCM-SIV based key encapsulation wrapper.

**What it tests**:
- Wraps 32-byte root key with 32-byte KEK (key encryption key)
- Produces 60-byte wrapped key (12 nonce + 32 ciphertext + 16 tag)
- Unwraps with same KEK
- Verifies unwrapped key matches original

**Why it matters**: Key wrapping is used for all Key-Arena wraps (A/B/C/D) and Shamir share sealing.

**Failure modes**:
- AES-GCM-SIV encryption failure
- Nonce derivation failure
- Unwrapping produces wrong key
- Tag verification failure

---

### 11. Zeroization

**Purpose**: Validates secure memory wiping with verification.

**What it tests**:
- Wipes all sensitive buffers used in previous tests:
  - Argon2id derived keys
  - AES keys
  - Nonces
  - Plaintexts/ciphertexts
  - ML-KEM/ML-DSA secret keys
  - Signatures
- Verifies all buffers are fully zeroed using constant-time comparison

**Why it matters**: Memory not wiped after use can leak secrets via:
- Core dumps
- Swap files
- Memory probing (cold boot attacks)
- Process memory scanning

**Failure modes**:
- Compiler optimizes away memset (mitigated by volatile writes)
- Memory is not actually zeroed (indicates C binding failure)

---

### 12. Key-Arena

**Purpose**: Validates multi-wrap root key storage structure.

**What it tests**:
- Creates Key-Arena with Wrap A (passphrase wrap)
- Serializes to binary format (magic "KARN" + wraps)
- Deserializes from binary
- Validates policy constraints (Wrap A required, Touch ID requires passphrase)
- Verifies round-trip (serialize → deserialize → validate)

**Why it matters**: Key-Arena is the vault's cryptographic core, managing multiple wraps of the root key for different unlock methods.

**Failure modes**:
- Serialization produces invalid binary
- Deserialization fails (total parsing violation)
- Policy validation fails (security invariant broken)
- Round-trip produces corrupted data

---

### 13. Policy Engine

**Purpose**: Validates unlock policy validation and enforcement.

**What it tests**:
- Default policy is valid (passphrase-only)
- Policy validation (no unlock method → reject, Touch ID alone → reject)
- Serialize/deserialize round-trip
- Fast unlock with Touch ID requires passphrase
- Unlock logic (passphrase allows unlock, Touch ID alone rejected)

**Why it matters**: Policy engine enforces SparkPass's core security property: **software-only availability** (Touch ID never sole factor).

**Failure modes**:
- Invalid policy accepted (security invariant violated)
- Touch ID alone allowed (violates NIST SP 800-63B)
- Serialization round-trip corrupts policy
- Unlock logic allows forbidden factor combinations

---

### 14. Platform Integration (Optional)

**Purpose**: Validates platform-specific features.

**Tests included** (when available):
- **LAContext** (macOS): Biometric availability, policy evaluation
- **Keychain** (macOS): Store/retrieve/delete operations
- **File permissions**: Vault file 0600 enforcement
- **Termios** (POSIX): Secure password input without echo

**Why it matters**: Platform features provide user convenience (Touch ID) and additional security (Keychain, file permissions).

**Why skipped**: Platform tests require specific hardware (Touch ID) or OS features (Keychain). They're not required for core functionality.

**Failure modes** (non-critical):
- Touch ID hardware not available
- Keychain access denied
- File permission enforcement unsupported
- TTY not available for password input

---

## Output Formats

### Human-Readable Output

Default output format with visual indicators:

```
SparkPass v1.0 Self-Test
================================================================================

[PASS] ALL TESTS PASSED

[1/4] Cryptographic Primitives

  [PASS] Argon2id KDF                [ 2.847 s]
  [PASS] HKDF-SHA-384
  [PASS] AES-256-GCM-SIV
  [PASS] ML-KEM-1024
  [PASS] ML-DSA-87
  [PASS] Random (CSPRNG)
    Tamper detection: [PASS] detected

[2/4] SparkPass Cryptography

  [PASS] Shamir Secret Sharing
  ⊙ Reed-Solomon FEC
  [PASS] Nonce Derivation
  [PASS] Key Wrapping
  [PASS] Zeroization

[3/4] Vault Operations

  [PASS] Key-Arena
  [PASS] Policy Engine

[4/4] Platform Integration

  ⊙ Platform-specific tests

================================================================================
Total Duration:  3.301 s
LibOQS: [PASS]
```

**Legend**:
- `[PASS]` — Test passed
- `✗` — Test failed
- `⊙` — Test skipped (not required for pass)

---

### JSON Output

Machine-readable format for CI/CD:

```json
{
  "sparkpass_version": "2.0.8",
  "test_mode": "COMPREHENSIVE",
  "timestamp": "1729123456",
  "system": {
    "os": "macOS",
    "arch": "arm64"
  },
  "results": {
    "passed": true,
    "duration_seconds": 3.301
  },
  "tests": {
    "liboqs": true,
    "argon2id": "SUCCEEDED",
    "hkdf": "SUCCEEDED",
    "aes_gcm_siv": "SUCCEEDED",
    "ml_kem": "SUCCEEDED",
    "ml_dsa": "SUCCEEDED",
    "random": "SUCCEEDED",
    "shamir": "SUCCEEDED",
    "reed_solomon": "SUCCEEDED",
    "nonce": "SUCCEEDED",
    "wrapping": "SUCCEEDED",
    "zeroization": "SUCCEEDED",
    "key_arena": "SUCCEEDED",
    "policy": "SUCCEEDED"
  }
}
```

---

## CI/CD Integration

### GitHub Actions

```yaml
name: SparkPass Self-Test

on: [push, pull_request]

jobs:
  test:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install dependencies
        run: |
          brew install gnat gprbuild
          alr get
      - name: Build SparkPass
        run: gprbuild -P sparkpass.gpr
      - name: Run self-tests
        run: |
          ./bin/sparkpass self-test --comprehensive --json > test_results.json
          cat test_results.json
      - name: Check test status
        run: |
          if ! ./bin/sparkpass self-test --comprehensive; then
            echo "Self-tests failed!"
            exit 1
          fi
      - name: Upload test results
        uses: actions/upload-artifact@v3
        with:
          name: test-results
          path: test_results.json
```

### Exit Codes

- **0**: All tests passed
- **1**: One or more tests failed

Use `$?` in scripts to check status:

```bash
sparkpass self-test --comprehensive
if [ $? -ne 0 ]; then
    echo "Tests failed, aborting deployment"
    exit 1
fi
```

---

## Troubleshooting

### Test Failures

#### "Argon2id KDF failed"

**Cause**: Memory allocation failure or liboqs binding error.

**Solutions**:
- Check available memory: `free -m` (Linux) or `vm_stat` (macOS)
- Verify liboqs is installed: `brew list liboqs` (macOS)
- Check for memory limits: `ulimit -v` (POSIX)

---

#### "ML-DSA-87 tamper detection failed"

**Cause**: Critical security failure — signature verification is broken.

**Solutions**:
1. **Stop using this build immediately** (signature verification is critical)
2. Check liboqs version: `brew info liboqs` (should be ≥ 0.9.0)
3. Rebuild from clean state: `make clean && make`
4. Verify binary integrity: `shasum -a 256 bin/sparkpass`

---

#### "Reed-Solomon decode failed"

**Cause**: GF(256) arithmetic error or corruption.

**Solutions**:
- Run comprehensive tests: `sparkpass self-test --comprehensive`
- Check for bit flips: `md5sum src/sparkpass/crypto/sparkpass-crypto-reedsolomon.adb`
- Rebuild: `gprbuild -P sparkpass.gpr -f`

---

#### "Nonce derivation collision"

**Cause**: Critical security failure — nonce uniqueness is violated.

**Solutions**:
1. **Stop using this build immediately** (nonce reuse breaks AEAD security)
2. Check HKDF implementation: `git diff src/sparkpass/crypto/sparkpass-crypto-hkdf.adb`
3. Verify RNG is working: `sparkpass self-test --verbose | grep Random`
4. Report security issue: https://github.com/sicarii/sparkpass/security

---

#### "Policy engine failed"

**Cause**: Policy validation is broken (security invariant violated).

**Solutions**:
1. Check policy module: `git diff src/sparkpass/vault/sparkpass-vault-policy.adb`
2. Verify Touch ID policy: Test should reject Touch ID alone
3. Rebuild and retest: `make clean && make test`

---

### Performance Issues

#### "Argon2id takes > 10 seconds"

**Cause**: System is memory-constrained or CPU-bound.

**Expected**: 2-4 seconds on modern hardware (Apple M1/M2, Intel i7+).

**Solutions**:
- Close memory-intensive applications
- Check background processes: `top` (POSIX)
- Verify not using swap: `swapon -s` (Linux)

---

#### "Comprehensive tests take > 30 seconds"

**Cause**: Reed-Solomon decoding is slow on weak CPUs.

**Expected**: < 15 seconds on modern hardware.

**Solutions**:
- Use fast mode instead: `sparkpass self-test` (skips Reed-Solomon)
- Check CPU frequency: `sysctl -n hw.cpufrequency` (macOS)
- Disable power saving: System Preferences → Energy Saver

---

### CI/CD Issues

#### "Tests pass locally but fail in CI"

**Common causes**:
1. **Memory limits**: CI runners may have < 1 GiB available RAM
   - Solution: Use `--comprehensive` to allow Argon2id fallback
2. **Missing dependencies**: liboqs, OpenSSL, libsodium not installed
   - Solution: Add to CI install step
3. **Old liboqs**: CI package manager has outdated version
   - Solution: Build liboqs from source in CI

---

## Security Considerations

### What Self-Tests Do NOT Cover

Self-tests validate **correctness**, not **side-channel resistance**:

- [FAIL] Timing attacks (constant-time not verified)
- [FAIL] Cache-timing attacks (AES-NI usage not tested)
- [FAIL] Power analysis (hardware dependencies)
- [FAIL] Fault injection (requires specialized equipment)

**For production use**: Rely on audited implementations (liboqs, OpenSSL, libsodium) for side-channel resistance.

---

### Known Limitations

1. **Reed-Solomon skipped in fast mode**: Bit rot protection not tested unless `--comprehensive` used
2. **Platform tests optional**: Touch ID and Keychain not required for pass
3. **No fuzzing**: Input validation not exhaustively tested (see `test/fuzz/` for fuzz tests)
4. **No timing verification**: Constant-time operations assumed from upstream libraries

---

## Further Reading

- [SparkPass Architecture](./ARCHITECTURE.md)
- [Cryptographic Specification](./CRYPTO_SPEC.md)
- [Nonce Derivation Analysis](./NONCE_DERIVATION_ANALYSIS.md)
- [Shamir Secret Sharing](./SHAMIR_SPEC.md)
- [Reed-Solomon FEC](./REED_SOLOMON_SPEC.md)

---

## Support

- **Issues**: https://github.com/sicarii/sparkpass/issues
- **Security**: https://github.com/sicarii/sparkpass/security
- **Discussions**: https://github.com/sicarii/sparkpass/discussions
