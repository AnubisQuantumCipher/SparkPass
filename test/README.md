# Argon2id Test Vector Validation

This directory contains comprehensive test vector validation for the SparkPass Argon2id implementation.

## Quick Start

### Generate Test Vectors

```bash
# Create Python virtual environment
python3 -m venv venv
source venv/bin/activate

# Install argon2-cffi (reference implementation wrapper)
pip install argon2-cffi

# Generate test vectors
python3 generate_test_vectors.py
```

### Build and Run Tests

```bash
# Build the test program (requires GNAT Ada compiler)
gprbuild -P test_argon2id.gpr -p

# Or use the Makefile
make

# Run tests
./bin/test_argon2id_vectors

# Or
make run
```

### Expected Output

When all tests pass:

```
======================================================================
  SparkPass Argon2id RFC 9106 Test Vector Validation
======================================================================

Configuration:
  Algorithm:    Argon2id (variant 2)
  Version:      0x13 (19)
  Memory:       16384 KiB (16 MiB)
  Iterations:   4
  Parallelism:  1
  Output:       32 bytes

Test vectors generated from argon2-cffi (phc-winner-argon2 reference)

----------------------------------------------------------------------

Test  1: password/somesalt ... PASS
Test  2: long password/hex salt ... PASS
Test  3: minimal password/zero salt ... PASS
Test  4: UTF-8 password/max salt ... PASS
Test  5: long password/alternating salt ... PASS

======================================================================
  Test Summary
======================================================================
  Total:   5
  Passed:  5
  Failed:  0

  Result: ALL TESTS PASSED

  SparkPass Argon2id implementation is VALIDATED against RFC 9106.
======================================================================
```

## Files

| File | Description |
|------|-------------|
| `test_argon2id_vectors.adb` | Ada test program with 5 test vectors |
| `test_argon2id.gpr` | GNAT project file for building test |
| `generate_test_vectors.py` | Python script to generate reference vectors |
| `Makefile` | Build automation |
| `RFC9106_VALIDATION_REPORT.md` | Complete validation documentation |
| `README.md` | This file |

## Test Vectors

### Vector 1: Simple ASCII Password
- **Password**: `"password"` (8 bytes)
- **Salt**: `"somesaltSOMESALTsomesaltSOMESALT"` (32 bytes)
- **Expected**: `dbda37811a190cf4dffda38f6aaeef2f2bb74c675d1c333512790d4d902107a3`

### Vector 2: Long Passphrase
- **Password**: `"correct horse battery staple"` (28 bytes)
- **Salt**: `0x01..0x20` (hex sequence)
- **Expected**: `eae1d8e1e8c734062249f94f9ed774529209cb1bec306da82a770c6ff526525a`

### Vector 3: Minimal Password (Edge Case)
- **Password**: `"a"` (1 byte)
- **Salt**: All zeros (32 bytes)
- **Expected**: `cb36aabdd01f665d8fd4958061a20e7113e5b004297998cdacbb7f6068fcaa07`

### Vector 4: UTF-8 Password
- **Password**: `"π√∞"` (UTF-8, 8 bytes)
- **Salt**: All 0xFF (32 bytes)
- **Expected**: `2b5654a108b52dce4f9f1caadb20cb8e884c5e4e4fa66209a7332fccf7448149`

### Vector 5: Long Password
- **Password**: `"The quick brown fox jumps over the lazy dog. Jackdaws love my bi"` (64 bytes)
- **Salt**: Alternating `0xAA/0x55` (32 bytes)
- **Expected**: `f46c16847148066c2eafee9ba03bd443fe245f98ab74df266fc3f83da994ff09`

## Configuration

All test vectors use SparkPass-compatible Argon2id parameters:

- **Algorithm**: Argon2id (RFC 9106 Section 3.4.1.3)
- **Version**: 0x13 (19 decimal, Argon2 v1.3)
- **Memory**: 16 MiB (16,384 KiB) - matches `Test_Medium` verification mode
- **Iterations**: 4 (t=4)
- **Parallelism**: 1 (p=1)
- **Output**: 32 bytes
- **Secret key**: None
- **Associated data**: None

## Reference Implementation

Test vectors were generated using **argon2-cffi** (https://github.com/hynek/argon2-cffi), which wraps the official **phc-winner-argon2** reference implementation (https://github.com/P-H-C/phc-winner-argon2).

This is the same reference implementation used to validate RFC 9106 compliance.

## Why Not Use RFC 9106 Official Test Vectors?

RFC 9106 Section 5.3 provides official test vectors, but they use:
- Memory: 32 KiB (SparkPass uses 16 MiB)
- Iterations: 3 (SparkPass uses 4)
- Parallelism: 4 (SparkPass uses 1)
- Secret key and associated data (SparkPass doesn't support these)

Therefore, we generated custom test vectors using the **same reference implementation** but with SparkPass-compatible parameters.

## Validation Status

✅ **Phase 2.9 COMPLETE**
- Test vectors generated from reference implementation
- Test harness created with 5 comprehensive test cases
- Build system configured
- Validation report written

⏳ **Pending**: Runtime execution (requires GNAT installation: `brew install gcc`)

## Security Notes

⚠️ **WARNING**: Test vectors use **weak passwords** and **predictable salts** for testing only!

**Production code MUST**:
- Use cryptographic RNG for salt generation (`randombytes_buf`)
- Use strong passphrases (≥12 characters, high entropy)
- Never reuse salts
- Store salts securely (encrypted in vault header)

## Installing GNAT Compiler

### macOS
```bash
brew install gcc
```

### Ubuntu/Debian
```bash
sudo apt install gnat gprbuild
```

### Fedora/RHEL
```bash
sudo dnf install gcc-gnat gprbuild
```

## Troubleshooting

### Test Fails

If any test fails, the program will print:
```
Test  1: password/somesalt ... FAIL (Output mismatch)
  Output Comparison:
    Expected: dbda37811a190cf4dffda38f6aaeef2f2bb74c675d1c333512790d4d902107a3
    Actual:   [actual output in hex]
```

**Debugging steps**:
1. Verify Argon2id implementation matches RFC 9106
2. Check Blake2b implementation (used for H₀ and H')
3. Verify G mixing function correctness
4. Validate indexing functions
5. Review memory initialization

### Build Fails

If compilation fails:
1. Ensure GNAT is installed (`which gnat`)
2. Check Ada source paths in `test_argon2id.gpr`
3. Verify SparkPass library is compiled (`ls ../obj/*.o`)
4. Try cleaning and rebuilding (`make clean && make`)

## Further Reading

- **RFC 9106**: https://www.rfc-editor.org/rfc/rfc9106.html
- **phc-winner-argon2**: https://github.com/P-H-C/phc-winner-argon2
- **SPARK Ada**: https://www.adacore.com/about-spark
- **Argon2 Paper**: https://www.password-hashing.net/argon2-specs.pdf

---

**End of README**
