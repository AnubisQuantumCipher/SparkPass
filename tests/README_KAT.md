# NIST Known Answer Test (KAT) Infrastructure for ML-DSA-87

## Overview

This directory contains the NIST KAT validation infrastructure for the SparkPass pure SPARK ML-DSA-87 implementation. The infrastructure validates that our implementation correctly implements FIPS 204 ML-DSA-87 by testing against official NIST test vectors.

## Current Status

**Implementation**: COMPLETE
**Test Vectors**: DOWNLOADED
**Test Harness**: IMPLEMENTED
**Build System**: CONFIGURED
**Current Issue**: Runtime crash (under investigation)

## Files Created

### Test Vector Files
- **tests/vectors/ml_dsa_87_kat.txt** (2.7MB)
  - Source: https://gist.github.com/itzmeanjan/d14afc3866b82119221682f0f3c9822d
  - Format: Deterministic ML-DSA-87 KAT vectors
  - Contains: 100 test vectors with KeyGen, Sign, Verify test cases
  - Fields per vector:
    - `seed`: 32-byte random seed
    - `pkey`: 2592-byte public key
    - `skey`: 4896-byte secret key
    - `mlen`: Message length
    - `msg`: Message bytes (variable length)
    - `ctx`: Context string
    - `sig`: 4627-byte signature

### Test Infrastructure Code

#### 1. Hex Parsing Utilities
**tests/test_hex_utils.ads** and **tests/test_hex_utils.adb**
- `Hex_Char_To_Byte`: Convert single hex character to byte value
- `Hex_String_To_Bytes`: Convert hex string to Byte_Array
- `Bytes_To_Hex_String`: Convert Byte_Array back to hex (for debugging)
- `Parse_U32`: Parse unsigned 32-bit integers from strings

#### 2. Full KAT Test Harness
**tests/test_mldsa87_nist_kat.adb**
- Parses NIST KAT file format
- Tests signature verification using official test vectors
- Tests sign/verify roundtrips with generated keypairs
- Tests cross-key rejection (security property)
- Comprehensive test reporting

#### 3. Simplified Test Harness
**tests/test_mldsa87_kat_simple.adb**
- Simplified version focusing on core functionality
- Test 1: KeyGen, Sign, Verify roundtrip
- Test 2: Cross-key rejection
- No file I/O dependencies

#### 4. Minimal Test
**tests/test_mldsa87_minimal.adb**
- Bare minimum test: KeyGen only
- For debugging runtime issues

### Build Configuration
**tests/test_mldsa87_kat.gpr**
- GPR project file for building all KAT tests
- Configured with 64MB stack size (ML-DSA-87 requires large stack for polynomial operations)
- Suppresses unnecessary warnings
- Builds three test executables

## ML-DSA-87 API Used

From `src/sparkpass/crypto/sparkpass-crypto-mldsa.ads`:

```ada
procedure Keypair (Public : out Public_Key; Secret : out Secret_Key);

procedure Sign
  (Secret  : Secret_Key;
   Message : Byte_Array;
   Output  : out Signature);

procedure Verify
  (Public  : Public_Key;
   Message : Byte_Array;
   Sig     : Signature;
   Success : out Boolean);
```

## Type Definitions

From `src/sparkpass/sparkpass-types.ads`:

```ada
subtype MLDsa_Public_Key_Array is Byte_Array (1 .. 2592);   -- FIPS 204 ML-DSA-87
subtype MLDsa_Secret_Key_Array is Byte_Array (1 .. 4896);   -- includes signing key
subtype MLDsa_Signature_Array  is Byte_Array (1 .. 4627);   -- signature output
```

## Building the Tests

```bash
# From SparkPass project root
alr exec -- gprbuild -P tests/test_mldsa87_kat.gpr

# Build specific test
alr exec -- gprbuild -P tests/test_mldsa87_kat.gpr test_mldsa87_kat_simple.adb
```

## Running the Tests

```bash
# Full KAT test (parses test vector file)
./bin/test_mldsa87_nist_kat tests/vectors/ml_dsa_87_kat.txt

# Simplified test (no file I/O)
./bin/test_mldsa87_kat_simple

# Minimal test (KeyGen only)
./bin/test_mldsa87_minimal
```

## Current Runtime Issue

**Symptom**: All test executables crash immediately with no output (only dyld warnings about duplicate RPATH)

**Investigation Steps Taken**:
1. Added 64MB stack size to linker flags (ML-DSA uses large polynomial arrays)
2. Reduced buffer sizes in parser (100KB line buffer removed from stack)
3. Created minimal test with exception handling
4. Verified main sparkpass_main binary works correctly with same ML-DSA implementation

**Observations**:
- Main `sparkpass_main` binary works perfectly (vault init with ML-DSA succeeds)
- All test programs built via tests/test_mldsa87_kat.gpr crash
- Crash occurs before main() is entered (likely during elaboration/initialization)
- Issue appears specific to test build environment, not the ML-DSA implementation itself

**Next Steps for Debugging**:
1. Compare linker flags between sparkpass.gpr and test_mldsa87_kat.gpr
2. Check for library initialization issues (Random module uses /dev/urandom)
3. Try building tests using main sparkpass.gpr instead of separate test GPR
4. Add verbose elaboration tracing (-gnatea flag)
5. Check for symbol conflicts or duplicate definitions

## Test Vector Format Details

The NIST KAT file format for ML-DSA-87:

```
seed = 7c9935a0b07694aa...  # 32-byte random seed (64 hex chars)
pkey = 903efbf16cd1f779...  # 2592-byte public key (5184 hex chars)
skey = 903efbf16cd1f779...  # 4896-byte secret key (9792 hex chars)
mlen = 33                    # Message length in bytes
msg = d81c4d8d734fcbfb...   # Message bytes (variable length hex)
ctx_len = 33                 # Context string length
ctx = 8626ed79d4511408...   # Context string (variable length hex)
sig = ef5e4b6a19d7e3f2...   # 4627-byte signature (9254 hex chars)

# Next vector starts with another "seed = " line
```

## Test Coverage

When operational, the KAT tests will validate:

1. **Signature Verification**: Verify signatures in test vectors using provided public keys
2. **Sign/Verify Roundtrip**: Generate keypairs, sign messages, verify own signatures
3. **Cross-Key Rejection**: Verify that signatures from Key A are rejected by Key B
4. **Multiple Message Sizes**: Test with varying message lengths (33 bytes to several KB)
5. **Deterministic Signing**: Ensure same message+key produces same signature

## Security Properties Tested

- **Correctness**: Valid signatures verify successfully
- **Unforgeability**: Signatures from different keys are rejected
- **Determinism**: Deterministic signing mode produces consistent signatures
- **NIST Compliance**: Outputs match official FIPS 204 test vectors

## Integration with CI/CD

Once runtime issue is resolved, add to CI pipeline:

```yaml
# Example GitHub Actions workflow
- name: Run ML-DSA-87 NIST KATs
  run: |
    alr exec -- gprbuild -P tests/test_mldsa87_kat.gpr
    ./bin/test_mldsa87_nist_kat tests/vectors/ml_dsa_87_kat.txt
```

## Future Enhancements

1. **Expand Test Coverage**:
   - Test all 100 vectors (currently limited to 10 for development)
   - Add hedged (randomized) signing mode tests
   - Test context string variations
   - Add invalid signature rejection tests

2. **Additional Test Vectors**:
   - Download ACVP JSON format vectors for comprehensive testing
   - Test with NIST CAVP official validation vectors
   - Generate custom test cases for edge conditions

3. **Performance Benchmarking**:
   - Measure KeyGen, Sign, Verify throughput
   - Compare against reference implementation
   - Profile hot paths for optimization opportunities

4. **Test Automation**:
   - Automated test vector download
   - Regression test suite
   - Continuous validation against new NIST releases

## References

- **FIPS 204**: ML-DSA Standard - https://csrc.nist.gov/pubs/fips/204/final
- **Test Vectors**: https://github.com/itzmeanjan/d14afc3866b82119221682f0f3c9822d
- **ACVP Server**: https://github.com/usnistgov/ACVP-Server (JSON format vectors)
- **ML-DSA Reference**: https://github.com/pq-crystals/dilithium

## Contact

For questions or issues with the KAT infrastructure, please open an issue in the SparkPass repository.

---

**Last Updated**: October 19, 2025
**Status**: Infrastructure complete, debugging runtime crash
