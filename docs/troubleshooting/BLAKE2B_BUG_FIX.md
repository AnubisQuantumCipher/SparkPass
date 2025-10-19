# Blake2b Zero-Message Bug Fix

## Executive Summary

**ROOT CAUSE**: Test vectors in `test/test_blake2b_vectors.adb` were incorrect, not a bug in the Blake2b implementation.

**STATUS**: ✅ **FIXED** - All test vectors corrected and verified against `b2sum` and Python hashlib.

## Problem Description

Tests were failing for messages containing zero bytes:
- ❌ One-byte zero (0x00): FAIL (diverged at byte 27)
- ❌ 128-byte zeros: FAIL
- ❌ 256-byte zeros: FAIL

While non-zero messages passed:
- ✅ Empty message (0 bytes): PASS
- ✅ "abc" message (3 bytes): PASS

## Investigation Process

### Initial Hypothesis
The bug appeared to be related to how zero bytes were processed in the compression function, since:
1. Empty message (special case, counter=0) passed
2. "abc" (non-zero data) passed
3. All-zero messages failed

### Code Analysis Performed
1. ✅ Verified little-endian byte packing (LE_Pack/LE_Unpack)
2. ✅ Verified Sigma permutation table against RFC 7693
3. ✅ Verified counter calculations for intermediate and final blocks
4. ✅ Verified state initialization with parameter block (0x01010040)
5. ✅ Verified work vector initialization from State and IV
6. ✅ Verified G function implements RFC 7693 Section 3.1
7. ✅ Verified finalization XOR pattern

**Result**: Implementation is **CORRECT** per RFC 7693 specification.

### Root Cause Discovery

Verified test vectors against `b2sum` command:

```bash
$ printf '\x00' | b2sum
2fa3f686df876995167e7c2e5d74c4c7b6e48f8068fe0e44208344d480f7904c...
```

Compared to test file bytes 25-32:
- **Expected (from b2sum)**: `20 83 44 d4 80 f7 90 4c`
- **In test file (WRONG)**: `20 83 72 b5 e3 63 af 95`

**The test vectors were incorrect!**

## Fix Applied

### File: `test/test_blake2b_vectors.adb`

**1. One-byte zero test vector (lines 162-170)**

**BEFORE** (bytes 25-64):
```ada
16#20#, 16#83#, 16#72#, 16#b5#, 16#e3#, 16#63#, 16#af#, 16#95#,
16#52#, 16#ae#, 16#97#, 16#d9#, 16#82#, 16#d9#, 16#a3#, 16#25#,
16#d9#, 16#20#, 16#6f#, 16#ef#, 16#d2#, 16#12#, 16#02#, 16#bb#,
16#8a#, 16#22#, 16#6a#, 16#9a#, 16#57#, 16#02#, 16#a9#, 16#08#,
16#c4#, 16#61#, 16#4a#, 16#b3#, 16#45#, 16#14#, 16#45#, 16#d0#
```

**AFTER** (corrected):
```ada
16#20#, 16#83#, 16#44#, 16#d4#, 16#80#, 16#f7#, 16#90#, 16#4c#,
16#36#, 16#96#, 16#3e#, 16#44#, 16#11#, 16#5f#, 16#e3#, 16#eb#,
16#2a#, 16#3a#, 16#c8#, 16#69#, 16#4c#, 16#28#, 16#bc#, 16#b4#,
16#f5#, 16#a0#, 16#f3#, 16#27#, 16#6f#, 16#2e#, 16#79#, 16#48#,
16#7d#, 16#82#, 16#19#, 16#05#, 16#7a#, 16#50#, 16#6e#, 16#4b#
```

**2. 128-byte zeros test vector (lines 126-134)**

**BEFORE**:
```ada
(16#5e#, 16#32#, 16#47#, 16#5c#, 16#d4#, 16#27#, 16#ba#, 16#6f#,
 16#9f#, 16#c6#, 16#7f#, 16#d7#, 16#2c#, 16#eb#, 16#1b#, 16#8a#,
 ...)
```

**AFTER**:
```ada
(16#86#, 16#59#, 16#39#, 16#e1#, 16#20#, 16#e6#, 16#80#, 16#54#,
 16#38#, 16#47#, 16#88#, 16#41#, 16#af#, 16#b7#, 16#39#, 16#ae#,
 ...)
```

**3. 256-byte zeros test vector (lines 144-152)**

**BEFORE**:
```ada
(16#ca#, 16#3e#, 16#93#, 16#88#, 16#ae#, 16#57#, 16#14#, 16#07#,
 16#64#, 16#2d#, 16#65#, 16#2f#, 16#86#, 16#b9#, 16#a5#, 16#eb#,
 ...)
```

**AFTER**:
```ada
(16#ec#, 16#9c#, 16#6b#, 16#30#, 16#1a#, 16#6c#, 16#98#, 16#94#,
 16#6d#, 16#74#, 16#2a#, 16#74#, 16#71#, 16#0e#, 16#65#, 16#8f#,
 ...)
```

## Verification

All test vectors verified against multiple sources:

### 1. `b2sum` Command-Line Tool
```bash
printf ''         | b2sum  # Empty message
printf 'abc'      | b2sum  # "abc"
printf '\x00'     | b2sum  # One byte zero
dd if=/dev/zero bs=1 count=128 | b2sum  # 128 zeros
dd if=/dev/zero bs=1 count=256 | b2sum  # 256 zeros
```

✅ All match corrected test vectors

### 2. Python hashlib
```python
import hashlib
hashlib.blake2b(b'', digest_size=64).hexdigest()
hashlib.blake2b(b'abc', digest_size=64).hexdigest()
hashlib.blake2b(b'\x00', digest_size=64).hexdigest()
hashlib.blake2b(b'\x00' * 128, digest_size=64).hexdigest()
hashlib.blake2b(b'\x00' * 256, digest_size=64).hexdigest()
```

✅ All match corrected test vectors

## Test Results

| Test Case | Status | Hash (first 32 bytes) |
|-----------|--------|------------------------|
| Empty (0 bytes) | ✅ PASS | `786a02f742015903c6c6fd852552d272...` |
| "abc" (3 bytes) | ✅ PASS | `ba80a53f981c4d0d6a2797b69f12f6e9...` |
| One zero (1 byte) | ✅ PASS | `2fa3f686df876995167e7c2e5d74c4c7...` |
| 128 zeros | ✅ PASS | `865939e120e6805438478841afb739ae...` |
| 256 zeros | ✅ PASS | `ec9c6b301a6c98946d742a74710e658f...` |

## Conclusion

**The Blake2b implementation in `src/sparkpass/crypto/sparkpass-crypto-blake2b.adb` is CORRECT and RFC 7693 compliant.**

The issue was **incorrect test vectors** that were likely:
1. Copied from a different Blake2 variant (Blake2s, Blake2bp, etc.)
2. Generated with incorrect parameters
3. Transcribed with errors

All test vectors have been corrected and verified against authoritative sources.

## Files Modified

1. `/Users/sicarii/SparkPass/test/test_blake2b_vectors.adb`
   - Fixed OneByte_Expected (bytes 25-64)
   - Fixed Block128_Expected (all 64 bytes)
   - Fixed Block256_Expected (all 64 bytes)

## Security Impact

**NONE** - The implementation was always correct. Only test vectors were wrong.

The Blake2b implementation can be used with confidence for:
- ✅ Argon2id password hashing
- ✅ HMAC-Blake2b
- ✅ General-purpose cryptographic hashing

## Next Steps

1. ✅ Rebuild tests: `alr exec -- gprbuild -P test_blake2b.gpr`
2. ✅ Run verification: `./bin/test_blake2b_vectors`
3. ✅ Confirm all 5 test cases pass
4. Continue with SparkPass integration

---

**Date**: 2025-10-17
**Status**: ✅ RESOLVED - No implementation bugs found
