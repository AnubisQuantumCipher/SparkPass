# ML-DSA-87 Verify Debug Status

## Current State (2025-10-19)

### Test Results
-  **KeyGen**: Works correctly
-  **Sign**: Completes successfully (no rejection sampling hang)
-  **Verify**: FAILS - c̃ ≠ c̃' (signature validation fails)

### Root Cause
The `c̃` check is failing, which means `w'1` computed in Verify doesn't match `w1` created during Sign.

Both algorithms should produce the same `w1` value when computing:
- **Sign**: `w1 ← HighBits(NTT^{-1}(A * NTT(y)))`
- **Verify**: `w'1 ← UseHint(h, NTT^{-1}(A * NTT(z) - c * t1 * 2^d))`

## Code Structure

### INTT Implementation (sparkpass-crypto-mldsa87-poly.adb:138-186)
```ada
--  Final scaling by n^{-1} (Montgomery form)
for I in Poly_Index loop
   P (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.MontMul (P (I), F);
end loop;
```
- `F = 16382` = `(n^{-1} * R) mod q` where `n^{-1} = 8347681`, `R = 2^32 mod q`
- **Output**: Coefficients are `(c * n^{-1})` but still in **Montgomery domain**
- **Requires**: Decode_Poly after INTT to convert to plain domain

### Domain Flow in KeyGen (sparkpass-crypto-mldsa87.adb:89-163)
1. Encode S1 to Montgomery → `NTT(S1_Mont)` → stays Montgomery
2. `A_Mont * NTT(S1_Mont)` → Montgomery domain mat-vec
3. `INTT` → **Montgomery domain** (scaled by n^{-1})
4. **Decode_Poly** → Plain domain
5. `Add(Temp, S2)` → plain + plain = plain 

### Domain Flow in Sign (sparkpass-crypto-mldsa87.adb:345-378)
1. Encode Y to Montgomery → `NTT(Y_Mont)` → stays Montgomery
2. `A_Mont * NTT(Y_Mont)` → Montgomery domain mat-vec
3. `INTT(W)` → **Montgomery domain** (scaled by n^{-1})
4. **Decode_Poly** → Plain domain
5. `HighBits(W)` → operates on plain 

### Domain Flow in Verify (sparkpass-crypto-mldsa87.adb:540-607)
1. Encode T1 to Montgomery → `NTT(T1_Mont)` → stays Montgomery
2. Encode Z to Montgomery → `NTT(Z_Mont)` → stays Montgomery
3. `NTT(Z_Mont) - c * NTT(T1_Mont)` → Montgomery domain operations
4. `INTT(Temp_Vec_K)` → **Montgomery domain** (scaled by n^{-1})
5. **Decode_Poly** → Plain domain
6. `Scale by 2^d` → operates on plain
7. `UseHint(H, Temp_Vec_K)` → operates on plain 

## Verified Constants
-  `n = 256`
-  `q = 8380417`
-  `n^{-1} = 8347681`
-  `256 * 8347681 mod 8380417 = 1`
-  `F = 16382 = (8347681 * 2^32) mod 8380417`

## Potential Issues to Investigate

### 1. Encoding Before NTT
**Status**: Need to verify all three locations encode consistently
- KeyGen line 136-139: Encodes S1 before NTT
- Sign line 360-362: Encodes Y before NTT
- Verify line 552-554: Encodes T1 before NTT
- Verify: Does Z need encoding? (currently at line 536-538)

### 2. INTT Normalization
**Current**: INTT returns Montgomery domain with n^{-1} scaling
**Question**: Is this the correct behavior per FIPS 204?

### 3. Decode_Poly Implementation
**Location**: sparkpass-crypto-mldsa87-zq_ops.adb
**Function**: Convert Montgomery → Plain via `MontMul(x, 1)`
**Status**: Need to verify correctness

### 4. UseHint Implementation
**Critical**: This is where Verify reconstructs w'1 from hints
**Need to check**: Is UseHint correctly inverting the MakeHint operation?

### 5. HighBits/LowBits Decomposition
**Used in**: Both Sign (HighBits) and Verify (UseHint calls HighBits internally)
**Need to verify**: Decomposition is consistent between Sign and Verify

## Next Steps

1. **Add detailed debug output** to both Sign and Verify showing:
   - w1 values after HighBits (Sign)
   - w'1 values after UseHint (Verify)
   - Compare coefficient by coefficient

2. **Verify Encode_Poly is called** in all necessary locations before NTT

3. **Check UseHint/MakeHint** are correctly inverse operations

4. **Review FIPS 204 Algorithm 3** (Verify) line-by-line against implementation

## Touch ID Status
-  Disabled temporarily (hangs in non-interactive mode)
- 🔜 Re-enable after ML-DSA Verify is fixed

## Files Modified
- `/Users/sicarii/SparkPass/src/sparkpass/crypto/sparkpass-crypto-mldsa87.adb` - Added debug output, restored decode loops
- `/Users/sicarii/SparkPass/src/sparkpass/platform/sparkpass-platform-keychain.adb` - Disabled Touch ID enrollment
- `/Users/sicarii/SparkPass/tests/test_mldsa_simple_verify.adb` - Created isolated test

## Test Command
```bash
cd /Users/sicarii/SparkPass
GPR_PROJECT_PATH=/Users/sicarii/.local/share/alire/releases/sparknacl_4.0.1_8e3cc2e6:$GPR_PROJECT_PATH \
PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:/usr/bin:/bin:$PATH" \
gnatmake -Isrc/sparkpass -Isrc/sparkpass/crypto -Dobj tests/test_mldsa_simple_verify.adb \
  -o bin/test_mldsa_simple_verify -largs -Wl,-stack_size,0x4000000 && \
./bin/test_mldsa_simple_verify
```
