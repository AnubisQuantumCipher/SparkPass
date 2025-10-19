# Blake2b Implementation Analysis for SPARK

**Date**: 2025-10-17
**Purpose**: Research phase for Phase 1 Blake2b implementation
**Target**: Pure SPARK Blake2b with 91%+ verification rate

---

## Executive Summary

Blake2b is a cryptographic hash function producing 64-byte (512-bit) digests, optimized for 64-bit platforms. It will serve as the foundation for Argon2id (Phase 2) in the SparkPass pure SPARK migration.

**Key Findings**:
1. SPARKNaCl SHA-512 provides excellent proof patterns applicable to Blake2b
2. Blake2b has simpler structure than SHA-512 (12 rounds vs 80 rounds)
3. Expected verification: 127 VCs, ~115 proven (91%)
4. Critical dependency: Rotation operations must use Ada intrinsics

**References**:
- RFC 7693: https://datatracker.ietf.org/doc/html/rfc7693
- SPARKNaCl: `/Users/sicarii/.local/share/alire/releases/sparknacl_4.0.1_8e3cc2e6/src/`
- Blake2 reference: https://github.com/BLAKE2/BLAKE2

---

## 1. SPARKNaCl SHA-512 Proof Patterns

### 1.1 Type Definitions

**SPARKNaCl Pattern**:
```ada
subtype Digest is Bytes_64;  -- 64-byte output
type U64_Seq_8 is array (Index_8) of U64;  -- State array
type U64_Seq_16 is array (Index_16) of U64;  -- Message schedule
```

**Key Insights**:
- Use fixed-size arrays (not unconstrained) for state
- Separate types for different conceptual entities (state vs message)
- Byte arrays for I/O, U64 arrays for internal computation

**Blake2b Application**:
```ada
subtype Hash_Type is Byte_Array (1 .. 64);  -- Match SPARKNaCl naming
type State_Words is array (0 .. 7) of U64;  -- 8x 64-bit state
type Work_Vector is array (0 .. 15) of U64;  -- 16x 64-bit working vector
subtype Block_Type is Byte_Array (1 .. 128);  -- 128-byte blocks
```

### 1.2 Rotation and Shift Operations

**SPARKNaCl Pattern**:
```ada
function LC_Sigma0 (X : in U64) return U64
is (Rotate_Right (X, 1) xor
    Rotate_Right (X, 8) xor
    Shift_Right (X, 7))
  with Global => null;
```

**Key Insights**:
- Use expression functions for bitwise operations
- `Rotate_Right` is safe (Ada intrinsic, no overflow)
- `Shift_Right` proven automatically
- Combine with XOR in single expression

**Blake2b Application**:
```ada
-- Blake2b uses rotations: 32, 24, 16, 63 bits
function Rotr32 (X : U64) return U64 is (Rotate_Right (X, 32))
  with Global => null, Inline;
function Rotr24 (X : U64) return U64 is (Rotate_Right (X, 24))
  with Global => null, Inline;
function Rotr16 (X : U64) return U64 is (Rotate_Right (X, 16))
  with Global => null, Inline;
function Rotr63 (X : U64) return U64 is (Rotate_Right (X, 63))
  with Global => null, Inline;
```

### 1.3 Loop Invariants

**SPARKNaCl Pattern** (Hashblocks_512, line 193-197):
```ada
while (LN >= 128) loop
   pragma Loop_Variant (Decreases => LN);
   pragma Loop_Invariant
     ((LN + I64 (CB) = I64 (M'Last) + 1) and
      (LN in 128 .. M'Length) and
      (CB in M'First .. (M'Last - 127)));
```

**Key Insights**:
- Single compound invariant (not multiple pragmas)
- Establish relationship between loop counter and array bounds
- Prove remaining length stays within bounds
- Use `Loop_Variant` for termination

**Blake2b Application**:
```ada
-- Main block processing loop
while Offset <= Message'Last - 127 loop
   pragma Loop_Variant (Increases => Offset);
   pragma Loop_Invariant
     (Offset in Message'First .. Message'Last - 127 and
      Offset mod 128 = Message'First mod 128);

   -- Process block at Offset
   Compress (State, Message (Offset .. Offset + 127), ...);
   Offset := Offset + 128;
end loop;
```

### 1.4 Inner Compression Loop

**SPARKNaCl Pattern** (80 rounds, line 216-248):
```ada
for I in Index_80 loop
   pragma Loop_Optimize (No_Unroll);
   pragma Loop_Invariant ((LN + I64 (CB) = I64 (M'Last) + 1) and
                          (LN in 128 .. M'Length) and
                          (CB in M'First .. (M'Last - 127)));
   B := A;
   T := A (7) + UC_Sigma1 (A (4)) + Ch (A (4), A (5), A (6)) +
        K_512 (I) + W (I mod 16);
   -- ... rotate state ...
end loop;
```

**Key Insights**:
- `pragma Loop_Optimize (No_Unroll)` prevents code bloat
- Carry outer loop invariants into inner loop
- Use temporary variable to simplify state updates
- Message schedule accessed via `W (I mod 16)`

**Blake2b Application**:
```ada
-- Blake2b has 12 rounds (much simpler than SHA-512's 80!)
for Round in 0 .. 11 loop
   pragma Loop_Optimize (No_Unroll);
   pragma Loop_Invariant (V'Length = 16);

   -- Apply G function 8 times per round using sigma permutations
   -- Round % 10 selects permutation (rounds 10-11 repeat 0-1)
   G (V, 0, 4,  8, 12, M (Sigma (Round mod 10, 0)), M (Sigma (Round mod 10, 1)));
   G (V, 1, 5,  9, 13, M (Sigma (Round mod 10, 2)), M (Sigma (Round mod 10, 3)));
   -- ... 6 more G calls ...
end loop;
```

### 1.5 Byte Packing/Unpacking

**SPARKNaCl Pattern** (Big_Endian_Unpack, line 67-91):
```ada
function Big_Endian_Unpack (Input : in U64) return Bytes_8
is
   Output : Bytes_8 with Relaxed_Initialization;
   X      : U64 := Input;
begin
   Output (Output'Last) := Byte (X mod 256);

   for I in reverse Index_8'First .. (Index_8'Last - 1) loop
      X := Shift_Right (X, 8);
      Output (I) := Byte (X mod 256);

      pragma Loop_Invariant (
        (X = Shift_Right (Input, Integer (Index_8'Last - I) * 8)) and then
        (Output (I .. Index_8'Last)'Initialized) and then
        (for all J in I .. Index_8'Last => Output (J) =
          Big_Endian_Get_Byte (Input, J)));
   end loop;

   return Output;
end Big_Endian_Unpack;
```

**Key Insights**:
- Use `Relaxed_Initialization` for partial initialization
- Ghost function (`Big_Endian_Get_Byte`) for postconditions
- Loop invariant proves correctness incrementally
- Quantified expressions in postconditions

**Blake2b Application**:
- Blake2b uses **little-endian** byte order (different from SHA-512!)
- Need corresponding `Little_Endian_Pack` and `Little_Endian_Unpack`
- Reuse same proof pattern, just reverse byte order

### 1.6 Padding Strategy

**SPARKNaCl Pattern** (Hash_512_Core, line 274-308):
```ada
procedure Hash_512_Core (Output :    out Bytes_64;
                         IV     : in     Bytes_64;
                         M      : in     Byte_Seq)
is
   subtype Final_Block_Length is I32 range 0 .. 127;
   H     : Bytes_64 := IV;
   X     : Bytes_256;  -- Buffer for final block(s)
   B     : Final_Block_Length;
begin
   Hashblocks_512 (H, M);  -- Process all complete blocks

   B := Final_Block_Length (I64 (M'Length) mod 128);

   if B > 0 then
      Final_Block_First := (M'Last - B) + 1;
      X (0 .. B - 1) := M (Final_Block_First .. M'Last);
   end if;
   X (B) := 128;  -- Padding byte

   -- Append length and process final block(s)
   -- ...
end Hash_512_Core;
```

**Key Insights**:
- Use bounded subtype for final block length (proves bounds)
- Separate processing of complete blocks vs final block
- Use local buffer for padding (no heap allocation)
- Handle case where padding doesn't fit in one block

**Blake2b Difference**:
- Blake2b has no explicit padding (simpler!)
- Final block flag in compression function
- Length encoded in finalization, not padding

---

## 2. Blake2b Algorithm Specification

### 2.1 Core Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| Word size | 64 bits | Optimized for 64-bit platforms |
| Rounds | 12 | Much simpler than SHA-512's 80 |
| Block size | 128 bytes | 16x 64-bit words |
| Hash length | 1-64 bytes | Variable output (we'll use 64) |
| Key length | 0-64 bytes | Optional keying (we'll use 0) |
| Rotations | 32, 24, 16, 63 | Fixed constants |

### 2.2 Initialization Vector (IV)

From RFC 7693 Section 2.6:

```ada
IV : constant State_Words :=
  (16#6A09E667F3BCC908#,
   16#BB67AE8584CAA73B#,
   16#3C6EF372FE94F82B#,
   16#A54FF53A5F1D36F1#,
   16#510E527FADE682D1#,
   16#9B05688C2B3E6C1F#,
   16#1F83D9ABFB41BD6B#,
   16#5BE0CD19137E2179#);
```

**Derivation**: `IV[i] = floor(2^64 * frac(sqrt(prime(i+1))))`
Primes: 2, 3, 5, 7, 11, 13, 17, 19

### 2.3 Sigma Permutations

Blake2b uses 10 permutations, cycling for 12 rounds:

```ada
type Sigma_Row is array (0 .. 15) of Natural range 0 .. 15;
type Sigma_Table is array (0 .. 9) of Sigma_Row;

Sigma : constant Sigma_Table :=
  ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),  -- Round 0
   (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3),  -- Round 1
   (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4),  -- Round 2
   (7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8),  -- Round 3
   (9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13),  -- Round 4
   (2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9),  -- Round 5
   (12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11),  -- Round 6
   (13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10),  -- Round 7
   (6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5),  -- Round 8
   (10, 4, 13, 8, 7, 12, 9, 1, 3, 11, 14, 5, 0, 6, 2, 15)); -- Round 9
```

### 2.4 G Function (Mixing Function)

RFC 7693 Section 3.1:

```
G(v, a, b, c, d, x, y):
    v[a] := (v[a] + v[b] + x) mod 2^64
    v[d] := (v[d] ^ v[a]) >>> 32
    v[c] := (v[c] + v[d]) mod 2^64
    v[b] := (v[b] ^ v[c]) >>> 24
    v[a] := (v[a] + v[b] + y) mod 2^64
    v[d] := (v[d] ^ v[a]) >>> 16
    v[c] := (v[c] + v[d]) mod 2^64
    v[b] := (v[b] ^ v[c]) >>> 63
```

**SPARK Implementation Strategy**:
```ada
procedure G
  (V : in out Work_Vector;
   A, B, C, D : in Natural;
   X, Y : in U64)
with
  Global => null,
  Pre    => A in V'Range and B in V'Range and
            C in V'Range and D in V'Range and
            A /= B and A /= C and A /= D and
            B /= C and B /= D and C /= D,
  Post   => V'Length = V'Length'Old;
is
begin
   V (A) := V (A) + V (B) + X;
   V (D) := Rotr32 (V (D) xor V (A));
   V (C) := V (C) + V (D);
   V (B) := Rotr24 (V (B) xor V (C));
   V (A) := V (A) + V (B) + Y;
   V (D) := Rotr16 (V (D) xor V (A));
   V (C) := V (C) + V (D);
   V (B) := Rotr63 (V (B) xor V (C));
end G;
```

**Expected VCs**: 15
- 4 pre-condition checks (indices in range, distinct)
- 8 operation overflow checks (will need annotations)
- 1 post-condition check
- 2 array bounds checks

### 2.5 Compression Function F

RFC 7693 Section 3.2:

```
F(h[0..7], m[0..15], t, f):
    // Initialize work vector v[0..15]
    v[0..7] := h[0..7]
    v[8..15] := IV[0..7]
    v[12] := v[12] ^ t0  // Offset counter low
    v[13] := v[13] ^ t1  // Offset counter high
    if f:
        v[14] := v[14] ^ 0xFFFFFFFFFFFFFFFF  // Final block flag

    // 12 rounds of mixing
    for round in 0..11:
        s := sigma[round % 10]
        G(v, 0, 4,  8, 12, m[s[0]], m[s[1]])
        G(v, 1, 5,  9, 13, m[s[2]], m[s[3]])
        G(v, 2, 6, 10, 14, m[s[4]], m[s[5]])
        G(v, 3, 7, 11, 15, m[s[6]], m[s[7]])
        G(v, 0, 5, 10, 15, m[s[8]], m[s[9]])
        G(v, 1, 6, 11, 12, m[s[10]], m[s[11]])
        G(v, 2, 7,  8, 13, m[s[12]], m[s[13]])
        G(v, 3, 4,  9, 14, m[s[14]], m[s[15]])

    // Finalization
    for i in 0..7:
        h[i] := h[i] ^ v[i] ^ v[i+8]
```

**Expected VCs**: 50
- 12 iterations × 8 G calls = 96 G calls (each ~5 VCs = 480 total)
- Loop invariants: 10 VCs
- Finalization: 8 VCs

**Optimization**: Pre-compute sigma indices to reduce VCs

### 2.6 Full Blake2b Algorithm

```
Blake2b(input):
    h[0..7] := IV[0..7]
    h[0] := h[0] ^ 0x01010040  // Parameter block (64-byte output, no key)

    if input.length > 128:
        while input has complete 128-byte blocks:
            F(h, next_block, bytes_processed, false)

    // Final block (may be partial or empty)
    F(h, final_block, input.length, true)

    return first 64 bytes of h[0..7]
```

**Expected VCs**: 62
- Initialization: 10 VCs
- Block loop: 20 VCs
- Final block handling: 15 VCs
- Output extraction: 12 VCs
- Overflow checks: 5 VCs

---

## 3. Comparison: SHA-512 vs Blake2b

| Aspect | SHA-512 | Blake2b | Advantage |
|--------|---------|---------|-----------|
| Rounds | 80 | 12 | Blake2b: Simpler |
| Message schedule | 16-word sliding window | 16-word permuted | Blake2b: More regular |
| Padding | Complex (128-bit length) | None (final flag) | Blake2b: Simpler |
| Endianness | Big-endian | Little-endian | Neutral |
| Finalization | Unpack state | XOR state halves | Blake2b: Simpler |
| Proof complexity | High (80 iterations) | Moderate (12 iterations) | Blake2b: Easier |

**Conclusion**: Blake2b should be **easier to verify** than SHA-512 due to:
1. Fewer rounds (12 vs 80)
2. Simpler padding (none vs complex)
3. More regular structure (permutation table vs conditional logic)

---

## 4. Verification Strategy

### 4.1 Phased Implementation

**Phase 1: G Function** (Day 1)
- Implement rotation helpers
- Implement G with full contracts
- Target: 15 VCs, 100% proven
- Validation: Unit tests with known vectors

**Phase 2: Compression Function F** (Days 2-3)
- Implement work vector initialization
- Implement 12-round loop with invariants
- Implement finalization XOR
- Target: 50 VCs, ~90% proven
- Validation: Single-block test vectors

**Phase 3: Full Blake2b** (Days 4-5)
- Implement parameter block initialization
- Implement multi-block processing
- Implement final block handling
- Target: 62 VCs, ~85% proven
- Validation: RFC 7693 test vectors

**Phase 4: Optimization** (Days 6-7)
- Add intermediate assertions for unproven VCs
- Optimize contract structure
- Run at proof level 3 for stubborn VCs
- Target: 127 total VCs, 115+ proven (91%)

### 4.2 Key Proof Techniques

**1. Rotation Safety**:
```ada
-- No proof needed - Ada intrinsic is safe
function Rotr32 (X : U64) return U64 is (Rotate_Right (X, 32))
  with Global => null, Inline;
```

**2. Addition Overflow**:
```ada
-- Ada U64 addition wraps by default (mod 2^64 semantics)
-- No explicit contract needed, but can add assertion:
V (A) := V (A) + V (B) + X;
pragma Assert (V (A)'Valid);  -- Should prove automatically
```

**3. Array Indexing**:
```ada
-- Strong precondition ensures safety
Pre => A in V'Range and B in V'Range and
       C in V'Range and D in V'Range
-- SPARK will prove all array accesses safe
```

**4. Loop Termination**:
```ada
for Round in 0 .. 11 loop
   pragma Loop_Variant (Increases => Round);  -- Automatic termination
   pragma Loop_Invariant (V'Length = 16);     -- Structural invariant
```

**5. Quantified Expressions** (if needed):
```ada
Post => (for all I in State'Range => State (I)'Valid)
```

### 4.3 Expected Unproven VCs

Based on SPARKNaCl experience, expect ~12 unproven VCs (9%):

1. **Complex arithmetic expressions** (5 VCs)
   - Mitigation: Break into intermediate assertions
   - Example: `T := A + B + C` → separate into `T1 := A + B; T := T1 + C`

2. **Loop invariant stability** (4 VCs)
   - Mitigation: Add intermediate ghost variables
   - Example: Track permutation validity across rounds

3. **Final block boundary conditions** (3 VCs)
   - Mitigation: Use proof by cases (empty, partial, full)
   - Example: Separate handling of 0-byte, 1-127 byte, 128-byte cases

**Justification for unproven VCs**:
- Not safety-critical (no memory safety issues)
- Mathematical properties beyond SPARK's decidability
- Would require extensive lemma library (out of scope for Phase 1)

### 4.4 Marmaragan Methodology Application

For Blake2b compression function F (moderate complexity, n=6):

**n=6**: Moderate algorithmic complexity
- Straightforward algorithm (12-round loop)
- Regular structure (permutation table)
- Limited branching (final block flag only)

**r=1**: Minimal proof refinement
- Use single compound loop invariants
- Avoid complex ghost predicates
- Rely on SPARK's automatic proofs
- Only add assertions for stubborn VCs

**Target**: 91% proof rate (acceptable for Gold tier, good for Phase 1)

---

## 5. Test Vectors (RFC 7693 Appendix A)

### 5.1 Empty Message

**Input**: `""` (0 bytes)

**Blake2b-512 Output**:
```
786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419
d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce
```

### 5.2 "abc" Message

**Input**: `"abc"` (3 bytes: 0x61 0x62 0x63)

**Blake2b-512 Output**:
```
ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d1
7d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923
```

### 5.3 Long Message (recommended)

Test with 1000-byte, 10000-byte messages to validate multi-block processing.

### 5.4 Test Harness Structure

```ada
with SparkPass.Crypto.Blake2b;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Blake2b_Vectors is
   use SparkPass.Types;
   use SparkPass.Crypto.Blake2b;

   procedure Test_Empty is
      Input : constant Byte_Array (1 .. 0) := (others => 0);
      Expected : constant Hash_Type := (...);  -- From above
      Result : Hash_Type;
   begin
      Hash (Input, Result);
      if Result = Expected then
         Put_Line ("PASS: Empty message");
      else
         Put_Line ("FAIL: Empty message");
      end if;
   end Test_Empty;

   -- Similar for "abc", long messages...

begin
   Test_Empty;
   Test_ABC;
   Test_Long;
   Put_Line ("All tests complete");
end Test_Blake2b_Vectors;
```

---

## 6. Integration with Argon2id

### 6.1 Argon2id Requirements

Argon2id uses Blake2b in two modes:

**Mode 1: Variable-length hash (H')**
- Used for domain-separated hashing
- Output: 1-64 bytes (variable)
- Implementation: Truncate 64-byte Blake2b output

**Mode 2: Compression function (G)**
- Used directly in Argon2 G function
- Operates on 1024-byte blocks
- Implementation: Call Blake2b compression function F

### 6.2 API Extensions Needed

```ada
-- Variable-length hash for H'
procedure Blake2b_Variable_Length
  (Message : in  Byte_Array;
   Output  : out Byte_Array)
with
  Pre  => Output'Length in 1 .. 64,
  Post => Output'Length = Output'Length'Old;

-- Direct compression function access for Argon2 G
procedure Blake2b_Compress
  (State  : in out State_Words;
   Block  : in     Byte_Array;
   Offset : in     U64;
   Final  : in     Boolean)
with
  Pre => State'Length = 8 and Block'Length = 128;
```

### 6.3 Performance Requirements

Argon2id requires **high throughput** for KDF operations:

**Target**: 200 MB/s on modern 64-bit CPU
- Blake2b reference (C): ~1 GB/s
- SPARK overhead: ~5x slowdown expected
- Result: ~200 MB/s (acceptable for KDF)

**Optimization strategy**:
- Inline rotation functions
- Use `pragma Loop_Optimize (No_Unroll)` sparingly
- Avoid heap allocations (all stack)
- Consider assembly for critical path (Phase 3 optimization)

---

## 7. Action Items

### Immediate (Day 1)
1. Create `sparkpass-crypto-blake2b.ads` with type definitions
2. Implement rotation helper functions
3. Implement G function with full contracts
4. Create basic test harness

### Short-term (Days 2-5)
1. Implement compression function F
2. Implement full Blake2b hash
3. Validate against RFC 7693 test vectors
4. Run gnatprove at level 2

### Medium-term (Days 6-7)
1. Analyze unproven VCs
2. Add intermediate assertions
3. Document proof strategies
4. Prepare for Argon2id integration

---

## 8. Success Criteria

### Functional
- [ ] Compiles without errors
- [ ] Passes RFC 7693 test vectors (empty, "abc", long)
- [ ] No runtime exceptions
- [ ] Constant-time operations (no branches on secrets)

### Verification
- [ ] 115+ / 127 VCs proven (91%)
- [ ] No memory safety issues
- [ ] All overflow checks proven or justified
- [ ] Flow analysis: 100% (no data flow issues)

### Performance
- [ ] >= 200 MB/s throughput
- [ ] No heap allocations
- [ ] Stack usage < 2KB

### Documentation
- [ ] Proof strategies documented
- [ ] Unproven VCs justified
- [ ] Integration guide for Argon2id
- [ ] Test vector compliance report

---

## 9. Risk Assessment

### Low Risk
- G function implementation (simple, well-defined)
- Rotation operations (Ada intrinsic, safe)
- Test vector validation (straightforward)

### Medium Risk
- Compression function loop invariants (may need iteration)
- Multi-block processing (boundary conditions)
- Final block handling (edge cases)

### High Risk (but manageable)
- Achieving 91% proof rate (may require proof tuning)
- Constant-time guarantees (requires careful review)
- Performance target (may need optimization in Phase 3)

### Mitigation Strategies
1. Implement incrementally (G → F → Full)
2. Test at each stage (unit tests + test vectors)
3. Run gnatprove frequently (catch issues early)
4. Use SPARKNaCl patterns (proven to work)
5. Accept some unproven VCs (document and justify)

---

## 10. Conclusion

Blake2b is an **excellent foundation** for SparkPass Phase 1:

**Advantages**:
1. Simpler than SHA-512 (12 vs 80 rounds)
2. Well-specified (RFC 7693, official test vectors)
3. Reference implementations available (libsodium, blake2b.net)
4. Required for Argon2id (immediate use case)

**Challenges**:
1. Little-endian byte order (different from SHA-512)
2. Permutation table complexity (10 different permutations)
3. Achieving 91% proof rate (requires discipline)

**Realistic Timeline**: 2-3 weeks
- Week 1: Core implementation (G, F, Hash)
- Week 2: Verification and testing
- Week 3: Documentation and integration prep

**Confidence Level**: HIGH
- SPARKNaCl provides proven patterns
- Blake2b is simpler than SHA-512
- Clear path from here to Argon2id

---

## References

1. **RFC 7693**: BLAKE - A Cryptographic Hash and Message Authentication Code
   https://datatracker.ietf.org/doc/html/rfc7693

2. **SPARKNaCl**: Verified cryptographic library in SPARK
   `/Users/sicarii/.local/share/alire/releases/sparknacl_4.0.1_8e3cc2e6/src/`

3. **Blake2 Reference Implementation**:
   https://github.com/BLAKE2/BLAKE2

4. **SPARK User Guide**: Loop Invariants and Variants
   https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/

5. **Marmaragan SPARK Methodology**: Proof complexity tiers
   (Internal SparkPass documentation)

---

**End of Analysis** - Ready to proceed with implementation!
