# Zeta_Index Bounds Analysis and Fix Attempt

**Date:** 2025-10-20
**Session:** Continuation - Attempting 100% Silver Level
**Status:** Analysis Complete | Bounds Issue Understood

---

## Problem Statement

GNATprove reported 1 unproven assertion in NTT:
```
sparkpass-crypto-mlkem-ntt.adb:154:48: medium: assertion might fail, cannot prove Zeta_Index <= 127
```

This was preventing 100% Silver Level verification (98% → 100% goal).

---

## Root Cause Analysis

### Algorithm Structure

NTT uses 127 twiddle factors across 7 layers:
```
Len=128:   1 block  ×  1 zeta =   1 zetas   (indices: 1)
Len= 64:   2 blocks ×  1 zeta =   2 zetas   (indices: 2-3)
Len= 32:   4 blocks ×  1 zeta =   4 zetas   (indices: 4-7)
Len= 16:   8 blocks ×  1 zeta =   8 zetas   (indices: 8-15)
Len=  8:  16 blocks ×  1 zeta =  16 zetas   (indices: 16-31)
Len=  4:  32 blocks ×  1 zeta =  32 zetas   (indices: 32-63)
Len=  2:  64 blocks ×  1 zeta =  64 zetas   (indices: 64-127)

Total: 127 zetas (Zeta_Index: 1 → 128)
```

###Loop Invariant Challenge

**Code Pattern:**
```ada
Zeta_Index : Natural := 1;  -- Start at 1

-- Outer loop invariant
pragma Loop_Invariant (Zeta_Index >= 1 and Zeta_Index <= 128);

-- Middle loop
while Start < 256 loop
   pragma Loop_Invariant (Zeta_Index >= 1 and Zeta_Index <= 128);

   -- THE PROBLEM:
   pragma Assert (Zeta_Index >= 0 and Zeta_Index <= 127);  -- FAILS

   Zeta := Zeta_BitRev(Zeta_Index);  -- Needs Zeta_Index in 0..127
   Zeta_Index := Zeta_Index + 1;     -- Increments to 128 at end
end loop;
```

**The Dilemma:**
- Loop invariant allows `Zeta_Index <= 128` (needed for final iteration validity)
- Assertion requires `Zeta_Index <= 127` (needed for array access)
- SMT solver cannot prove 128 ≤ 127

---

## Attempted Fixes

### Attempt 1: Tighten Loop Invariant to `<= 127`

**Change:**
```ada
pragma Loop_Invariant (Zeta_Index >= 1 and Zeta_Index <= 127);
```

**Result:** ❌ FAILED
- Broke 2 additional proofs (NTT: 53/54 → 52/54)
- Cannot prove loop invariant preservation after increment:
  - `Zeta_Index := 127; Zeta_Index + 1 = 128`
  - But invariant requires `Zeta_Index <= 127`
  - Contradiction!

### Attempt 2: Remove Redundant Assertion

**Change:**
```ada
-- REMOVED: pragma Assert (Zeta_Index >= 0 and Zeta_Index <= 127);
-- Note: Zeta_Index in 1..128, but only values 1..127 are used (before increment)
Zeta := Zeta_BitRev(Zeta_Index);
```

**Rationale:**
- The array index check on `Zeta_BitRev(Zeta_Index)` IS the proof obligation
- The assertion was redundant
- Let SMT prove the index check directly using loop invariant at line 149:
  ```ada
  pragma Loop_Invariant (Zeta_Index <= 127 + (256 - Start) / (2 * Len));
  ```

**Result:** ⏳ TO BE TESTED

---

## Why The Assertion Exists

Looking at the code history, the assertion was likely added to:
1. Make the proof obligation explicit
2. Help SMT solvers by stating the property directly
3. Debug why index checks were failing

However, it backfired because:
- Loop invariant `<= 128` is necessary for correctness
- Assertion `<= 127` contradicts this
- SMT cannot resolve the apparent contradiction

---

## The Correct Approach

### Option A: Remove Assertion (Chosen)

Let the natural array index check serve as the proof obligation:
```ada
Zeta := Zeta_BitRev(Zeta_Index);
-- GNATprove must prove: Zeta_Index in Zeta_BitRev'Range (0..127)
```

Rely on the tighter loop invariant at line 149:
```ada
pragma Loop_Invariant (Zeta_Index <= 127 + (256 - Start) / (2 * Len));
```

When `Start < 256`, this proves `Zeta_Index <= 127` at the point of use.

### Option B: Conditional Assertion (Alternative)

```ada
if Zeta_Index <= 127 then
   pragma Assert (Zeta_Index in 0..127);
   Zeta := Zeta_BitRev(Zeta_Index);
   Zeta_Index := Zeta_Index + 1;
end if;
```

But this changes the algorithm structure and is unnecessary.

### Option C: Proof Hint (Most Explicit)

```ada
pragma Assert (Start < 256);  -- From loop condition
pragma Assert (Len in 2 | 4 | 8 | 16 | 32 | 64 | 128);  -- From invariant
pragma Assert (Zeta_Index <= 127 + (256 - Start) / (2 * Len));  -- From invariant
-- Therefore: Zeta_Index <= 127 when Start > 0
Zeta := Zeta_BitRev(Zeta_Index);
```

---

## Mathematical Proof

**Claim:** `Zeta_Index ∈ [1, 127]` before each array access.

**Proof:**

1. **Initialization:** `Zeta_Index = 1` ✓

2. **Invariant:** `Zeta_Index ≤ 127 + (256 - Start) / (2 × Len)`

3. **At First Use in Loop:**
   - `Start ∈ {0, 2×Len, 4×Len, ..., 256-2×Len}`  (from `Start mod (2×Len) = 0`)
   - When `Start = 0`: `Zeta_Index ≤ 127 + 256/(2×Len) = 127 + 128/Len`

4. **Maximum Zeta_Index Before Increment:**
   - At `Len = 2`, `Start = 254`: `Zeta_Index ≤ 127 + 2/4 = 127` ✓

5. **After 127 Uses:**
   - `Zeta_Index = 128`
   - Loop condition `Start < 256` becomes false
   - No more iterations → Zeta_Index never exceeds 128 ✓

**QED:** The algorithm is correct; the assertion was overly restrictive.

---

## Verification Status

### Before Fix
- NTT: 53 out of 54 checks proved (98.1%)
- INTT: 66 out of 67 checks proved (98.5%)
- **Overall NTT Module:** 98.3% Silver Level

### After Assertion Removal  (To Be Verified)
- **Target:** 54/54 (NTT) + 67/67 (INTT) = 100% Silver Level
- **Expected:** Index check proves via loop invariant line 149

---

## Lessons Learned

### 1. Redundant Assertions Can Harm Proofs
- Adding `pragma Assert` doesn't always help
- Can introduce contradictions with loop invariants
- SMT solvers may get confused

### 2. Loop Invariants Must Allow Valid End States
- `Zeta_Index = 128` is a valid final state
- Invariant must permit this
- But we only access indices 1-127

### 3. Let Natural Checks Be Proof Obligations
- Array index checks are automatic
- Don't duplicate with explicit assertions unless necessary
- Trust the SMT solver to use available invariants

### 4. Complex Invariants Are More Powerful
- Line 149: `Zeta_Index <= 127 + (256 - Start) / (2 * Len)`
- This is tighter than `Zeta_Index <= 128`
- Should be sufficient for index check proof

---

## Next Steps

1. **Verify the fix works:**
   ```bash
   gnatprove -P sparkpass.gpr --mode=prove --level=2 \
     -u sparkpass-crypto-mlkem-ntt.adb
   ```

2. **If successful:**
   - Document 100% Silver Level achievement
   - Update status documents
   - Proceed with axiomatic Gold Level verification

3. **If still failing:**
   - Add explicit proof hint (Option C above)
   - Or strengthen loop invariant further

---

## Alternative: Accept 98% As Excellent

**Pragmatic View:**
- 98% Silver Level is already excellent
- The remaining 2% is a technicality
- Algorithm is correct (127 values used, indices 1-127)
- May not be worth extensive loop invariant engineering

**However:**
- 100% is achievable with the right approach
- Demonstrates mastery of SPARK verification
- Provides clean foundation for Gold Level work

---

## Conclusion

The Zeta_Index bounds issue is **understood** and a fix has been **attempted**. The root cause is a conflict between:
1. Loop invariant allowing `Zeta_Index = 128` (correct, necessary)
2. Explicit assertion requiring `Zeta_Index ≤ 127` (overly restrictive)

**Solution:** Remove redundant assertion, rely on natural index check with existing complex loop invariant.

**Status:** Code modified, awaiting verification results.

---

**Last Updated:** 2025-10-20
**Next Action:** Test GNATprove with assertion removed
