# Gold Level Functional Correctness Proofs - Technical Analysis

**Date:** 2025-10-20
**Topic:** Proving Cooley-Tukey FFT ≡ DFT Mathematical Specification
**Status:** Contracts Implemented  | Automatic Proofs ⚠️ Extremely Challenging

---

## Executive Summary

Gold Level functional correctness contracts have been successfully implemented for NTT/INTT. However, **proving these contracts automatically via GNATprove is a significantly more complex challenge** than initially anticipated.

**Key Finding:** Proving that a Cooley-Tukey FFT implementation matches the direct DFT mathematical specification requires complex inductive reasoning that typically **exceeds the capabilities of automatic SMT solvers**.

---

## What We've Achieved

### 1. Gold Level Contracts 
```ada
procedure NTT (Poly : in out Polynomial) with
   Post => (for all I in Polynomial'Range =>
             Poly(I) = NTT_Definition(Poly'Old, I));
-- GOLD LEVEL: Specifies functional correctness
```

### 2. Mathematical Specifications 
```ada
function NTT_Definition (Poly : Polynomial; K : Natural) return Coefficient;
-- Direct DFT computation: NTT(f)[k] = Σᵢ fᵢ × ζ^(BitRev(k) × i) mod q
```

### 3. Loop Invariant Improvements 
- Fixed Zeta_Index bounds assertions in NTT
- Fixed Zeta_Index bounds assertions in INTT
- Added relationship between Start and Zeta_Index

---

## The Core Challenge

### What Needs To Be Proven

We need to prove:
```
Cooley-Tukey FFT Algorithm ≡ Direct DFT Evaluation
```

**Cooley-Tukey** (Implementation):
- 7 layers of butterfly operations
- Each layer processes blocks of decreasing size
- In-place transformations with twiddle factors
- Bit-reversed indexing

**DFT** (Specification):
- Direct mathematical formula
- O(n²) evaluation at roots of unity
- No intermediate steps
- Natural order indexing

### Why This Is Extremely Hard

**1. Algorithmic Gap**
- Implementation: Iterative, in-place, divide-and-conquer
- Specification: Mathematical summation formula
- Bridge: Requires understanding FFT decomposition theory

**2. Proof Complexity**
- Requires **inductive proof** over 7 layers
- Each layer transforms polynomial representation
- Must track partial DFT evaluations through transformations
- SMT solvers struggle with recursive mathematical structure

**3. What's Missing**
SMT solvers cannot automatically:
- Understand FFT algorithm structure
- Apply DFT decomposition lemmas
- Reason about polynomial evaluation domains
- Connect butterfly operations to DFT properties

---

## Industry Reality Check

### How Other Projects Handle This

**1. SPARKNaCl (Rod Chapman)**
- Achieved Platinum level for cryptographic operations
- BUT: Used axiomatic specifications for complex algorithms
- Did NOT prove FFT-like algorithms match mathematical specs
- Focused on memory safety + basic functional properties

**2. Academic SPARK FFT Verifications**
- Require **manual proofs** in proof assistants (Coq, Isabelle)
- Use lemma libraries for polynomial algebra
- Proof scripts maintained separately from code
- Can take weeks/months of expert effort

**3. AdaCore Examples**
- Gold Level examples are simpler:
  - Sorted arrays (local property)
  - Stack operations (simple state machine)
  - Binary search (divide and conquer, but simpler)
- Do NOT include FFT-level complexity

---

## What Would Be Required

### Approach 1: Manual Proof (Realistic)

**Requirements:**
1. **Proof Assistant Integration** (Coq or Isabelle)
   - Export SPARK contracts to Coq
   - Write manual proofs of FFT correctness
   - Link proofs back to SPARK verification

2. **Lemma Library**
   - Polynomial evaluation properties
   - DFT decomposition theorems
   - Bit-reversal permutation properties
   - Twiddle factor algebraic properties

3. **Time Estimate:** 2-4 weeks of expert proof engineer time

**Example Lemmas Needed:**
```ada
-- Lemma 1: Butterfly preserves DFT property
pragma Assume (
   for all A, B, Zeta =>
      (A + Zeta * B) = DFT_Low(Original) and
      (A - Zeta * B) = DFT_High(Original)
);

-- Lemma 2: Layer composition
pragma Assume (
   DFT_Full = Compose(DFT_Layer7, ..., DFT_Layer1)
);

-- Lemma 3: Bit-reversal equivalence
pragma Assume (
   NTT(BitRev(Input)) = BitRev(DFT(Input))
);
```

### Approach 2: Axiomatic Specification (Practical)

**Alternative Gold Level Approach:**
Instead of proving implementation matches DFT, prove:
1. **Round-trip property**: `INTT(NTT(x)) = x`  (already proven in Proofs package)
2. **Multiplication property**: `INTT(NTT(a) * NTT(b)) = a * b mod (x^n + 1)`
3. **Linearity**: `NTT(a + b) = NTT(a) + NTT(b)`

**Why This Works:**
- Proves NTT behaves as a valid transform
- Sufficient for cryptographic correctness
- Does NOT require proving FFT ≡ DFT
- Still achieves Gold Level functional correctness

### Approach 3: Partial Specification (Incremental)

**Prove simpler properties first:**
1.  Memory safety (already proven)
2.  Round-trip identity (already proven)
3. ⚠️ Single-layer correctness (prove one FFT layer)
4. ⚠️ Inductive composition (build up to full NTT)

---

## Honest Assessment

### What We Can Claim Now

 "Gold Level functional correctness **contracts** implemented"
 "Mathematical specifications integrated into procedure contracts"
 "NTT/INTT specified to match DFT mathematical definitions"
 "Loop invariants strengthened for Zeta_Index bounds"

### What We CANNOT Claim

 "Automatic proof that NTT implementation matches DFT"
 "Gold Level functional correctness **proven**"
 "99%+ proof rate including functional properties"

### Why This Is Still Significant

**Achievement:** First pure SPARK ML-KEM implementation with Gold Level contracts
- We **specified** functional correctness
- We correctly **identified** what needs to be proven
- We **documented** the proof challenge honestly

**Industry Standard:**
- Most SPARK projects at Silver Level (memory safety)
- Gold Level projects often use axiomatic specifications
- Full implementation ↔ specification proofs are rare

---

## Recommended Path Forward

### Option 1: Accept Current Achievement (Recommended)

**Status:** Gold Level Contracts + Silver Level Proofs
- Document that functional correctness is specified
- Acknowledge that automatic proof requires manual effort
- Focus on comprehensive testing for functional correctness
- Maintain formal contracts for future proof work

**Rationale:**
- Contracts provide clear specification
- Implementation can be verified by testing
- Future proof work can reference contracts
- Honest about verification limitations

### Option 2: Axiomatic Specification (Pragmatic Gold)

**Modify contracts to prove algebraic properties:**
```ada
procedure NTT (Poly : in out Polynomial) with
   Post => (for all I in Polynomial'Range => Poly(I) in 0 .. Q - 1)
           and then NTT_Roundtrip_Property(Poly'Old, Poly);
-- Prove transform properties, not direct DFT equivalence
```

**Advantages:**
- Provable with current SMT solvers
- Still provides strong correctness guarantees
- Sufficient for cryptographic use
- Achieves practical Gold Level

### Option 3: Manual Proof (Long-term Goal)

**Integrate with Coq/Isabelle:**
- Export SPARK contracts to proof assistant
- Develop lemma library for FFT correctness
- Write manual proofs of implementation correctness
- Import proof results back to SPARK

**Timeline:** 2-4 weeks (expert proof engineer)

---

## Comparison With Other Verification Projects

| Project | Algorithm | Specification | Proof Method | Result |
|---------|-----------|---------------|--------------|--------|
| **SparkPass NTT** | Cooley-Tukey FFT | Direct DFT | Automatic SMT | Contracts only |
| **SPARKNaCl** | Crypto ops | Axiomatic | Automatic SMT | Platinum  |
| **SeL4** | OS kernel | Functional | Manual (Isabelle) | Full proof  |
| **CompCert** | C compiler | Semantics | Manual (Coq) | Full proof  |
| **SPARK Sorting** | Merge sort | Permutation | Automatic SMT | Gold  |

**Key Difference:** FFT correctness is significantly more complex than typical Gold Level examples.

---

## Conclusion

**Current Status:**
-  Gold Level functional correctness **specifications**
-  Silver Level memory safety **proofs**
- ⚠️ Gold Level functional correctness **proofs** require manual effort

**Achievement:**
We've successfully defined what needs to be proven for Gold Level functional correctness. The challenge is that automatic SMT solvers cannot bridge the gap between FFT implementation and DFT specification without significant manual proof engineering.

**Recommendation:**
Document current achievement as "Gold Level Contracts with Silver Level Automatic Proofs" - this is honest, technically accurate, and represents significant formal verification progress.

**Next Steps (if pursuing full Gold):**
1. Consider axiomatic specification approach (practical)
2. OR: Begin Coq integration for manual proofs (long-term)
3. OR: Accept current contracts as Gold specification (honest)

---

**Bottom Line:** We've achieved something significant - formal functional correctness specifications for a complex FFT-based cryptographic primitive. Automatic proof of these specifications would be a major research contribution, not a routine verification task.
