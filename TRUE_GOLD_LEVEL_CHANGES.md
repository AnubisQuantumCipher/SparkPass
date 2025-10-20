# TRUE Gold Level Changes - What's Actually Needed

## The Core Problem

**Current State:** Silver Level (safety only)
```ada
procedure NTT (Poly : in out Polynomial) with
   Post => (for all I in Polynomial'Range => Poly(I) in 0 .. Q - 1);
-- Translation: "Output won't overflow"
-- Level: SILVER (safety, not correctness)
```

**Gold Level Requirement:** Functional Correctness
```ada
procedure NTT (Poly : in out Polynomial) with
   Post => (for all I in Polynomial'Range =>
             Poly(I) = NTT_Definition(Poly'Old, I));
-- Translation: "Output matches mathematical NTT specification"
-- Level: GOLD (proves algorithm correctness)
```

---

## Why We Can Do This NOW

We ALREADY have the mathematical specifications in our proof package:
-  `NTT_Definition` (ghost function) - lines 182-187 of sparkpass-crypto-mlkem-ntt-proofs.ads
-  `INTT_Definition` (ghost function) - lines 196-201 of sparkpass-crypto-mlkem-ntt-proofs.ads
-  `Theorem_NTT_Roundtrip_Correct` - PROVEN (11/11 VCs)

We just need to USE these in the actual NTT/INTT procedure contracts!

---

## Required Changes

### File 1: `sparkpass-crypto-mlkem-ntt.ads`

**Change 1: NTT Procedure Postcondition**

**Before (line 100-103):**
```ada
procedure NTT (Poly : in out Polynomial) with
   Global => null,
   Pre    => True,
   Post   => (for all I in Polynomial'Range => Poly(I) in 0 .. Q - 1);
```

**After:**
```ada
with SparkPass.Crypto.MLKEM.NTT.Proofs; use SparkPass.Crypto.MLKEM.NTT.Proofs;

procedure NTT (Poly : in out Polynomial) with
   Global => null,
   Pre    => True,
   Post   => (for all I in Polynomial'Range => Poly(I) in 0 .. Q - 1)
             and then
             (for all I in Polynomial'Range =>
               Poly(I) = NTT_Definition(Poly'Old, I));
-- GOLD LEVEL: Proves output matches mathematical NTT specification
```

**Change 2: INTT Procedure Postcondition**

Find the INTT procedure declaration (around line 150-170) and add:

**After:**
```ada
procedure INTT (Poly : in out Polynomial) with
   Global => null,
   Pre    => True,
   Post   => (for all I in Polynomial'Range => Poly(I) in 0 .. Q - 1)
             and then
             (for all I in Polynomial'Range =>
               Poly(I) = INTT_Definition(Poly'Old, I));
-- GOLD LEVEL: Proves output matches mathematical INTT specification
```

**Change 3: Add Roundtrip Postcondition**

Add a new ghost procedure that proves the round-trip property:

```ada
procedure NTT_Roundtrip_Proof (Original : Polynomial) with
   Ghost,
   Global => null,
   Post   => (declare
                Temp : Polynomial := Original;
              begin
                NTT(Temp);
                INTT(Temp);
                (for all I in Polynomial'Range => Temp(I) = Original(I)));
-- GOLD LEVEL: Proves NTT(INTT(x)) = x as a contract, not just a comment
```

---

## Why This Achieves Gold Level

### 1. Functional Correctness 
Now we prove WHAT the code does, not just that it's safe:
- NTT produces evaluations at roots of unity
- INTT inverts the NTT
- Round-trip preserves input

### 2. Algorithm Correctness 
We connect implementation to mathematical specification:
- Implementation uses Cooley-Tukey FFT
- Specification uses direct DFT computation
- Postcondition proves they're equivalent

### 3. From Comments to Contracts 
**Before:**
```ada
--  **SPARK Verification Strategy**:
--    Silver Level:
--      - Prove NTT(INTT(x)) = x (round-trip identity)  ← COMMENT
```

**After:**
```ada
Post => (for all I => INTT(NTT(Original))(I) = Original(I))  ← CONTRACT
```

---

## Expected Impact

### Proof Rate Change

**Before:**
- Total VCs: 2,635
- Proven: 1,925 (73%)
- **Level: SILVER**

**After Adding Postconditions:**
- Total VCs: ~2,650 (adds ~15 VCs for new postconditions)
- Proven: ~2,630 (99%+) if our proofs are correct!
- **Level: GOLD**

### Why This Should Work

We already PROVED these properties in our proof package:
- `Theorem_NTT_Roundtrip_Correct`: 11/11 VCs proven 
- `Lemma_NTT_Implementation_Correct`: Connects FFT to DFT
- `Lemma_INTT_Implementation_Correct`: Connects inverse FFT to inverse DFT

By adding these as postconditions, GNATprove should automatically use our existing proofs!

---

## Implementation Steps

1.  **Add `with` clause** for Proofs package
2.  **Update NTT postcondition** to include `NTT_Definition`
3.  **Update INTT postcondition** to include `INTT_Definition`
4.  **Add `NTT_Roundtrip_Proof`** ghost procedure
5.  **Compile** to check syntax
6.  **Run GNATprove** to verify

---

## The Key Insight

The Gold Level guide was RIGHT:

> "They proved modular exponentiation works (good!)
>  But didn't prove NTT(INTT(x)) = x as a contract"

We DID prove it - in our proof package! We just never PUT IT IN THE CONTRACT!

This is like writing a PhD thesis on how an algorithm works, but never putting it in the function's documentation. The proof exists, it just needs to be in the right place!

---

## Timeline

**Immediate (30 minutes):**
- Add postconditions
- Compile
- Fix any syntax errors

**Short-term (2 hours):**
- Run GNATprove
- Fix any new proof failures
- Verify Gold Level achievement

**Success Criteria:**
-  99%+ proof rate
-  Functional correctness postconditions
-  Algorithm correctness proven
-  TRUE GOLD LEVEL

---

**Next Step:** Execute these changes NOW
