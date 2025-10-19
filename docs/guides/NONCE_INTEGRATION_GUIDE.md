# SparkPass Nonce Derivation Integration Guide

**Module**: `SparkPass.Crypto.Nonce`
**Version**: 1.0
**Date**: October 16, 2025

---

## Quick Start

### 1. Import the Module

```ada
with SparkPass.Crypto.Nonce; use SparkPass.Crypto.Nonce;
```

### 2. Basic Usage

```ada
declare
   Nonce : Nonce_Array := Derive_Nonce
     (Counter  => Vault.Header.Nonce_Counter,
      Entry_ID => Entry.Id,
      Domain   => Entry_Data);
begin
   --  Use nonce for AEAD encryption
   SparkPass.Crypto.AES_GCM_SIV.Seal
     (Key        => Master_Key,
      Nonce      => Nonce,
      Plaintext  => Entry_Data,
      AAD        => Entry_AAD,
      Ciphertext => Entry.Ciphertext,
      Tag        => Entry.Tag);
end;
```

---

## Integration Points

### 1. Vault Entry Encryption

**File**: `src/sparkpass/vault/sparkpass-vault-operations.adb`

**Before** (using random nonces):
```ada
procedure Encrypt_Entry
  (Vault   : in out Vault_State;
   Entry   : in out Entry_Record;
   Key     : in Key_Array;
   Success : out Boolean)
is
   Nonce : Nonce_Array;
begin
   --  OLD: Random nonce generation (NOT deterministic)
   SparkPass.Crypto.Random.Fill (Nonce);

   --  Encryption with random nonce
   SparkPass.Crypto.AES_GCM_SIV.Seal (...);
end Encrypt_Entry;
```

**After** (using deterministic nonces):
```ada
with SparkPass.Crypto.Nonce;  --  ADD THIS

procedure Encrypt_Entry
  (Vault   : in out Vault_State;
   Entry   : in out Entry_Record;
   Key     : in Key_Array;
   Success : out Boolean)
is
   --  NEW: Deterministic nonce derivation
   Nonce : constant Nonce_Array :=
     SparkPass.Crypto.Nonce.Derive_Nonce
       (Counter  => Vault.Header.Nonce_Counter,
        Entry_ID => Entry.Id,
        Domain   => SparkPass.Crypto.Nonce.Entry_Data);
begin
   --  IMPORTANT: Bump counter BEFORE encryption (fail-closed)
   SparkPass.Vault.Header.Bump (Vault.Header, Get_Current_Timestamp);

   --  Encryption with deterministic nonce
   SparkPass.Crypto.AES_GCM_SIV.Seal
     (Key        => Key,
      Nonce      => Nonce,
      Plaintext  => Entry_Plaintext,
      AAD        => Entry_AAD,
      Ciphertext => Entry.Ciphertext,
      Tag        => Entry.Tag);

   Success := True;
end Encrypt_Entry;
```

**Key Changes**:
1. [OK] Import `SparkPass.Crypto.Nonce`
2. [OK] Replace random nonce with `Derive_Nonce` call
3. [OK] Pass `Vault.Header.Nonce_Counter` (monotonic counter)
4. [OK] Pass `Entry.Id` (unique entry identifier)
5. [OK] Pass `Entry_Data` domain (context separator)
6. [OK] Bump counter BEFORE encryption (ensures uniqueness even on failure)

---

### 2. Vault Entry Decryption

**File**: `src/sparkpass/vault/sparkpass-vault-operations.adb`

**Implementation**:
```ada
procedure Decrypt_Entry
  (Vault   : in Vault_State;
   Entry   : in Entry_Record;
   Key     : in Key_Array;
   Output  : out Byte_Array;
   Success : out Boolean)
is
   --  Deterministically regenerate nonce for decryption
   Nonce : constant Nonce_Array :=
     SparkPass.Crypto.Nonce.Derive_Nonce
       (Counter  => Vault.Header.Nonce_Counter,
        Entry_ID => Entry.Id,
        Domain   => SparkPass.Crypto.Nonce.Entry_Data);
begin
   --  Decryption with regenerated nonce
   SparkPass.Crypto.AES_GCM_SIV.Open
     (Key        => Key,
      Nonce      => Nonce,
      Ciphertext => Entry.Ciphertext,
      AAD        => Entry_AAD,
      Tag        => Entry.Tag,
      Plaintext  => Output,
      Success    => Success);
end Decrypt_Entry;
```

**Key Points**:
1. [OK] Same `Derive_Nonce` call as encryption
2. [OK] Counter value from vault header (persisted on disk)
3. [OK] Entry_ID from entry record (persisted on disk)
4. [OK] Same domain separator (Entry_Data)
5. [OK] Deterministic: encryption and decryption use identical nonce

---

### 3. Header Encryption (Wrapped Keys)

**File**: `src/sparkpass/vault/sparkpass-vault-header.adb`

**Master Key Wrapping**:
```ada
procedure Wrap_Master_Key
  (Header       : in out SparkPass.Types.Header;
   Master_Key   : in Key_Array;
   Wrap_Key     : in Key_Array)
is
   Nonce : constant Nonce_Array :=
     SparkPass.Crypto.Nonce.Derive_Nonce
       (Counter  => Header.Nonce_Counter,
        Entry_ID => Generate_Synthetic_ID ("master_key_wrap"),
        Domain   => SparkPass.Crypto.Nonce.Header_Seal);
begin
   SparkPass.Crypto.AES_GCM_SIV.Seal
     (Key        => Wrap_Key,
      Nonce      => Nonce,
      Plaintext  => Master_Key,
      AAD        => Header_AAD,
      Ciphertext => Header.Wrapped_Master_Key,
      Tag        => Header.Wrapped_Master_Tag);
end Wrap_Master_Key;
```

**Synthetic Entry_ID Generation**:
```ada
--  Generate deterministic Entry_ID for header operations
--  (since headers don't have UUIDs like entries do)
function Generate_Synthetic_ID (Context : String) return Entry_Id_Array is
   Result : Entry_Id_Array;
   IKM    : Byte_Array (1 .. Context'Length + 8);
begin
   --  Construct IKM: Counter || Context
   for I in 1 .. 8 loop
      IKM (I) := 0;  --  Padding (or use counter)
   end loop;
   for I in Context'Range loop
      IKM (8 + I - Context'First + 1) := Character'Pos (Context (I));
   end loop;

   --  Derive synthetic ID with HKDF
   declare
      Derived : constant Byte_Array :=
        SparkPass.Crypto.HKDF.Derive (IKM, Salt => "SyntheticID", Info => Empty, Length => 16);
   begin
      for I in Result'Range loop
         Result (I) := Derived (I);
      end loop;
   end;

   return Result;
end Generate_Synthetic_ID;
```

---

### 4. Counter Management

**File**: `src/sparkpass/vault/sparkpass-vault-header.adb`

**Counter Initialization**:
```ada
procedure Initialize
  (State     : out SparkPass.Types.Header;
   Password  : Byte_Array;
   Timestamp : U64;
   ...)
is
begin
   State.Nonce_Counter := 1;  --  Start at 1, never 0
   State.Created_At := Timestamp;
   State.Modified_At := Timestamp;
   --  ... rest of initialization
end Initialize;
```

**Counter Increment**:
```ada
procedure Bump
  (State     : in out SparkPass.Types.Header;
   Timestamp : U64)
with
  Post => State.Nonce_Counter = State.Nonce_Counter'Old + 1
is
begin
   State.Nonce_Counter := State.Nonce_Counter + 1;
   State.Modified_At := Timestamp;

   --  Update signature to cover new counter value
   Update_Signature (State);
end Bump;
```

**Counter Overflow Detection**:
```ada
--  Add to Bump procedure:
if State.Nonce_Counter = U64'Last then
   raise Program_Error with "Nonce counter overflow - vault exhausted";
end if;
```

---

### 5. Domain Separator Selection

| Use Case | Domain Separator | Rationale |
|----------|------------------|-----------|
| Entry payload encryption (password, TOTP, note) | `Entry_Data` | Primary encryption context |
| Entry label encryption (future) | `Entry_Metadata` | Separate context for metadata |
| Header wrapping (master key, chain key, signing key) | `Header_Seal` | Distinct from entry operations |
| Audit log encryption (future) | `Log_Record` | Future extension for audit logs |

**Usage Example**:
```ada
--  Entry data encryption
Nonce := Derive_Nonce (Counter, Entry.Id, Entry_Data);

--  Entry label encryption (future)
Nonce := Derive_Nonce (Counter, Entry.Id, Entry_Metadata);

--  Header wrapping
Nonce := Derive_Nonce (Counter, Synthetic_ID, Header_Seal);

--  Audit log (future)
Nonce := Derive_Nonce (Counter, Log_Entry.Id, Log_Record);
```

---

## Testing

### Build and Run Unit Tests

```bash
cd /Users/sicarii/SparkPass/test

# Build test executable
gprbuild -P test_nonce.gpr

# Run tests
./test_nonce_derivation
```

**Expected Output**:
```
============================================================================
SPARKPASS NONCE DERIVATION TEST SUITE
============================================================================

Running Known Answer Tests...
[PASS] KAT: Basic derivation

Running Determinism Tests...
[PASS] Determinism: Multiple invocations produce same nonce

Running Injectivity Tests...
[PASS] Injectivity: Counter=1 vs Counter=2
[PASS] Injectivity: Entry_ID1 vs Entry_ID2

Running Domain Separation Tests...
[PASS] Domain separation: Entry_Data vs Entry_Metadata

[SUCCESS] All tests passed!
============================================================================
```

### Integration Test Checklist

After integrating nonce derivation into vault operations:

- [ ] Create new vault → verify counter starts at 1
- [ ] Add entry → verify counter bumps to 2
- [ ] Encrypt entry → verify nonce derived correctly
- [ ] Decrypt entry → verify nonce matches encryption
- [ ] Modify entry → verify counter bumps again
- [ ] Add 10 entries → verify all nonces distinct
- [ ] Close and reopen vault → verify counter persisted
- [ ] Decrypt all entries → verify all nonces regenerated correctly

---

## SPARK Verification

### Verify Nonce Module

```bash
cd /Users/sicarii/SparkPass

# Flow analysis (data flow, initialization)
gnatprove -P sparkpass.gpr -u sparkpass-crypto-nonce.adb --mode=flow --level=4

# Proof mode (contracts, range checks)
gnatprove -P sparkpass.gpr -u sparkpass-crypto-nonce.adb --mode=prove --level=4

# Full verification
gnatprove -P sparkpass.gpr -u sparkpass-crypto-nonce.adb --mode=all --level=4
```

### Verify Integration Sites

After integrating into vault operations:

```bash
# Verify vault operations module
gnatprove -P sparkpass.gpr -u sparkpass-vault-operations.adb --mode=all --level=4

# Verify vault header module
gnatprove -P sparkpass.gpr -u sparkpass-vault-header.adb --mode=all --level=4

# Verify entire project
gnatprove -P sparkpass.gpr --mode=all --level=2
```

---

## Migration Checklist

### Phase 1: Module Implementation [OK]
- [x] Create `sparkpass-crypto-nonce.ads` specification
- [x] Create `sparkpass-crypto-nonce.adb` implementation
- [x] Create `test/test_nonce_derivation.adb` unit tests
- [x] Create `NONCE_DERIVATION_ANALYSIS.md` security documentation

### Phase 2: Integration (TODO)
- [ ] Update `sparkpass-vault-operations.adb` to use `Derive_Nonce`
- [ ] Update `sparkpass-vault-header.adb` for header wrapping
- [ ] Add synthetic Entry_ID generation for header operations
- [ ] Add counter overflow detection and handling
- [ ] Update counter bump to occur BEFORE encryption

### Phase 3: Testing (TODO)
- [ ] Run unit tests (`test_nonce_derivation`)
- [ ] Run integration tests (create/modify/decrypt entries)
- [ ] Run stress tests (2000 entries, verify all nonces distinct)
- [ ] Verify SPARK contracts pass for all modules

### Phase 4: Documentation (TODO)
- [ ] Update `README.md` with nonce derivation security properties
- [ ] Update API documentation with nonce derivation examples
- [ ] Add security advisory about nonce uniqueness guarantees
- [ ] Update threat model documentation

---

## Common Pitfalls (AVOID THESE!)

### [FAIL] DON'T: Use Random Nonces
```ada
--  BAD: Non-deterministic nonce generation
SparkPass.Crypto.Random.Fill (Nonce);
SparkPass.Crypto.AES_GCM_SIV.Seal (Key, Nonce, ...);
```

**Why Bad**: Cannot regenerate nonce for decryption without storing it.

### [OK] DO: Use Deterministic Nonce Derivation
```ada
--  GOOD: Deterministic nonce derivation
Nonce := Derive_Nonce (Counter, Entry_ID, Domain);
SparkPass.Crypto.AES_GCM_SIV.Seal (Key, Nonce, ...);
```

---

### [FAIL] DON'T: Bump Counter After Encryption
```ada
--  BAD: Counter bumped after encryption (nonce reuse on failure)
SparkPass.Crypto.AES_GCM_SIV.Seal (Key, Nonce, ...);
SparkPass.Vault.Header.Bump (Vault.Header, Timestamp);  --  Too late!
```

**Why Bad**: If encryption fails, counter doesn't bump, next attempt reuses nonce.

### [OK] DO: Bump Counter Before Encryption
```ada
--  GOOD: Counter bumped before encryption (fail-closed)
SparkPass.Vault.Header.Bump (Vault.Header, Timestamp);
SparkPass.Crypto.AES_GCM_SIV.Seal (Key, Nonce, ...);
```

---

### [FAIL] DON'T: Reuse Entry_ID Across Entries
```ada
--  BAD: Same Entry_ID for multiple entries
Entry1.Id := Some_Fixed_UUID;
Entry2.Id := Some_Fixed_UUID;  --  Collision!
```

**Why Bad**: Same Entry_ID + same counter = same nonce = nonce reuse!

### [OK] DO: Generate Unique Entry_ID for Each Entry
```ada
--  GOOD: Unique Entry_ID for each entry
SparkPass.Crypto.Random.Fill (Entry1.Id);
SparkPass.Crypto.Random.Fill (Entry2.Id);  --  Distinct with overwhelming probability
```

---

### [FAIL] DON'T: Use Wrong Domain Separator
```ada
--  BAD: Entry_Data domain for header operation
Nonce := Derive_Nonce (Counter, Header_ID, Entry_Data);  --  Wrong domain!
SparkPass.Crypto.AES_GCM_SIV.Seal (Wrap_Key, Nonce, Master_Key, ...);
```

**Why Bad**: Domain confusion could lead to nonce reuse across contexts.

### [OK] DO: Use Correct Domain Separator
```ada
--  GOOD: Header_Seal domain for header operation
Nonce := Derive_Nonce (Counter, Header_ID, Header_Seal);
SparkPass.Crypto.AES_GCM_SIV.Seal (Wrap_Key, Nonce, Master_Key, ...);
```

---

### [FAIL] DON'T: Skip Counter Persistence
```ada
--  BAD: Counter not saved to vault file
Vault.Header.Nonce_Counter := Vault.Header.Nonce_Counter + 1;
--  ... encryption happens ...
--  ... but vault not written to disk!
```

**Why Bad**: On next vault load, counter resets, nonces get reused.

### [OK] DO: Persist Counter to Disk
```ada
--  GOOD: Counter saved to vault file
Vault.Header.Nonce_Counter := Vault.Header.Nonce_Counter + 1;
SparkPass.Vault.Save (Vault);  --  Write to disk!
```

---

## Performance Considerations

**Nonce Derivation Overhead**:
- HKDF-SHA-384: ~1-5 microseconds (single hash operation)
- Entry encryption: ~100-500 microseconds (AES-GCM-SIV)
- **Overhead**: < 1% (negligible compared to encryption)

**Memory Usage**:
- Stack allocation only: ~100 bytes (IKM buffer, intermediate results)
- No heap allocation
- No persistent state

**Optimization Opportunities**:
- [FAIL] **DON'T cache nonces** (defeats security purpose)
- [FAIL] **DON'T parallelize nonce derivation** (counter must be sequential)
- [OK] **DO parallelize entry encryption** (after nonces derived)

---

## Security Considerations

### Counter Monotonicity

**CRITICAL**: Counter must NEVER decrease or reset. Enforce with:
1. Signature verification (ML-DSA covers counter in vault header)
2. Timestamp validation (decreasing timestamps trigger warning)
3. Backup integrity (prevent replay of old vault states)

### Entry_ID Uniqueness

**CRITICAL**: Entry_IDs must be globally unique. Enforce with:
1. Cryptographic RNG (`crypto_random_bytes`)
2. Duplicate detection on entry insertion (optional)
3. 128-bit entropy space (2^128 possible UUIDs)

### Domain Separation

**CRITICAL**: Use correct domain for each operation. Enforce with:
1. Type system (enum, not string)
2. Code review (verify domain at call sites)
3. Static analysis (grep for `Derive_Nonce` calls)

---

## Troubleshooting

### Problem: Nonce derivation tests fail

**Symptoms**: `test_nonce_derivation` reports failures

**Diagnosis**:
1. Check HKDF implementation (compare with RFC 5869 test vectors)
2. Check byte ordering (big-endian for counter)
3. Check domain separator strings (typos in byte arrays)

**Solution**: Review `sparkpass-crypto-nonce.adb` implementation

---

### Problem: Decryption fails after encryption

**Symptoms**: `AES_GCM_SIV.Open` returns `Success := False`

**Diagnosis**:
1. Check nonce matches between encryption and decryption
2. Check counter persisted correctly to vault file
3. Check Entry_ID matches (not overwritten)
4. Check domain separator matches

**Solution**: Log nonces at encryption and decryption, compare values

---

### Problem: Counter overflow

**Symptoms**: `Constraint_Error` raised during `Bump`

**Diagnosis**:
1. Counter reached 2^64-1 (extremely unlikely)
2. Counter corrupted (file corruption)

**Solution**: Implement counter rollover policy (refuse further operations)

---

## Support

For questions or issues:
1. Read `NONCE_DERIVATION_ANALYSIS.md` for detailed security analysis
2. Review unit tests in `test/test_nonce_derivation.adb` for examples
3. Check SPARK verification output for contract violations
4. Contact: SparkPass Security Engineering Team

---

**Last Updated**: October 16, 2025
**Version**: 1.0
