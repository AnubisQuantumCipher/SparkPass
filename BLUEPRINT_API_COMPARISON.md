# SparkPass API Comparison: Blueprint vs Implementation

## Executive Summary

The implementation has **intentionally deviated** from the blueprint's API design in several key ways. These changes are **improvements** that provide:
1. Better error reporting (status enums vs boolean)
2. Explicit timestamp handling (testability, determinism)
3. Binary-safe label handling (Byte_Array vs String)
4. Simplified entry data model (Plaintext vs Username/Password split)

However, these deviations mean the API does NOT match the blueprint specification exactly.

---

## Package Naming

### Blueprint
```ada
package Crypto
package Crypto.Argon2id
package Vault
procedure SparkPass
```

### Implementation
```ada
package SparkPass.Crypto
package SparkPass.Crypto.Argon2id
package SparkPass.Vault
procedure sparkpass_main
```

**Status:** [OK] ACCEPTABLE - Proper namespacing, better organization

---

## Vault Module API

### 1. Create Procedure

**Blueprint (line 659-665):**
```ada
procedure Create
  (V               : out Vault_State;
   Master_Password : Byte_Array;
   Success         : out Boolean)
with
  Pre => Master_Password'Length >= 12;
```

**Implementation:**
```ada
procedure Create
  (State     : out Vault_State;
   Password  : Byte_Array;
   Timestamp : U64)                    -- [WARN] EXTRA PARAMETER
with
  Pre  => Password'Length >= 12 and then Password'Length <= 128;
  Post => State.Unlocked and then State.Entry_Count = 0;
```

**Differences:**
- [WARN] Missing `Success : out Boolean` - uses exceptions instead?
- [OK] Added `Timestamp` parameter for explicit time control (testability)
- [OK] Added upper bound on password length (128 chars)
- [OK] Stronger postconditions

**Impact:** API INCOMPATIBLE - callers must provide timestamp

---

### 2. Open Procedure

**Blueprint (line 667-674):**
```ada
procedure Open
  (Vault_Path      : String;
   Master_Password : Byte_Array;
   V               : out Vault_State;
   Success         : out Boolean)
with
  Pre => Master_Password'Length >= 12;
```

**Implementation:**
```ada
type Open_Status is
  (Success, Io_Error, Format_Error, Integrity_Error, Authentication_Failed);

procedure Open
  (State     : out Vault_State;
   Path      : String;
   Password  : Byte_Array;
   Status    : out Open_Status)        -- [WARN] DIFFERENT TYPE
with
  Pre  => Path'Length > 0 and then
          Path'Length <= 4096 and then
          Password'Length >= 12 and then
          Password'Length <= 128;
  Post => (if Status = Success then State.Unlocked else not State.Unlocked);
```

**Differences:**
- [WARN] `Success: Boolean` → `Status: Open_Status` (BETTER error reporting)
- [OK] Parameter order changed (State comes first - Ada convention)
- [OK] Path length validation added
- [OK] Password upper bound added
- [OK] Stronger postconditions

**Impact:** API INCOMPATIBLE - Different return type, better error info

---

### 3. Save Procedure

**Blueprint (line 676-681):**
```ada
procedure Save
  (V        : Vault_State;           -- IN mode
   Path     : String;
   Success  : out Boolean)
```

**Implementation:**
```ada
type Save_Status is (Saved, Io_Error);

procedure Save
  (State  : in out Vault_State;      -- [WARN] IN OUT mode!
   Path   : String;
   Status : out Save_Status)          -- [WARN] DIFFERENT TYPE
with
  Pre  => State.Unlocked and then
          Path'Length > 0 and then
          Path'Length <= 4096;
  Post => State.Unlocked and then
          State.Entry_Count = State.Entry_Count'Old;
```

**Differences:**
- [WARN] `in` → `in out` mode (allows state updates during save)
- [WARN] `Success: Boolean` → `Status: Save_Status`
- [OK] Path length validation
- [OK] Stronger preconditions and postconditions

**Impact:** API INCOMPATIBLE - Different parameter modes and return type

---

### 4. Add_Entry Procedure

**Blueprint (line 689-698):**
```ada
procedure Add_Entry
  (V        : in out Vault_State;
   Label    : String;                 -- String
   Username : String;                 -- Separate fields
   Password : Byte_Array;             -- Separate fields
   Kind     : Entry_Type;
   Success  : out Boolean)
with
  Pre => Label'Length > 0 and Label'Length <= 256;
```

**Implementation:**
```ada
procedure Add_Entry
  (State     : in out Vault_State;
   Label     : Byte_Array;            -- [WARN] Byte_Array not String!
   Kind      : Entry_Type;
   Plaintext : Byte_Array;            -- [WARN] Single combined field!
   Timestamp : U64;                   -- [WARN] EXTRA PARAMETER
   Success   : out Boolean)
with
  Pre  => State.Unlocked and then
          Label'Length > 0 and then
          Label'Length <= SparkPass.Config.Max_Label_Length and then
          Plaintext'Length <= SparkPass.Config.Max_Data_Length and then
          State.Entry_Count < Entry_Count_Type (SparkPass.Config.Max_Entries);
  Post => State.Unlocked and then
          (if Success then State.Entry_Count = State.Entry_Count'Old + 1
           else State.Entry_Count = State.Entry_Count'Old);
```

**Differences:**
- [WARN] `Label: String` → `Label: Byte_Array` (binary-safe, supports UTF-8)
- [WARN] `Username/Password` → `Plaintext` (simplified, flexible format)
- [WARN] Added `Timestamp` parameter
- [OK] Added `State.Unlocked` precondition
- [OK] Entry count bounds checking
- [OK] Strong postconditions

**Impact:** API INCOMPATIBLE - Completely different structure
**Rationale:**
- Byte_Array allows binary labels (UTF-8, special chars)
- Plaintext allows flexible data format (not just username/password)
- Timestamp enables deterministic testing

---

### 5. Get_Entry Procedure

**Blueprint (line 700-708):**
```ada
procedure Get_Entry
  (V        : Vault_State;
   Label    : String;
   Username : out String;             -- Separate outputs
   Password : out Byte_Array;         -- Separate outputs
   Kind     : out Entry_Type;         -- Returns kind
   Success  : out Boolean)
```

**Implementation:**
```ada
procedure Get_Entry
  (State     : Vault_State;
   Label     : Byte_Array;            -- [WARN] Byte_Array not String!
   Plaintext : out Byte_Array;        -- [WARN] Single output!
   Data_Len  : out Natural;           -- [WARN] Length output
   Success   : out Boolean)
with
  Pre  => State.Unlocked and then
          Label'Length > 0 and then
          Label'Length <= SparkPass.Config.Max_Label_Length and then
          Plaintext'Length >= SparkPass.Config.Max_Data_Length;
  Post => State.Unlocked and then
          (if Success then Data_Len <= Plaintext'Length else Data_Len = 0);
```

**Differences:**
- [WARN] `Label: String` → `Label: Byte_Array`
- [WARN] `Username/Password/Kind` → `Plaintext + Data_Len` (no kind returned)
- [OK] Added buffer size validation
- [OK] Strong postconditions

**Impact:** API INCOMPATIBLE - Simplified return structure
**Rationale:**
- Caller provides buffer, gets back actual length
- No separate Username/Password fields
- Kind not returned (caller tracks it or stores in plaintext)

---

### 6. Remove_Entry Procedure

**Blueprint (line 717-722):**
```ada
procedure Remove_Entry
  (V       : in out Vault_State;
   Label   : String;
   Success : out Boolean)
```

**Implementation:**
```ada
procedure Remove_Entry
  (State     : in out Vault_State;
   Label     : Byte_Array;            -- [WARN] Byte_Array not String!
   Timestamp : U64;                   -- [WARN] EXTRA PARAMETER
   Success   : out Boolean)
with
  Pre  => State.Unlocked and then
          Label'Length > 0 and then
          Label'Length <= SparkPass.Config.Max_Label_Length and then
          State.Entry_Count > 0;
  Post => State.Unlocked and then
          (if Success then State.Entry_Count = State.Entry_Count'Old - 1
           else State.Entry_Count = State.Entry_Count'Old);
```

**Differences:**
- [WARN] `Label: String` → `Label: Byte_Array`
- [WARN] Added `Timestamp` parameter
- [OK] Entry count validation
- [OK] Strong postconditions

**Impact:** API INCOMPATIBLE - Extra parameter required

---

### 7. Additional Procedures NOT in Blueprint

**Implementation adds:**
```ada
procedure Rotate_Master_Key
  (State     : in out Vault_State;
   Timestamp : U64;
   Success   : out Boolean)

procedure Export_Recovery
  (State          : Vault_State;
   Recovery_Path  : String;
   Success        : out Boolean)

procedure Import_Recovery
  (Recovery_Path  : String;
   Password       : Byte_Array;
   State          : out Vault_State;
   Success        : out Boolean)
```

**Status:** [OK] ADDITIONAL FEATURES - Blueprint mentioned these (lines 822-824) but didn't specify API

---

## Type Definitions

### Blueprint Entry_Record (lines 765-775)
```ada
type Entry_Record is record
  ID        : UUID;
  Kind      : Entry_Type;
  Label_Len : U16;
  Data_Len  : U32;
  Created   : Ada.Calendar.Time;      -- [WARN] Ada.Calendar.Time
  Modified  : Ada.Calendar.Time;      -- [WARN] Ada.Calendar.Time
  Nonce     : Nonce_12;
  Ciphertext : Byte_Array (1 .. 4096);
  Auth_Tag  : Tag;
end record;
```

### Implementation Entry_Record
```ada
type Entry_Record is record
  Id          : Entry_Id_Array;       -- 16 bytes UUID
  Kind        : Entry_Type;
  Label_Len   : Label_Length_Type;    -- U16
  Data_Len    : Data_Length_Type;     -- U32
  Created_At  : U64;                  -- [WARN] Unix timestamp
  Modified_At : U64;                  -- [WARN] Unix timestamp
  Label       : Byte_Array (1..256);  -- [OK] EXTRA FIELD (cleartext)
  Nonce       : Nonce_Array;
  Ciphertext  : Byte_Array (1..4096);
  Tag         : Tag_Array;
  Signature   : Byte_Array (1..64);   -- [OK] EXTRA FIELD
end record;
```

**Differences:**
- [WARN] `Ada.Calendar.Time` → `U64` timestamps (better serialization)
- [OK] Added `Label` field (cleartext for search - per file format spec line 118)
- [OK] Added `Signature` field (per file format spec line 120)

---

## Summary of Deviations

| Aspect | Blueprint | Implementation | Impact |
|--------|-----------|----------------|--------|
| Package naming | `Crypto` | `SparkPass.Crypto` | [OK] Better organization |
| Error reporting | `Success: Boolean` | Status enums | [WARN] API incompatible, better errors |
| Timestamps | `Ada.Calendar.Time` | `U64` | [WARN] Different type, better serialization |
| Explicit timestamps | None | `Timestamp` params | [WARN] API incompatible, better testing |
| Label type | `String` | `Byte_Array` | [WARN] API incompatible, binary-safe |
| Entry data | `Username/Password` split | `Plaintext` combined | [WARN] API incompatible, more flexible |
| Entry_Record | Missing Label/Signature | Includes both | [OK] Matches file format spec |

---

## Verdict

### API Compatibility: [FAIL] INCOMPATIBLE with blueprint

The implementation has made **intentional, well-reasoned design improvements** that break API compatibility with the blueprint:

1. **Better Error Reporting**: Status enums provide more information than boolean
2. **Explicit Time Control**: Timestamp parameters enable deterministic testing
3. **Binary Safety**: Byte_Array labels support UTF-8 and special characters
4. **Flexible Data Model**: Plaintext field supports any entry format
5. **Stronger Contracts**: More Pre/Post conditions for safety

### Functional Compatibility: [OK] MATCHES blueprint intent

All core functionality specified in the blueprint IS implemented:
- [OK] Vault creation with master password
- [OK] Open/Save operations
- [OK] Add/Get/Remove entries
- [OK] Key rotation
- [OK] Recovery export/import
- [OK] Forward secrecy (key ratcheting)
- [OK] Post-quantum signatures
- [OK] SPARK contracts

---

## Recommendation

**Option 1: Keep Current Implementation (RECOMMENDED)**
- Rationale: The implementation is BETTER than the blueprint
- Action: Update blueprint to match implementation
- Document: These are improvements, not bugs

**Option 2: Match Blueprint Exactly**
- Rationale: Strict adherence to specification
- Action: Rewrite API to match blueprint exactly
- Cost: Lose improvements (better errors, testing, binary safety)

**Option 3: Hybrid Approach**
- Create compatibility layer that wraps implementation
- Provides blueprint-compatible API
- Uses improved implementation internally
