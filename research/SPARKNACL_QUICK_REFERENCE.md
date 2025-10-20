# SPARKNaCl Quick Reference for SparkPass Phase 0

**Date**: 2025-10-17
**SPARKNaCl Version**: 4.0.1
**Location**: `/Users/sicarii/.local/share/alire/releases/sparknacl_4.0.1_8e3cc2e6/`

---

## 🎯 What We're Using

### **Secret Box (Authenticated Encryption)**
**File**: `sparknacl-secretbox.ads`
**Function**: ChaCha20-Poly1305 AEAD (Authenticated Encryption with Associated Data)
**Use Case**: Replace AES-GCM-SIV in SparkPass

**Key Details**:
- **Algorithm**: XSalsa20 stream cipher + Poly1305 MAC
- **Nonce**: 24 bytes (vs. 12 bytes in GCM-SIV)
- **Key**: 32 bytes
- **Tag**: 16 bytes (Poly1305 MAC)
- **API**: `Create` (encrypt), `Open` (decrypt)

---

## 📚 Available Modules

| **Module** | **Purpose** | **Phase** |
|-----------|-------------|-----------|
| `sparknacl-secretbox.ads` | ChaCha20-Poly1305 AEAD | **Phase 0**  |
| `sparknacl-hashing-sha512.ads` | SHA-512 hash | Phase 1 (Blake2b template) |
| `sparknacl-hashing-sha256.ads` | SHA-256 hash | Reference |
| `sparknacl-hkdf.ads` | HKDF key derivation | Already using similar |
| `sparknacl-cryptobox.ads` | Public-key crypto (Curve25519) | Future |
| `sparknacl-sign.ads` | Ed25519 signatures | Future |
| `sparknacl-stream.ads` | ChaCha20 stream cipher | Reference |
| `sparknacl-mac.ads` | Poly1305 MAC | Reference |

---

## 🔑 Secret Box API

### Type Definitions

```ada
subtype Nonce is Bytes_24;  -- 24-byte nonce
subtype Key is Bytes_32;    -- 32-byte key
```

### Create (Encrypt)

```ada
procedure Create
  (C      :    out Bytes;     -- Ciphertext output (plaintext length + 16)
   M      : in     Bytes;     -- Plaintext input
   N      : in     Nonce;     -- 24-byte nonce
   K      : in     Key;       -- 32-byte key
   Status :    out Boolean);  -- Success flag
```

**Notes**:
- Output `C` must be 16 bytes longer than `M` (for Poly1305 tag)
- Tag is prepended to ciphertext (first 16 bytes)

### Open (Decrypt)

```ada
procedure Open
  (M      :    out Bytes;     -- Plaintext output
   C      : in     Bytes;     -- Ciphertext input (includes 16-byte tag)
   N      : in     Nonce;     -- 24-byte nonce (must match encryption)
   K      : in     Key;       -- 32-byte key
   Status :    out Boolean);  -- True if authentic, False if tampered
```

**Notes**:
- Returns `False` if authentication fails
- `M` length must be `C'Length - 16`

---

## 🔄 Mapping to SparkPass API

### Current SparkPass (AES-GCM-SIV)

```ada
procedure Seal
  (Key        : in  Key_Array;       -- 32 bytes
   Nonce      : in  Nonce_Array;     -- 12 bytes
   Plaintext  : in  Byte_Array;
   AAD        : in  Byte_Array;      -- Additional Authenticated Data
   Ciphertext : out Byte_Array;
   Tag        : out Tag_Array);      -- 16 bytes
```

### SPARKNaCl Secret Box

```ada
procedure Create
  (C      :    out Bytes;            -- Ciphertext + Tag (combined)
   M      : in     Bytes;            -- Plaintext
   N      : in     Nonce;            -- 24 bytes
   K      : in     Key;              -- 32 bytes
   Status :    out Boolean);
```

### Key Differences

| **Aspect** | **AES-GCM-SIV** | **SPARKNaCl Secret Box** |
|------------|-----------------|--------------------------|
| **Nonce Size** | 12 bytes | **24 bytes** |
| **Tag Location** | Separate output | **Prepended to ciphertext** |
| **AAD Support** | Yes | **No** (not in Secret Box) |
| **Output Format** | Ciphertext + Tag separate | **Combined** (tag + ciphertext) |

---

## 🛠️ Adapter Strategy

### Challenge 1: Nonce Size (12 → 24 bytes)

**Solution**: Zero-padding
```ada
-- Extend 12-byte SparkPass nonce to 24-byte SPARKNaCl nonce
Nonce_24 : SPARKNaCl.Secretbox.Nonce;
Nonce_24(0..11) := Nonce(Nonce'Range);  -- Copy 12 bytes
Nonce_24(12..23) := (others => 0);      -- Zero-pad remaining 12 bytes
```

### Challenge 2: Tag Separation

**Solution**: Split/combine functions
```ada
-- Encrypt: SPARKNaCl outputs Tag+Ciphertext, split into separate outputs
C_with_Tag : Bytes(0 .. Plaintext'Length + 15);  -- Combined
Create(C_with_Tag, Plaintext, Nonce_24, Key, Status);
Tag := C_with_Tag(0..15);                        -- First 16 bytes
Ciphertext := C_with_Tag(16..C_with_Tag'Last);  -- Remaining bytes

-- Decrypt: Combine Tag+Ciphertext before passing to SPARKNaCl
C_with_Tag(0..15) := Tag;
C_with_Tag(16..C_with_Tag'Last) := Ciphertext;
Open(Plaintext, C_with_Tag, Nonce_24, Key, Status);
```

### Challenge 3: AAD Support

**Problem**: SPARKNaCl Secret Box doesn't support Additional Authenticated Data

**Solutions**:
1. **Phase 0**: Ignore AAD (SparkPass currently uses empty AAD anyway)
2. **Future**: Use `sparknacl-cryptobox.ads` which has AAD support
3. **Alternative**: Manually include AAD in encryption (custom extension)

**For Now**: Verify SparkPass doesn't use AAD, proceed without it

---

##  Phase 0 Implementation Checklist

### Step 1: Create Adapter Package Spec

```bash
cat > src/sparkpass/crypto/sparkpass-crypto-chacha20poly1305.ads <<'EOF'
pragma SPARK_Mode (On);
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.ChaCha20Poly1305 is
   procedure Seal (
      Key        : in  Key_Array;
      Nonce      : in  Nonce_Array;
      Plaintext  : in  Byte_Array;
      AAD        : in  Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out Tag_Array
   ) with
      Global => null,
      Pre => Ciphertext'Length = Plaintext'Length and
             Plaintext'Length <= 4096,
      Post => Ciphertext'Length = Plaintext'Length;

   procedure Open (
      Key        : in  Key_Array;
      Nonce      : in  Nonce_Array;
      Ciphertext : in  Byte_Array;
      AAD        : in  Byte_Array;
      Tag        : in  Tag_Array;
      Plaintext  : out Byte_Array;
      Success    : out Boolean
   ) with
      Global => null,
      Pre => Plaintext'Length = Ciphertext'Length,
      Post => (if not Success then
                 (for all I in Plaintext'Range => Plaintext(I) = 0));
end SparkPass.Crypto.ChaCha20Poly1305;
EOF
```

### Step 2: Create Adapter Package Body

```bash
cat > src/sparkpass/crypto/sparkpass-crypto-chacha20poly1305.adb <<'EOF'
pragma SPARK_Mode (On);
with SPARKNaCl.Secretbox;

package body SparkPass.Crypto.ChaCha20Poly1305 is
   procedure Seal (...) is
      -- TODO: Implement nonce conversion, call SPARKNaCl.Secretbox.Create
   end Seal;

   procedure Open (...) is
      -- TODO: Implement tag combination, call SPARKNaCl.Secretbox.Open
   end Open;
end SparkPass.Crypto.ChaCha20Poly1305;
EOF
```

**Full implementation**: See `TECHNICAL_IMPLEMENTATION_GUIDE.md`

### Step 3: Update sparkpass.gpr

Add SPARKNaCl to source directories (Alire handles this automatically):
```
# No manual changes needed - Alire manages dependencies
```

### Step 4: Build and Test

```bash
# Build
gprbuild -P sparkpass.gpr

# Verify
gnatprove -P sparkpass.gpr -u sparkpass-crypto-chacha20poly1305 --level=2

# Expected: 20-25 VCs, 100% proven
```

---

## 📖 Further Reading

### SPARKNaCl Documentation

- **Main package**: `sparknacl.ads` - Type definitions
- **Secret Box**: `sparknacl-secretbox.ads` - Authenticated encryption
- **Hashing**: `sparknacl-hashing-sha512.ads` - Template for Blake2b (Phase 1)

### Verification Status

SPARKNaCl is **100% SPARK-verified** with:
-  Memory safety (no buffer overflows)
-  Type safety (no range violations)
-  Functional correctness (some properties)
-  Constant-time operations (timing-attack resistant)

**References**:
- GitHub: https://github.com/rod-chapman/SPARKNaCl
- Paper: "SPARKNaCl: A Verified, Fast Implementation of NaCl in SPARK"

---

## 🚀 Next Steps

1. **Study** `sparknacl-secretbox.ads` (15 minutes)
2. **Implement** adapter (2-3 hours)
3. **Test** with RFC 8439 vectors (1 hour)
4. **Verify** with GNATprove (30 minutes)
5. **Integrate** into SparkPass (1 hour)

**Total Phase 0 Time**: 1-2 days

**Success Criteria**: 20-25 new VCs, 100% proven 

---

**Created**: 2025-10-17
**Status**: Ready for implementation
