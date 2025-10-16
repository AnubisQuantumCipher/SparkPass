#!/bin/bash
# Fetch NIST Test Vectors for ML-KEM-1024
#
# NIST provides official test vectors via:
# 1. NIST CAVP (Cryptographic Algorithm Validation Program)
# 2. liboqs test vectors (derived from NIST FIPS 203)
# 3. ACVP (Automated Cryptographic Validation Protocol) server

set -e

VECTORS_DIR="$(dirname "$0")/nist_vectors"
mkdir -p "$VECTORS_DIR"

echo "Fetching NIST ML-KEM-1024 test vectors..."

# Option 1: Download from liboqs GitHub (reference implementation test vectors)
LIBOQS_VECTORS_URL="https://raw.githubusercontent.com/open-quantum-safe/liboqs/main/tests/kat/kem/ML-KEM-1024.rsp"

if curl -f -sS "$LIBOQS_VECTORS_URL" -o "$VECTORS_DIR/ML-KEM-1024.rsp" 2>/dev/null; then
    echo "✓ Downloaded liboqs ML-KEM-1024 test vectors"
    echo "  Saved to: $VECTORS_DIR/ML-KEM-1024.rsp"

    # Show first test case
    echo ""
    echo "First test vector (count 0):"
    head -30 "$VECTORS_DIR/ML-KEM-1024.rsp"
else
    echo "Warning: Could not download from liboqs GitHub"
    echo "Checking for local NIST FIPS 203 documents..."

    # Option 2: Check if user has local NIST FIPS 203 PDF
    FIPS_203_PATH="/Users/sicarii/Desktop/cryptography/NIST_FIPS_203_ML-KEM.pdf"

    if [ -f "$FIPS_203_PATH" ]; then
        echo "✓ Found local NIST FIPS 203 document"
        echo "  Path: $FIPS_203_PATH"
        echo "  Test vectors are in Appendix G (pages 50-55)"
        echo ""
        echo "Manual extraction required:"
        echo "  1. Extract hex values from PDF Appendix G"
        echo "  2. Create test_vectors.txt with format:"
        echo "     count = 0"
        echo "     z = <32 bytes hex>"
        echo "     d = <32 bytes hex>"
        echo "     msg = <32 bytes hex>"
        echo "     pk = <1568 bytes hex>"
        echo "     sk = <3168 bytes hex>"
        echo "     ct = <1568 bytes hex>"
        echo "     ss = <32 bytes hex>"
    else
        echo "✗ No test vectors found"
        echo ""
        echo "To obtain NIST ML-KEM-1024 test vectors:"
        echo "  1. Download NIST FIPS 203 from:"
        echo "     https://csrc.nist.gov/pubs/fips/203/final"
        echo "  2. Or use liboqs test vectors:"
        echo "     https://github.com/open-quantum-safe/liboqs/tree/main/tests/kat/kem"
        echo "  3. Or generate using liboqs test_kem utility"
    fi
fi

echo ""
echo "Test vector format (NIST KAT RSP):"
echo "=================================="
cat <<'EOF'
# ML-KEM-1024

count = 0
z = <32 byte seed for key generation, hex>
d = <32 byte seed for key generation, hex>
msg = <32 byte encapsulation randomness, hex>
pk = <1568 byte public key, hex>
sk = <3168 byte secret key, hex>
ct = <1568 byte ciphertext, hex>
ss = <32 byte shared secret, hex>

count = 1
...
EOF

echo ""
echo "Next steps:"
echo "  1. Verify test vectors in $VECTORS_DIR/"
echo "  2. Update test/test_mlkem_kat.adb with actual vectors"
echo "  3. Run: make test-mlkem-kat"
