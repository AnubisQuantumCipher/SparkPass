#!/bin/bash
# Verify code signature and notarization of SparkPass binary

set -e

BINARY="${1:-/Users/sicarii/SparkPass/bin/sparkpass_main}"

if [ ! -f "$BINARY" ]; then
    echo "ERROR: Binary not found: $BINARY"
    echo "Usage: $0 [path-to-binary]"
    exit 1
fi

echo "=== SparkPass Signature Verification ==="
echo ""
echo "Binary: $BINARY"
echo ""

# ============================================================================
# CHECK 1: Basic Signature Verification
# ============================================================================

echo "--- Check 1: Basic Signature Verification ---"
echo ""

codesign --verify --verbose=4 "$BINARY" 2>&1 && echo "[PASS] Signature is valid" || echo "[FAIL] Signature verification FAILED"
echo ""

# ============================================================================
# CHECK 2: Display Signature Details
# ============================================================================

echo "--- Check 2: Signature Details ---"
echo ""

codesign --display --verbose=4 "$BINARY" 2>&1
echo ""

# ============================================================================
# CHECK 3: Check Hardened Runtime
# ============================================================================

echo "--- Check 3: Hardened Runtime Status ---"
echo ""

if codesign --display --verbose "$BINARY" 2>&1 | grep -q "runtime"; then
    echo "[PASS] Hardened Runtime is ENABLED"
else
    echo "[FAIL] Hardened Runtime is NOT enabled"
fi
echo ""

# ============================================================================
# CHECK 4: Display Entitlements
# ============================================================================

echo "--- Check 4: Entitlements ---"
echo ""

codesign --display --entitlements - "$BINARY" 2>&1 || echo "No entitlements or not signed"
echo ""

# ============================================================================
# CHECK 5: Gatekeeper Assessment
# ============================================================================

echo "--- Check 5: Gatekeeper Assessment ---"
echo ""

spctl --assess --type execute --verbose "$BINARY" 2>&1 && echo "[PASS] Gatekeeper ACCEPTS this binary" || echo "[FAIL] Gatekeeper REJECTS this binary"
echo ""

# ============================================================================
# CHECK 6: Check Notarization (online)
# ============================================================================

echo "--- Check 6: Notarization Status (Online Check) ---"
echo ""

if codesign -dv --verbose=4 "$BINARY" 2>&1 | grep -q "Notarization"; then
    echo "[PASS] Binary appears to be notarized"
else
    echo "[WARN] Cannot determine notarization status from signature"
    echo "  (This is normal for binaries that aren't stapled)"
fi

echo ""
echo "Online notarization check:"
spctl --assess --type execute --verbose=4 "$BINARY" 2>&1 | grep -i notarization || echo "No notarization info available"

echo ""

# ============================================================================
# CHECK 7: Check Linked Libraries
# ============================================================================

echo "--- Check 7: Linked Libraries and Frameworks ---"
echo ""

otool -L "$BINARY"
echo ""

# ============================================================================
# CHECK 8: Architecture
# ============================================================================

echo "--- Check 8: Binary Architecture ---"
echo ""

file "$BINARY"
lipo -info "$BINARY" 2>/dev/null || true
echo ""

# ============================================================================
# SUMMARY
# ============================================================================

echo "========================================"
echo "Verification Complete"
echo "========================================"
echo ""
echo "If all checks passed, the binary is properly signed and ready for distribution."
echo ""
echo "To test on another Mac:"
echo "  1. Copy the binary: scp $BINARY otheruser@othermac:/tmp/sparkpass"
echo "  2. Set quarantine bit: xattr -w com.apple.quarantine '0081;' /tmp/sparkpass"
echo "  3. Try to run: /tmp/sparkpass --help"
echo "  4. macOS should accept it without warnings"
