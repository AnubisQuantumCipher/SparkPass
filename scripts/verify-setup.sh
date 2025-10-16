#!/bin/bash
# Verify that code signing and notarization setup is complete
# Run this to check if you're ready to sign and notarize SparkPass

set -e

PROJECT_ROOT="/Users/sicarii/SparkPass"
cd "$PROJECT_ROOT"

PASSED=0
FAILED=0
WARNINGS=0

echo "=== SparkPass Code Signing Setup Verification ==="
echo ""
echo "This script checks if your environment is ready for code signing and notarization."
echo ""

# ============================================================================
# CHECK 1: Xcode Command Line Tools
# ============================================================================

echo "--- Check 1: Xcode Command Line Tools ---"
echo ""

if xcode-select -p &>/dev/null; then
    XCODE_PATH=$(xcode-select -p)
    echo "✓ Xcode Command Line Tools installed at: $XCODE_PATH"
    ((PASSED++))
else
    echo "✗ Xcode Command Line Tools NOT installed"
    echo "  Install with: xcode-select --install"
    ((FAILED++))
fi
echo ""

# ============================================================================
# CHECK 2: Code Signing Tools
# ============================================================================

echo "--- Check 2: Code Signing Tools ---"
echo ""

TOOLS_OK=true

if command -v codesign &>/dev/null; then
    echo "✓ codesign found: $(which codesign)"
else
    echo "✗ codesign NOT found"
    TOOLS_OK=false
fi

if command -v productsign &>/dev/null; then
    echo "✓ productsign found: $(which productsign)"
else
    echo "✗ productsign NOT found"
    TOOLS_OK=false
fi

if xcrun --find notarytool &>/dev/null; then
    echo "✓ notarytool found: $(xcrun --find notarytool)"
else
    echo "✗ notarytool NOT found"
    TOOLS_OK=false
fi

if xcrun --find stapler &>/dev/null; then
    echo "✓ stapler found: $(xcrun --find stapler)"
else
    echo "✗ stapler NOT found"
    TOOLS_OK=false
fi

if [ "$TOOLS_OK" = true ]; then
    ((PASSED++))
else
    echo "  Install Xcode Command Line Tools: xcode-select --install"
    ((FAILED++))
fi
echo ""

# ============================================================================
# CHECK 3: Developer ID Certificate
# ============================================================================

echo "--- Check 3: Developer ID Application Certificate ---"
echo ""

CERT_COUNT=$(security find-identity -v -p codesigning | grep -c "Developer ID Application" || true)

if [ "$CERT_COUNT" -eq 0 ]; then
    echo "✗ No Developer ID Application certificate found"
    echo ""
    echo "  To obtain a certificate:"
    echo "  1. Run: ./scripts/generate-csr.sh"
    echo "  2. Upload CSR to https://developer.apple.com/account/resources/certificates/add"
    echo "  3. Select 'Developer ID Application'"
    echo "  4. Download certificate and run: ./scripts/install-certificate.sh cert.cer"
    echo ""
    ((FAILED++))
elif [ "$CERT_COUNT" -eq 1 ]; then
    IDENTITY=$(security find-identity -v -p codesigning | grep "Developer ID Application" | awk -F'"' '{print $2}')
    echo "✓ Developer ID Application certificate found:"
    echo "  $IDENTITY"
    echo ""
    echo "  Set environment variable:"
    echo "  export SPARKPASS_SIGNING_IDENTITY=\"$IDENTITY\""
    ((PASSED++))
else
    echo "⚠ Multiple Developer ID Application certificates found:"
    security find-identity -v -p codesigning | grep "Developer ID Application"
    echo ""
    echo "  Choose one and set environment variable:"
    echo "  export SPARKPASS_SIGNING_IDENTITY=\"<identity name>\""
    ((WARNINGS++))
fi
echo ""

# ============================================================================
# CHECK 4: Signing Identity Environment Variable
# ============================================================================

echo "--- Check 4: Signing Identity Configuration ---"
echo ""

if [ -n "$SPARKPASS_SIGNING_IDENTITY" ]; then
    echo "✓ SPARKPASS_SIGNING_IDENTITY is set:"
    echo "  $SPARKPASS_SIGNING_IDENTITY"

    # Verify this identity exists
    if security find-identity -v -p codesigning | grep -q "$SPARKPASS_SIGNING_IDENTITY"; then
        echo "✓ Identity found in keychain"
        ((PASSED++))
    else
        echo "✗ Identity NOT found in keychain"
        echo "  Available identities:"
        security find-identity -v -p codesigning
        ((FAILED++))
    fi
else
    echo "⚠ SPARKPASS_SIGNING_IDENTITY not set"
    echo ""
    echo "  Set it with:"
    if [ "$CERT_COUNT" -eq 1 ]; then
        IDENTITY=$(security find-identity -v -p codesigning | grep "Developer ID Application" | awk -F'"' '{print $2}')
        echo "  export SPARKPASS_SIGNING_IDENTITY=\"$IDENTITY\""
    else
        echo "  export SPARKPASS_SIGNING_IDENTITY=\"Developer ID Application: Your Name (TEAM123)\""
    fi
    echo ""
    echo "  Add to ~/.zshrc or ~/.bash_profile to make permanent"
    ((WARNINGS++))
fi
echo ""

# ============================================================================
# CHECK 5: Entitlements File
# ============================================================================

echo "--- Check 5: Entitlements File ---"
echo ""

if [ -f "signing/sparkpass.entitlements" ]; then
    echo "✓ Entitlements file found: signing/sparkpass.entitlements"

    # Validate plist syntax
    if plutil -lint signing/sparkpass.entitlements &>/dev/null; then
        echo "✓ Entitlements file is valid"
        ((PASSED++))
    else
        echo "✗ Entitlements file has syntax errors"
        plutil -lint signing/sparkpass.entitlements
        ((FAILED++))
    fi
else
    echo "✗ Entitlements file NOT found: signing/sparkpass.entitlements"
    echo "  This file should have been created during setup"
    ((FAILED++))
fi
echo ""

# ============================================================================
# CHECK 6: Notarization Profile
# ============================================================================

echo "--- Check 6: Notarization Credentials ---"
echo ""

if xcrun notarytool history --keychain-profile sparkpass-notarization &>/dev/null; then
    echo "✓ Notarization profile 'sparkpass-notarization' is configured"
    ((PASSED++))
else
    echo "✗ Notarization profile NOT configured"
    echo ""
    echo "  To configure:"
    echo "  1. Generate app-specific password: https://appleid.apple.com/account/manage"
    echo "  2. Run: ./scripts/setup-notarization.sh"
    echo ""
    ((FAILED++))
fi
echo ""

# ============================================================================
# CHECK 7: Build Dependencies
# ============================================================================

echo "--- Check 7: Homebrew Build Dependencies ---"
echo ""

DEPS_OK=true

if brew list liboqs &>/dev/null; then
    echo "✓ liboqs installed: $(brew --prefix liboqs)"
else
    echo "✗ liboqs NOT installed"
    echo "  Install with: brew install liboqs"
    DEPS_OK=false
fi

if brew list openssl@3 &>/dev/null; then
    echo "✓ openssl@3 installed: $(brew --prefix openssl@3)"
else
    echo "✗ openssl@3 NOT installed"
    echo "  Install with: brew install openssl@3"
    DEPS_OK=false
fi

if brew list libsodium &>/dev/null; then
    echo "✓ libsodium installed: $(brew --prefix libsodium)"
else
    echo "✗ libsodium NOT installed"
    echo "  Install with: brew install libsodium"
    DEPS_OK=false
fi

if [ "$DEPS_OK" = true ]; then
    ((PASSED++))
else
    echo ""
    echo "  Install all dependencies:"
    echo "  brew install liboqs openssl@3 libsodium"
    ((FAILED++))
fi
echo ""

# ============================================================================
# CHECK 8: SparkPass Binary
# ============================================================================

echo "--- Check 8: SparkPass Binary ---"
echo ""

if [ -f "bin/sparkpass_main" ]; then
    echo "✓ SparkPass binary exists: bin/sparkpass_main"

    # Check if signed
    if codesign --verify bin/sparkpass_main 2>/dev/null; then
        SIGNING_INFO=$(codesign --display --verbose bin/sparkpass_main 2>&1)
        echo "✓ Binary is signed"

        if echo "$SIGNING_INFO" | grep -q "runtime"; then
            echo "✓ Hardened Runtime enabled"
        else
            echo "⚠ Hardened Runtime NOT enabled"
            echo "  Re-sign with: ./scripts/sign-and-notarize.sh"
            ((WARNINGS++))
        fi
        ((PASSED++))
    else
        echo "⚠ Binary is NOT signed"
        echo "  Sign with: ./scripts/sign-and-notarize.sh"
        ((WARNINGS++))
    fi
else
    echo "⚠ SparkPass binary not found"
    echo "  Build with: ./build.sh"
    ((WARNINGS++))
fi
echo ""

# ============================================================================
# CHECK 9: Scripts Permissions
# ============================================================================

echo "--- Check 9: Script Permissions ---"
echo ""

SCRIPTS=(
    "scripts/generate-csr.sh"
    "scripts/install-certificate.sh"
    "scripts/setup-notarization.sh"
    "scripts/sign-and-notarize.sh"
    "scripts/build-and-sign.sh"
    "scripts/verify-signature.sh"
    "scripts/create-distribution.sh"
    "build.sh"
)

SCRIPTS_OK=true
for script in "${SCRIPTS[@]}"; do
    if [ -x "$script" ]; then
        echo "✓ $script is executable"
    else
        echo "✗ $script is NOT executable"
        echo "  Fix with: chmod +x $script"
        SCRIPTS_OK=false
    fi
done

if [ "$SCRIPTS_OK" = true ]; then
    ((PASSED++))
else
    echo ""
    echo "  Fix all permissions:"
    echo "  chmod +x scripts/*.sh build.sh"
    ((FAILED++))
fi
echo ""

# ============================================================================
# CHECK 10: Git Ignore
# ============================================================================

echo "--- Check 10: Git Configuration ---"
echo ""

if [ -f ".gitignore" ]; then
    if grep -q "signing/*.key" .gitignore 2>/dev/null; then
        echo "✓ .gitignore configured to exclude private keys"
        ((PASSED++))
    else
        echo "⚠ .gitignore may not exclude private keys"
        echo "  Add to .gitignore:"
        echo "    signing/*.key"
        echo "    signing/*.p12"
        echo "    dist/"
        ((WARNINGS++))
    fi
else
    echo "⚠ No .gitignore found"
    ((WARNINGS++))
fi
echo ""

# ============================================================================
# SUMMARY
# ============================================================================

echo "========================================"
echo "Setup Verification Summary"
echo "========================================"
echo ""
echo "Passed:   $PASSED checks"
echo "Failed:   $FAILED checks"
echo "Warnings: $WARNINGS checks"
echo ""

if [ $FAILED -eq 0 ]; then
    if [ $WARNINGS -eq 0 ]; then
        echo "🎉 All checks passed! You're ready to sign and notarize SparkPass."
        echo ""
        echo "Next steps:"
        echo "  1. Build and sign: ./scripts/build-and-sign.sh"
        echo "  2. Or use Makefile: make release"
    else
        echo "✓ Setup is mostly complete, but there are some warnings."
        echo "  Review the warnings above and address them if needed."
        echo ""
        echo "You can proceed with signing:"
        echo "  ./scripts/build-and-sign.sh"
    fi
else
    echo "✗ Setup is incomplete. Please address the failed checks above."
    echo ""
    echo "Quick setup guide:"
    echo "  1. Install dependencies: brew install liboqs openssl@3 libsodium"
    echo "  2. Get Developer ID certificate:"
    echo "     ./scripts/generate-csr.sh"
    echo "     (Upload CSR to Apple, download cert, install)"
    echo "  3. Set up notarization:"
    echo "     ./scripts/setup-notarization.sh"
    echo "  4. Configure identity:"
    echo "     export SPARKPASS_SIGNING_IDENTITY='<your identity>'"
    echo ""
    echo "For detailed instructions, see: docs/CODE_SIGNING_QUICKSTART.md"
fi
echo ""

exit $FAILED
