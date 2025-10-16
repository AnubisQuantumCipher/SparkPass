#!/bin/bash
# Create distribution packages for SparkPass
# Supports: tar.gz, zip, DMG, and PKG formats

set -e

PROJECT_ROOT="/Users/sicarii/SparkPass"
BINARY_PATH="${PROJECT_ROOT}/bin/sparkpass_main"
DIST_DIR="${PROJECT_ROOT}/dist"
VERSION="${SPARKPASS_VERSION:-1.0.0}"
DATE=$(date +%Y%m%d-%H%M%S)

echo "=== SparkPass Distribution Package Creator ==="
echo ""
echo "Version: $VERSION"
echo "Binary: $BINARY_PATH"
echo ""

# Verify binary exists and is signed
if [ ! -f "$BINARY_PATH" ]; then
    echo "ERROR: Binary not found at $BINARY_PATH"
    echo "Run 'make build' first."
    exit 1
fi

# Check if signed
if ! codesign --verify "$BINARY_PATH" 2>/dev/null; then
    echo "WARNING: Binary is not signed!"
    echo "For distribution, you should sign and notarize first."
    read -p "Continue anyway? (y/n): " CONTINUE
    if [[ "$CONTINUE" != "y" ]]; then
        exit 1
    fi
fi

mkdir -p "$DIST_DIR"

# ============================================================================
# FORMAT 1: TAR.GZ (Universal, preferred for CLI tools)
# ============================================================================

echo "=== Creating tar.gz archive ==="
echo ""

TARBALL_NAME="sparkpass-${VERSION}-macos-arm64.tar.gz"
TARBALL_PATH="${DIST_DIR}/${TARBALL_NAME}"

# Create staging directory
STAGING="${DIST_DIR}/sparkpass-staging"
rm -rf "$STAGING"
mkdir -p "${STAGING}/sparkpass-${VERSION}"

# Copy binary
cp "$BINARY_PATH" "${STAGING}/sparkpass-${VERSION}/sparkpass"

# Create README for distribution
cat > "${STAGING}/sparkpass-${VERSION}/README.txt" << 'EOF'
SparkPass - Quantum-Resistant Password Manager
===============================================

Installation:
  sudo cp sparkpass /usr/local/bin/
  sudo chmod 755 /usr/local/bin/sparkpass

Usage:
  sparkpass --help

Requirements:
  - macOS 11.0 or later (Apple Silicon)
  - Homebrew dependencies (installed automatically via Homebrew)

First Run:
  1. Run: sparkpass init
  2. Follow the prompts to create your vault

Security:
  - Uses Touch ID / Face ID for authentication
  - Quantum-resistant encryption (ML-KEM, ML-DSA)
  - Data stored in macOS Keychain

Documentation:
  https://github.com/yourusername/sparkpass

License:
  [Your License Here]
EOF

# Create tar.gz
cd "${STAGING}"
tar -czf "$TARBALL_PATH" "sparkpass-${VERSION}"
cd "$PROJECT_ROOT"

# Clean up staging
rm -rf "$STAGING"

echo "✓ Created: $TARBALL_PATH"
ls -lh "$TARBALL_PATH"
echo ""

# ============================================================================
# FORMAT 2: ZIP (Notarization-friendly)
# ============================================================================

echo "=== Creating ZIP archive ==="
echo ""

ZIP_NAME="sparkpass-${VERSION}-macos-arm64.zip"
ZIP_PATH="${DIST_DIR}/${ZIP_NAME}"

# Create staging directory
STAGING="${DIST_DIR}/sparkpass-staging"
rm -rf "$STAGING"
mkdir -p "${STAGING}/sparkpass-${VERSION}"

# Copy binary
cp "$BINARY_PATH" "${STAGING}/sparkpass-${VERSION}/sparkpass"

# Copy README
cat > "${STAGING}/sparkpass-${VERSION}/README.txt" << 'EOF'
SparkPass - Quantum-Resistant Password Manager
===============================================

Installation:
  sudo cp sparkpass /usr/local/bin/
  sudo chmod 755 /usr/local/bin/sparkpass

Usage:
  sparkpass --help

For more information, visit:
  https://github.com/yourusername/sparkpass
EOF

# Create zip (preserving permissions and symlinks)
cd "${STAGING}"
ditto -c -k --keepParent "sparkpass-${VERSION}" "$ZIP_PATH"
cd "$PROJECT_ROOT"

# Clean up staging
rm -rf "$STAGING"

echo "✓ Created: $ZIP_PATH"
ls -lh "$ZIP_PATH"
echo ""

# ============================================================================
# FORMAT 3: DMG (Optional, more user-friendly)
# ============================================================================

read -p "Create DMG image? (y/n): " CREATE_DMG

if [[ "$CREATE_DMG" == "y" ]]; then
    echo ""
    echo "=== Creating DMG image ==="
    echo ""

    DMG_NAME="SparkPass-${VERSION}-macos-arm64.dmg"
    DMG_PATH="${DIST_DIR}/${DMG_NAME}"
    DMG_TEMP="${DIST_DIR}/dmg-temp"

    # Create DMG staging directory
    rm -rf "$DMG_TEMP"
    mkdir -p "$DMG_TEMP"

    # Copy binary
    cp "$BINARY_PATH" "${DMG_TEMP}/sparkpass"

    # Create install script
    cat > "${DMG_TEMP}/Install SparkPass.command" << 'INSTALL_EOF'
#!/bin/bash
# Install SparkPass to /usr/local/bin

cd "$(dirname "$0")"

echo "Installing SparkPass to /usr/local/bin..."
sudo mkdir -p /usr/local/bin
sudo cp sparkpass /usr/local/bin/
sudo chmod 755 /usr/local/bin/sparkpass

echo ""
echo "✓ SparkPass installed successfully!"
echo ""
echo "Run 'sparkpass --help' to get started."
echo ""
read -p "Press ENTER to close..."
INSTALL_EOF
    chmod +x "${DMG_TEMP}/Install SparkPass.command"

    # Create README
    cat > "${DMG_TEMP}/README.txt" << 'README_EOF'
SparkPass - Quantum-Resistant Password Manager
===============================================

To install:
  1. Double-click "Install SparkPass.command"
  2. Enter your password when prompted
  3. Run "sparkpass --help" in Terminal

Manual installation:
  sudo cp sparkpass /usr/local/bin/
  sudo chmod 755 /usr/local/bin/sparkpass

Requirements:
  - macOS 11.0 or later (Apple Silicon)
  - Homebrew (for dependencies)

Documentation:
  https://github.com/yourusername/sparkpass
README_EOF

    # Create DMG
    hdiutil create -volname "SparkPass ${VERSION}" \
        -srcfolder "$DMG_TEMP" \
        -ov -format UDZO \
        "$DMG_PATH"

    # Clean up
    rm -rf "$DMG_TEMP"

    echo "✓ Created: $DMG_PATH"
    ls -lh "$DMG_PATH"
    echo ""

    # If binary is signed, sign the DMG too
    if codesign --verify "$BINARY_PATH" 2>/dev/null; then
        if [ -n "$SPARKPASS_SIGNING_IDENTITY" ]; then
            echo "Signing DMG..."
            codesign --sign "$SPARKPASS_SIGNING_IDENTITY" \
                --timestamp \
                "$DMG_PATH"
            echo "✓ DMG signed"
        fi
    fi
fi

# ============================================================================
# FORMAT 4: PKG (Installer package)
# ============================================================================

read -p "Create PKG installer? (y/n): " CREATE_PKG

if [[ "$CREATE_PKG" == "y" ]]; then
    echo ""
    echo "=== Creating PKG installer ==="
    echo ""

    PKG_NAME="SparkPass-${VERSION}-macos-arm64.pkg"
    PKG_PATH="${DIST_DIR}/${PKG_NAME}"
    PKG_ROOT="${DIST_DIR}/pkg-root"

    # Create package root
    rm -rf "$PKG_ROOT"
    mkdir -p "${PKG_ROOT}/usr/local/bin"
    cp "$BINARY_PATH" "${PKG_ROOT}/usr/local/bin/sparkpass"

    # Build package
    pkgbuild --identifier "com.sparkpass.cli" \
        --version "$VERSION" \
        --install-location "/" \
        --root "$PKG_ROOT" \
        "$PKG_PATH"

    # If binary is signed, sign the package
    if codesign --verify "$BINARY_PATH" 2>/dev/null; then
        if [ -n "$SPARKPASS_SIGNING_IDENTITY" ]; then
            echo "Signing PKG..."
            productsign --sign "$SPARKPASS_SIGNING_IDENTITY" \
                --timestamp \
                "$PKG_PATH" \
                "${PKG_PATH}.signed"
            mv "${PKG_PATH}.signed" "$PKG_PATH"
            echo "✓ PKG signed"
        fi
    fi

    # Clean up
    rm -rf "$PKG_ROOT"

    echo "✓ Created: $PKG_PATH"
    ls -lh "$PKG_PATH"
    echo ""
fi

# ============================================================================
# CREATE CHECKSUMS
# ============================================================================

echo "=== Generating Checksums ==="
echo ""

cd "$DIST_DIR"
shasum -a 256 sparkpass-*.tar.gz sparkpass-*.zip > "sparkpass-${VERSION}-checksums.txt" 2>/dev/null || true
if [ -f "SparkPass-${VERSION}-macos-arm64.dmg" ]; then
    shasum -a 256 SparkPass-*.dmg >> "sparkpass-${VERSION}-checksums.txt"
fi
if [ -f "SparkPass-${VERSION}-macos-arm64.pkg" ]; then
    shasum -a 256 SparkPass-*.pkg >> "sparkpass-${VERSION}-checksums.txt"
fi
cd "$PROJECT_ROOT"

echo "✓ Checksums saved to: ${DIST_DIR}/sparkpass-${VERSION}-checksums.txt"
cat "${DIST_DIR}/sparkpass-${VERSION}-checksums.txt"
echo ""

# ============================================================================
# SUMMARY
# ============================================================================

echo "========================================"
echo "Distribution Packages Created"
echo "========================================"
echo ""
echo "Location: $DIST_DIR"
echo ""
ls -lh "$DIST_DIR"/sparkpass-* "$DIST_DIR"/SparkPass-* 2>/dev/null || true
echo ""
echo "Upload to GitHub Releases:"
echo "  gh release create v${VERSION} ${DIST_DIR}/sparkpass-${VERSION}*"
echo ""
echo "Or manually upload at:"
echo "  https://github.com/yourusername/sparkpass/releases/new"
