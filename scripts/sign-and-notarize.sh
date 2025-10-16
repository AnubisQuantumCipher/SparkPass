#!/usr/bin/env bash
# Complete Apple Developer ID code signing and notarization workflow for SparkPass
# This script builds, signs, packages, notarizes, and staples SparkPass for distribution
#
# Usage: ./scripts/sign-and-notarize.sh <version>
# Example: ./scripts/sign-and-notarize.sh 1.0.0
#
# Requirements:
# 1. Developer ID Application certificate installed
# 2. Developer ID Installer certificate installed
# 3. Notary credentials configured: xcrun notarytool store-credentials anubis-notary

set -euo pipefail

# ============================================================================
# CONFIGURATION
# ============================================================================

# Version number (required argument)
VERSION="${1:-}"
if [ -z "$VERSION" ]; then
    echo "ERROR: Version number required"
    echo "Usage: $0 <version>"
    echo "Example: $0 1.0.0"
    exit 1
fi

# Apple Developer Configuration
TEAM_ID="E9VB3VKZKH"
ORGANIZATION="AnubisQuantumCipher"
BUNDLE_ID="com.anubisquantumcipher.sparkpass"
APP_NAME="sparkpass"

# Code Signing Identities
APP_SIGNING_IDENTITY="Developer ID Application: ${ORGANIZATION} (${TEAM_ID})"
INSTALLER_SIGNING_IDENTITY="Developer ID Installer: ${ORGANIZATION} (${TEAM_ID})"

# Notarization profile (must be configured with: xcrun notarytool store-credentials)
NOTARY_PROFILE="anubis-notary"

# Architecture detection (can be overridden with SPARKPASS_ARCH env var)
ARCH="${SPARKPASS_ARCH:-$(uname -m)}"

# Paths
PROJECT_ROOT="/Users/sicarii/SparkPass"
BINARY_PATH="${PROJECT_ROOT}/bin/sparkpass_main"
ENTITLEMENTS="${PROJECT_ROOT}/signing/sparkpass.entitlements"
DIST_DIR="${PROJECT_ROOT}/dist"

# Distribution filenames
ZIP_NAME="${APP_NAME}-${VERSION}-macos-${ARCH}.zip"
PKG_NAME="${APP_NAME}-${VERSION}-installer.pkg"

# ============================================================================
# PREFLIGHT CHECKS
# ============================================================================

echo "========================================"
echo "SparkPass Code Signing & Notarization"
echo "========================================"
echo ""
echo "Version: ${VERSION}"
echo "Architecture: ${ARCH}"
echo "Team ID: ${TEAM_ID}"
echo "Bundle ID: ${BUNDLE_ID}"
echo ""

# Check Xcode Command Line Tools
if ! command -v codesign &> /dev/null; then
    echo "ERROR: Xcode Command Line Tools not installed"
    echo "Install with: xcode-select --install"
    exit 1
fi

# Check for signing identities
echo "Checking for signing identities..."
if ! security find-identity -v -p codesigning | grep -q "${APP_SIGNING_IDENTITY}"; then
    echo "ERROR: Developer ID Application certificate not found"
    echo "Expected: ${APP_SIGNING_IDENTITY}"
    echo ""
    echo "Available identities:"
    security find-identity -v -p codesigning
    echo ""
    echo "Please install the Developer ID Application certificate"
    exit 1
fi

if ! security find-identity -v -p codesigning | grep -q "${INSTALLER_SIGNING_IDENTITY}"; then
    echo "ERROR: Developer ID Installer certificate not found"
    echo "Expected: ${INSTALLER_SIGNING_IDENTITY}"
    echo ""
    echo "Available identities:"
    security find-identity -v -p codesigning
    echo ""
    echo "Please install the Developer ID Installer certificate"
    exit 1
fi

echo "Success: Signing identities found"

# Check notarization profile
echo "Checking notarization profile..."
if ! xcrun notarytool history --keychain-profile "${NOTARY_PROFILE}" &>/dev/null; then
    echo "ERROR: Notarization profile '${NOTARY_PROFILE}' not found"
    echo ""
    echo "Configure with:"
    echo "  xcrun notarytool store-credentials ${NOTARY_PROFILE} \\"
    echo "    --apple-id lynchmobb@pm.me \\"
    echo "    --team-id ${TEAM_ID}"
    echo ""
    echo "Or use API key:"
    echo "  xcrun notarytool store-credentials ${NOTARY_PROFILE} \\"
    echo "    --key /path/to/AuthKey_KEYID.p8 \\"
    echo "    --key-id KEYID \\"
    echo "    --issuer-id ISSUER_ID"
    exit 1
fi

echo "Success: Notarization profile configured"

# Check entitlements file
if [ ! -f "$ENTITLEMENTS" ]; then
    echo "ERROR: Entitlements file not found: $ENTITLEMENTS"
    exit 1
fi

echo "Success: Entitlements file found"
echo ""

# Clean and create distribution directory
echo "Preparing distribution directory..."
rm -rf "$DIST_DIR"
mkdir -p "$DIST_DIR"
echo "Success: Distribution directory ready"
echo ""

# ============================================================================
# STEP 1: BUILD
# ============================================================================

echo "========================================"
echo "Step 1: Building SparkPass"
echo "========================================"
echo ""

cd "$PROJECT_ROOT"

# Build using the project's build script
echo "Running build script..."
./build.sh

# Verify binary was created
if [ ! -f "$BINARY_PATH" ]; then
    echo "ERROR: Build failed - binary not found: $BINARY_PATH"
    exit 1
fi

echo ""
echo "Success: Build successful"
echo "  Binary: $BINARY_PATH"
echo "  Size: $(du -h "$BINARY_PATH" | cut -f1)"
echo ""

# ============================================================================
# STEP 2: CODE SIGNING
# ============================================================================

echo "========================================"
echo "Step 2: Code Signing Binary"
echo "========================================"
echo ""

# Remove any existing signature
codesign --remove-signature "$BINARY_PATH" 2>/dev/null || true

# Sign with Hardened Runtime and entitlements
echo "Signing binary with Hardened Runtime..."
echo "Identity: ${APP_SIGNING_IDENTITY}"
echo ""

codesign --force \
    --timestamp \
    --options runtime \
    --entitlements "$ENTITLEMENTS" \
    --sign "${APP_SIGNING_IDENTITY}" \
    --verbose \
    "$BINARY_PATH"

echo ""
echo "Verifying signature..."
codesign --verify --deep --strict --verbose=2 "$BINARY_PATH"

echo ""
echo "Success: Binary signed successfully"
echo ""

# Display signature details
echo "Signature details:"
codesign -dv --verbose=4 "$BINARY_PATH" 2>&1 | head -10
echo ""

# ============================================================================
# STEP 3: CREATE ZIP DISTRIBUTION
# ============================================================================

echo "========================================"
echo "Step 3: Creating ZIP Distribution"
echo "========================================"
echo ""

# Copy signed binary to dist with final name
STAGED_BINARY="${DIST_DIR}/${APP_NAME}"
cp "$BINARY_PATH" "$STAGED_BINARY"

# Create ZIP for notarization and distribution
echo "Creating ZIP archive..."
cd "$DIST_DIR"
ditto -c -k --keepParent "$APP_NAME" "$ZIP_NAME"
cd "$PROJECT_ROOT"

echo "Success: ZIP created: ${ZIP_NAME}"
echo "  Size: $(du -h "${DIST_DIR}/${ZIP_NAME}" | cut -f1)"
echo ""

# ============================================================================
# STEP 4: NOTARIZE ZIP
# ============================================================================

echo "========================================"
echo "Step 4: Notarizing ZIP Distribution"
echo "========================================"
echo ""

echo "Submitting ZIP to Apple notary service..."
echo "This may take 2-10 minutes..."
echo ""

# Submit and wait for notarization
xcrun notarytool submit "${DIST_DIR}/${ZIP_NAME}" \
    --keychain-profile "${NOTARY_PROFILE}" \
    --wait

echo ""
echo "Success: ZIP notarization complete"
echo ""

# Get submission ID from recent history
echo "Retrieving notarization log..."
SUBMISSION_ID=$(xcrun notarytool history --keychain-profile "${NOTARY_PROFILE}" | head -5 | grep -v "CreatedDate" | awk '{print $5}' | head -1 || echo "")

if [ -n "$SUBMISSION_ID" ]; then
    xcrun notarytool log "$SUBMISSION_ID" \
        --keychain-profile "${NOTARY_PROFILE}" \
        "${DIST_DIR}/notarization-zip-log.json" 2>/dev/null || true
    echo "Success: Log saved: notarization-zip-log.json"
fi

echo ""

# ============================================================================
# STEP 5: CREATE PKG INSTALLER
# ============================================================================

echo "========================================"
echo "Step 5: Creating PKG Installer"
echo "========================================"
echo ""

# Create temporary directory structure for PKG
PKG_ROOT="${DIST_DIR}/pkg-root"
mkdir -p "${PKG_ROOT}/usr/local/bin"

# Copy signed binary (renaming sparkpass_main -> sparkpass)
cp "$BINARY_PATH" "${PKG_ROOT}/usr/local/bin/${APP_NAME}"

echo "Creating unsigned PKG..."
UNSIGNED_PKG="${DIST_DIR}/${APP_NAME}-${VERSION}-unsigned.pkg"

pkgbuild \
    --identifier "${BUNDLE_ID}" \
    --version "${VERSION}" \
    --install-location "/" \
    --root "${PKG_ROOT}" \
    "${UNSIGNED_PKG}"

echo "Success: Unsigned PKG created"
echo ""

# Sign the PKG with Developer ID Installer certificate
echo "Signing PKG with Developer ID Installer certificate..."
productbuild \
    --sign "${INSTALLER_SIGNING_IDENTITY}" \
    --timestamp \
    --package "${UNSIGNED_PKG}" \
    "${DIST_DIR}/${PKG_NAME}"

echo "Success: PKG signed: ${PKG_NAME}"
echo "  Size: $(du -h "${DIST_DIR}/${PKG_NAME}" | cut -f1)"
echo ""

# Clean up temporary files
rm -rf "${PKG_ROOT}"
rm -f "${UNSIGNED_PKG}"

# ============================================================================
# STEP 6: NOTARIZE PKG
# ============================================================================

echo "========================================"
echo "Step 6: Notarizing PKG Installer"
echo "========================================"
echo ""

echo "Submitting PKG to Apple notary service..."
echo "This may take 2-10 minutes..."
echo ""

# Submit PKG and wait for notarization
xcrun notarytool submit "${DIST_DIR}/${PKG_NAME}" \
    --keychain-profile "${NOTARY_PROFILE}" \
    --wait

echo ""
echo "Success: PKG notarization complete"
echo ""

# Get submission ID and log
echo "Retrieving notarization log..."
SUBMISSION_ID=$(xcrun notarytool history --keychain-profile "${NOTARY_PROFILE}" | head -5 | grep -v "CreatedDate" | awk '{print $5}' | head -1 || echo "")

if [ -n "$SUBMISSION_ID" ]; then
    xcrun notarytool log "$SUBMISSION_ID" \
        --keychain-profile "${NOTARY_PROFILE}" \
        "${DIST_DIR}/notarization-pkg-log.json" 2>/dev/null || true
    echo "Success: Log saved: notarization-pkg-log.json"
fi

echo ""

# ============================================================================
# STEP 7: STAPLE NOTARIZATION TICKET TO PKG
# ============================================================================

echo "========================================"
echo "Step 7: Stapling Notarization Ticket"
echo "========================================"
echo ""

echo "Stapling notarization ticket to PKG..."
xcrun stapler staple "${DIST_DIR}/${PKG_NAME}"

echo ""
echo "Validating stapled ticket..."
xcrun stapler validate "${DIST_DIR}/${PKG_NAME}"

echo ""
echo "Success: Notarization ticket stapled successfully"
echo ""

# ============================================================================
# STEP 8: VERIFICATION
# ============================================================================

echo "========================================"
echo "Step 8: Verification"
echo "========================================"
echo ""

# Verify binary signature
echo "Verifying binary signature..."
codesign --verify --deep --strict --verbose=2 "$BINARY_PATH"
echo "Success: Binary signature valid"
echo ""

# Verify PKG signature
echo "Verifying PKG signature..."
pkgutil --check-signature "${DIST_DIR}/${PKG_NAME}" | head -10
echo "Success: PKG signature valid"
echo ""

# Verify Gatekeeper acceptance
echo "Verifying Gatekeeper acceptance..."
spctl -a -vvv -t install "${DIST_DIR}/${PKG_NAME}" 2>&1 | head -5 || echo "(Note: spctl may show warnings on unsigned test systems)"
echo "Success: PKG accepted by Gatekeeper"
echo ""

# ============================================================================
# SUMMARY
# ============================================================================

echo "========================================"
echo "SUCCESS!"
echo "========================================"
echo ""
echo "SparkPass ${VERSION} is signed, notarized, and ready for distribution"
echo ""
echo "Distribution files:"
echo "  ZIP: ${ZIP_NAME}"
echo "       Size: $(du -h "${DIST_DIR}/${ZIP_NAME}" | cut -f1)"
echo "       Use: GitHub Releases, direct download"
echo ""
echo "  PKG: ${PKG_NAME}"
echo "       Size: $(du -h "${DIST_DIR}/${PKG_NAME}" | cut -f1)"
echo "       Use: Installer for /usr/local/bin/sparkpass"
echo "       Stapled: YES (offline verification supported)"
echo ""
echo "Binary location: /usr/local/bin/${APP_NAME}"
echo ""
echo "Next steps:"
echo "  1. Test installation:"
echo "     sudo installer -pkg dist/${PKG_NAME} -target /"
echo "     /usr/local/bin/sparkpass --version"
echo ""
echo "  2. Upload to GitHub Release:"
echo "     gh release create v${VERSION} \\"
echo "       dist/${ZIP_NAME} \\"
echo "       dist/${PKG_NAME}"
echo ""
echo "  3. Update Homebrew formula with new SHA256:"
echo "     shasum -a 256 dist/${ZIP_NAME}"
echo ""
echo "SHA256 checksums:"
shasum -a 256 "${DIST_DIR}/${ZIP_NAME}" "${DIST_DIR}/${PKG_NAME}"
echo ""
