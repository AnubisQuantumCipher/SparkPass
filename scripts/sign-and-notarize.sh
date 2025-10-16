#!/usr/bin/env bash
set -euo pipefail

# ==== CONFIG =========================================================
APP_NAME="sparkpass"
BIN_PATH="bin/sparkpass_main"
PKG_ID="com.anubisquantumcipher.sparkpass"
PKG_VERSION="${1:-1.0.0}"
TEAM_ID="E9VB3VKZKH"

DEV_ID_APP="Developer ID Application: AnubisQuantumCipher (${TEAM_ID})"
DEV_ID_INST="Developer ID Installer: AnubisQuantumCipher (${TEAM_ID})"
NOTARY_PROFILE="anubis-notary"

STAGE="dist"
ENTITLEMENTS="signing/sparkpass.entitlements"
# ====================================================================

echo "========================================"
echo "SparkPass Code Signing & Notarization"
echo "Version: ${PKG_VERSION}"
echo "========================================"
echo ""

# Check if enrolled in Apple Developer Program
if ! security find-identity -v -p codesigning | grep -q "Developer ID Application"; then
    echo "ERROR: No Developer ID certificates found."
    echo ""
    echo "You need an Apple Developer Program membership (\$99/year)"
    echo "to code sign and notarize applications."
    echo ""
    echo "Enroll at: https://developer.apple.com/programs/enroll/"
    echo ""
    echo "After enrollment completes (24-48 hours):"
    echo "1. Generate certificates at developer.apple.com"
    echo "2. Run: make release VER=${PKG_VERSION}"
    echo ""
    exit 1
fi

mkdir -p "${STAGE}"

echo "==> Building (Ada/SPARK + Objective-C bindings)"
./build.sh

if [ ! -f "${BIN_PATH}" ]; then
    echo "ERROR: Build failed - binary not found: ${BIN_PATH}"
    exit 1
fi

echo "==> Code signing binary (Hardened Runtime)"
codesign --remove-signature "${BIN_PATH}" 2>/dev/null || true
codesign --force --timestamp --options runtime \
  --entitlements "${ENTITLEMENTS}" \
  --sign "${DEV_ID_APP}" \
  --verbose \
  "${BIN_PATH}"

echo ""
echo "==> Verifying code signature"
codesign --verify --deep --strict --verbose=2 "${BIN_PATH}"
codesign -dv --verbose=4 "${BIN_PATH}" 2>&1 | head -10

echo ""
echo "==> Creating ZIP distribution"
ZIP="${STAGE}/${APP_NAME}-${PKG_VERSION}-macos-$(uname -m).zip"
ditto -c -k --keepParent "${BIN_PATH}" "${ZIP}"
echo "Created: ${ZIP}"

echo ""
echo "==> Notarizing ZIP (this takes 2-10 minutes)"
xcrun notarytool submit "${ZIP}" \
  --keychain-profile "${NOTARY_PROFILE}" \
  --wait

echo ""
echo "==> Building unsigned PKG"
PKG_UNSIGNED="${STAGE}/${APP_NAME}-unsigned.pkg"
pkgbuild --install-location /usr/local/bin \
  --component "${BIN_PATH}" \
  --identifier "${PKG_ID}" \
  --version "${PKG_VERSION}" \
  "${PKG_UNSIGNED}"

echo ""
echo "==> Signing installer PKG"
PKG_SIGNED="${STAGE}/${APP_NAME}-${PKG_VERSION}-installer.pkg"
productbuild --sign "${DEV_ID_INST}" \
  --timestamp \
  --package "${PKG_UNSIGNED}" \
  "${PKG_SIGNED}"

echo ""
echo "==> Notarizing PKG (this takes 2-10 minutes)"
xcrun notarytool submit "${PKG_SIGNED}" \
  --keychain-profile "${NOTARY_PROFILE}" \
  --wait

echo ""
echo "==> Stapling notarization ticket to PKG"
xcrun stapler staple "${PKG_SIGNED}"

echo ""
echo "==> Validating stapled ticket"
xcrun stapler validate "${PKG_SIGNED}"

echo ""
echo "==> Gatekeeper verification"
spctl -a -vvv -t install "${PKG_SIGNED}" 2>&1 | head -10 || true

echo ""
echo "==> Cleaning up"
rm -f "${PKG_UNSIGNED}"

echo ""
echo "========================================"
echo "SUCCESS: Release artifacts created"
echo "========================================"
echo "ZIP:  ${ZIP}"
echo "PKG:  ${PKG_SIGNED} (notarized + stapled)"
echo ""
echo "To verify:"
echo "  codesign -dv --verbose=4 ${BIN_PATH}"
echo "  spctl -a -vvv ${PKG_SIGNED}"
echo ""
echo "To install:"
echo "  sudo installer -pkg ${PKG_SIGNED} -target /"
echo ""
