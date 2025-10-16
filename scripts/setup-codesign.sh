#!/bin/bash
#
# SparkPass Code-Signing Setup Script
#
# This script creates a self-signed code-signing certificate for SparkPass
# and signs the binary with it. This ensures the Keychain items remain
# accessible across rebuilds.
#

set -e

CERT_NAME="SparkPass Dev"
BINARY="./bin/sparkpass_main"

echo "=== SparkPass Code-Signing Setup ==="
echo ""

# Check if certificate already exists
if security find-identity -v -p codesigning | grep -q "$CERT_NAME"; then
    echo "✓ Certificate '$CERT_NAME' already exists"
else
    echo "Creating self-signed code-signing certificate..."
    echo ""
    echo "INSTRUCTIONS:"
    echo "1. A dialog will appear to create a certificate"
    echo "2. Enter these values:"
    echo "   - Name: $CERT_NAME"
    echo "   - Identity Type: Self Signed Root"
    echo "   - Certificate Type: Code Signing"
    echo "   - Let me override defaults: CHECKED"
    echo "3. Click Continue through all steps (accept defaults)"
    echo "4. Click Done"
    echo ""
    echo "Opening Keychain Access..."

    # Open Keychain Access to Certificate Assistant
    open "/System/Library/CoreServices/Applications/Keychain Access.app"

    echo ""
    echo "MANUAL STEP REQUIRED:"
    echo "In Keychain Access:"
    echo "  1. Keychain Access → Certificate Assistant → Create a Certificate..."
    echo "  2. Name: $CERT_NAME"
    echo "  3. Identity Type: Self Signed Root"
    echo "  4. Certificate Type: Code Signing"
    echo "  5. Click Create"
    echo ""
    read -p "Press ENTER after creating the certificate..."

    # Verify certificate was created
    if ! security find-identity -v -p codesigning | grep -q "$CERT_NAME"; then
        echo "✗ Certificate not found. Please create it manually and try again."
        exit 1
    fi

    echo "✓ Certificate created successfully"
fi

echo ""
echo "=== Signing Binary ==="

if [ ! -f "$BINARY" ]; then
    echo "✗ Binary not found: $BINARY"
    echo "  Run 'gprbuild -p -P sparkpass.gpr' first"
    exit 1
fi

echo "Binary: $BINARY"
echo "Certificate: $CERT_NAME"
echo ""

# Sign the binary
codesign --force --sign "$CERT_NAME" "$BINARY"

# Verify signature
echo ""
echo "=== Verifying Signature ==="
codesign -dv --verbose=4 "$BINARY" 2>&1 | grep -E "Authority|Identifier|Signature="

echo ""
echo "✓ Binary signed successfully!"
echo ""
echo "IMPORTANT: Always sign the binary after rebuilding:"
echo "  codesign --force --sign '$CERT_NAME' $BINARY"
echo ""
echo "Or use the automated script:"
echo "  ./scripts/build-and-sign.sh"
