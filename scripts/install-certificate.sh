#!/bin/bash
# Install Developer ID Application certificate
# Run this after downloading the certificate from Apple Developer portal

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <path-to-downloaded-certificate.cer>"
    echo ""
    echo "Example:"
    echo "  $0 ~/Downloads/developerID_application.cer"
    exit 1
fi

CERT_FILE="$1"

if [ ! -f "$CERT_FILE" ]; then
    echo "ERROR: Certificate file not found: $CERT_FILE"
    exit 1
fi

echo "=== Installing Developer ID Application Certificate ==="
echo ""
echo "Certificate: $CERT_FILE"
echo ""

# Import certificate into login keychain
security import "$CERT_FILE" -k ~/Library/Keychains/login.keychain-db -T /usr/bin/codesign

echo ""
echo "✓ Certificate installed to login keychain"
echo ""
echo "Available code signing identities:"
security find-identity -v -p codesigning

echo ""
echo "NEXT STEP:"
echo "Copy the full identity name (e.g., 'Developer ID Application: John Doe (TEAM123456)')"
echo "You'll need this for code signing."
