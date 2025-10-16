#!/bin/bash
#
# Programmatically create a self-signed code-signing certificate
# for SparkPass development
#

set -e

CERT_NAME="SparkPass Dev"
KEYCHAIN="$HOME/Library/Keychains/login.keychain-db"

echo "=== Creating Self-Signed Code-Signing Certificate ==="
echo ""

# Check if certificate already exists
if security find-identity -v -p codesigning | grep -q "$CERT_NAME"; then
    echo "✓ Certificate '$CERT_NAME' already exists"
    echo ""
    security find-identity -v -p codesigning | grep "$CERT_NAME"
    exit 0
fi

echo "Certificate name: $CERT_NAME"
echo "Keychain: $KEYCHAIN"
echo ""

# Create a temporary config file for the certificate
CERT_CONFIG=$(mktemp)
cat > "$CERT_CONFIG" <<EOF
[ req ]
default_bits       = 2048
distinguished_name = req_distinguished_name
x509_extensions    = v3_req
prompt             = no

[ req_distinguished_name ]
CN = $CERT_NAME

[ v3_req ]
keyUsage = critical, digitalSignature
extendedKeyUsage = codeSigning
basicConstraints = CA:false
EOF

echo "Generating RSA key pair and self-signed certificate..."

# Generate certificate
CERT_PEM=$(mktemp)
openssl req -x509 -new -nodes -newkey rsa:2048 \
    -keyout /tmp/sparkpass_key.pem \
    -out "$CERT_PEM" \
    -days 3650 \
    -config "$CERT_CONFIG" \
    2>/dev/null

# Convert PEM to PKCS#12 (required for macOS keychain)
CERT_P12=$(mktemp)
openssl pkcs12 -export \
    -out "$CERT_P12" \
    -inkey /tmp/sparkpass_key.pem \
    -in "$CERT_PEM" \
    -passout pass: \
    2>/dev/null

# Import into keychain
echo "Importing certificate into login keychain..."
security import "$CERT_P12" -k "$KEYCHAIN" -T /usr/bin/codesign -T /usr/bin/security

# Set trust settings to always trust for code signing
echo "Setting trust policy..."
security set-key-partition-list -S apple-tool:,apple: -k "" "$KEYCHAIN" 2>/dev/null || true

# Clean up
rm -f "$CERT_CONFIG" "$CERT_PEM" "$CERT_P12" /tmp/sparkpass_key.pem

echo ""
echo "✓ Certificate created successfully!"
echo ""

# Display the certificate
security find-identity -v -p codesigning | grep "$CERT_NAME" || {
    echo "⚠ Certificate created but not visible. You may need to unlock the keychain."
    echo "  Run: security unlock-keychain $KEYCHAIN"
}

echo ""
echo "To use this certificate:"
echo "  codesign --force --sign '$CERT_NAME' ./bin/sparkpass_main"
