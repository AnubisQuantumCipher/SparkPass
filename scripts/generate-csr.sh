#!/bin/bash
# Generate Certificate Signing Request for Developer ID Application
# This creates a CSR that you'll upload to Apple Developer portal

set -e

# Configuration
COMMON_NAME="Developer ID Application: YOUR_NAME_OR_COMPANY"
EMAIL="your.email@example.com"
OUTPUT_DIR="/Users/sicarii/SparkPass/signing"
CSR_FILE="${OUTPUT_DIR}/sparkpass-developer-id.certSigningRequest"
PRIVATE_KEY="${OUTPUT_DIR}/sparkpass-developer-id.key"

echo "=== Generating Developer ID Application CSR ==="
echo ""
echo "IMPORTANT: Before running this script, update the following:"
echo "  - COMMON_NAME: Replace YOUR_NAME_OR_COMPANY with your Apple Developer name"
echo "  - EMAIL: Your Apple Developer email address"
echo ""
read -p "Have you updated these values? (yes/no): " confirm
if [[ "$confirm" != "yes" ]]; then
    echo "Please edit this script and update COMMON_NAME and EMAIL"
    exit 1
fi

# Create output directory
mkdir -p "${OUTPUT_DIR}"
chmod 700 "${OUTPUT_DIR}"

# Generate CSR using Apple's recommended method
# This creates a 2048-bit RSA key and stores it in your keychain
cat > "${OUTPUT_DIR}/csr-config.txt" << EOF
[ req ]
default_bits       = 2048
distinguished_name = req_distinguished_name
prompt             = no

[ req_distinguished_name ]
CN = ${COMMON_NAME}
emailAddress = ${EMAIL}
EOF

# Generate private key and CSR
openssl req -new -newkey rsa:2048 -nodes \
    -keyout "${PRIVATE_KEY}" \
    -out "${CSR_FILE}" \
    -config "${OUTPUT_DIR}/csr-config.txt"

# Secure the private key
chmod 400 "${PRIVATE_KEY}"

# Clean up config
rm "${OUTPUT_DIR}/csr-config.txt"

echo ""
echo "✓ CSR generated successfully!"
echo ""
echo "Files created:"
echo "  - CSR: ${CSR_FILE}"
echo "  - Private Key: ${PRIVATE_KEY}"
echo ""
echo "NEXT STEPS:"
echo "1. Go to: https://developer.apple.com/account/resources/certificates/add"
echo "2. Select 'Developer ID Application'"
echo "3. Upload the CSR file: ${CSR_FILE}"
echo "4. Download the certificate (e.g., developerID_application.cer)"
echo "5. Run: open developerID_application.cer (installs to Keychain)"
echo "6. Verify with: security find-identity -v -p codesigning"
echo ""
echo "IMPORTANT: Keep ${PRIVATE_KEY} secure! Do NOT commit to git."
