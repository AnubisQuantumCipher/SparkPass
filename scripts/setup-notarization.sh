#!/bin/bash
# Set up notarization credentials for SparkPass
# This creates a keychain profile for secure credential storage

set -e

echo "=== Setting Up Notarization Credentials ==="
echo ""
echo "You need an App-Specific Password from Apple."
echo ""
echo "STEP 1: Generate App-Specific Password"
echo "  1. Go to: https://appleid.apple.com/account/manage"
echo "  2. Sign in with your Apple ID"
echo "  3. Under 'Security' → 'App-Specific Passwords' → Click '+'"
echo "  4. Name it 'SparkPass Notarization'"
echo "  5. Copy the generated password (format: xxxx-xxxx-xxxx-xxxx)"
echo ""
read -p "Press ENTER when you have your app-specific password ready..."

echo ""
echo "STEP 2: Store Credentials in Keychain"
echo ""
read -p "Enter your Apple ID email: " APPLE_ID
read -sp "Enter your app-specific password: " APP_PASSWORD
echo ""
read -p "Enter your Team ID (10-character, e.g., ABCD123456): " TEAM_ID

echo ""
echo "Creating notarization keychain profile 'sparkpass-notarization'..."

# Store credentials securely in keychain
xcrun notarytool store-credentials "sparkpass-notarization" \
    --apple-id "$APPLE_ID" \
    --team-id "$TEAM_ID" \
    --password "$APP_PASSWORD"

echo ""
echo "✓ Notarization credentials stored successfully!"
echo ""
echo "Profile name: sparkpass-notarization"
echo "Apple ID: $APPLE_ID"
echo "Team ID: $TEAM_ID"
echo ""
echo "SECURITY NOTE:"
echo "Your credentials are stored securely in macOS Keychain."
echo "The profile 'sparkpass-notarization' will be used for all notarization requests."
echo ""
echo "To verify, run: xcrun notarytool history --keychain-profile sparkpass-notarization"
