#!/bin/bash
# Complete build, sign, and notarize workflow for SparkPass
# This is the one-command solution for creating a release

set -e

PROJECT_ROOT="/Users/sicarii/SparkPass"
cd "$PROJECT_ROOT"

echo "=== SparkPass Complete Build and Release Workflow ==="
echo ""

# ============================================================================
# STEP 1: BUILD
# ============================================================================

echo "=== Step 1: Building SparkPass ==="
echo ""

./build.sh

if [ ! -f "bin/sparkpass_main" ]; then
    echo "ERROR: Build failed - binary not created"
    exit 1
fi

echo ""
echo "✓ Build completed successfully"
echo ""

# ============================================================================
# STEP 2: SIGN AND NOTARIZE
# ============================================================================

echo "=== Step 2: Signing and Notarization ==="
echo ""

# Check if signing identity is configured
if [ -z "$SPARKPASS_SIGNING_IDENTITY" ]; then
    echo "WARNING: SPARKPASS_SIGNING_IDENTITY not set"
    echo ""
    echo "Available signing identities:"
    security find-identity -v -p codesigning
    echo ""
    read -p "Do you want to sign and notarize? (y/n): " SIGN_NOW

    if [[ "$SIGN_NOW" == "y" ]]; then
        echo ""
        read -p "Enter your signing identity (full name from above): " IDENTITY
        export SPARKPASS_SIGNING_IDENTITY="$IDENTITY"
        ./scripts/sign-and-notarize.sh
    else
        echo ""
        echo "Skipping code signing and notarization."
        echo "To sign later, run: ./scripts/sign-and-notarize.sh"
        echo ""
        echo "Build completed: bin/sparkpass_main"
    fi
else
    ./scripts/sign-and-notarize.sh
fi

echo ""
echo "=== Build and Release Complete ==="
