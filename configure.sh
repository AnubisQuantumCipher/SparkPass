#!/bin/bash
# SparkPass Build Configuration Script
# Generates library paths using pkg-config for portable builds

set -e

echo "Configuring SparkPass build environment..."

# Check for pkg-config
if ! command -v pkg-config &> /dev/null; then
    echo "Error: pkg-config not found. Please install pkg-config first."
    exit 1
fi

# Check for required libraries
for lib in liboqs openssl libsodium; do
    if ! pkg-config --exists "$lib" 2>/dev/null; then
        echo "Error: $lib not found via pkg-config"
        echo "Please install $lib and ensure it's in PKG_CONFIG_PATH"
        exit 1
    fi
done

# Generate library flags
export LIBOQS_LIBS=$(pkg-config --libs liboqs)
export OPENSSL_LIBS=$(pkg-config --libs openssl)
export LIBSODIUM_LIBS=$(pkg-config --libs libsodium)

# Extract -L flags (library paths) and -l flags (library names)
export LIBOQS_LDFLAGS=$(echo "$LIBOQS_LIBS" | grep -o '\-L[^ ]*' | head -1)
export OPENSSL_LDFLAGS=$(echo "$OPENSSL_LIBS" | grep -o '\-L[^ ]*' | head -1)
export LIBSODIUM_LDFLAGS=$(echo "$LIBSODIUM_LIBS" | grep -o '\-L[^ ]*' | head -1)

echo "Library configuration:"
echo "  liboqs:    $LIBOQS_LDFLAGS"
echo "  openssl:   $OPENSSL_LDFLAGS"
echo "  libsodium: $LIBSODIUM_LDFLAGS"
echo ""
echo "Configuration complete. Build environment variables set."
echo ""
echo "To build SparkPass, source this script and run gprbuild:"
echo "  source ./configure.sh"
echo "  gprbuild -P sparkpass.gpr"
