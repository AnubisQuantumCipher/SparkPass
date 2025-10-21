#!/bin/bash
# Build script for SparkPass with LAContext support
# This script compiles Objective-C helpers before running gprbuild

set -e

echo "=== Building SparkPass with LAContext ==="

# Set up PATH for Alire toolchains
export PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:/usr/bin:/bin:$PATH"

# Configure library paths using pkg-config (with fallbacks)
echo "Detecting library paths..."
if command -v pkg-config &> /dev/null; then
    LIBOQS_LDFLAGS=$(pkg-config --libs-only-L liboqs 2>/dev/null || echo "-L/opt/homebrew/Cellar/liboqs/0.14.0/lib")
    OPENSSL_LDFLAGS=$(pkg-config --libs-only-L openssl 2>/dev/null || echo "-L/opt/homebrew/opt/openssl@3/lib")
    LIBSODIUM_LDFLAGS=$(pkg-config --libs-only-L libsodium 2>/dev/null || echo "-L/opt/homebrew/lib")

    # Extract just the path (remove -L prefix) for rpath
    LIBOQS_RPATH=$(echo "$LIBOQS_LDFLAGS" | sed 's/-L//')
    OPENSSL_RPATH=$(echo "$OPENSSL_LDFLAGS" | sed 's/-L//')

    echo "  liboqs:    $LIBOQS_LDFLAGS"
    echo "  openssl:   $OPENSSL_LDFLAGS"
    echo "  libsodium: $LIBSODIUM_LDFLAGS"
else
    echo "  pkg-config not found, using Homebrew ARM64 defaults"
    LIBOQS_LDFLAGS="-L/opt/homebrew/Cellar/liboqs/0.14.0/lib"
    OPENSSL_LDFLAGS="-L/opt/homebrew/opt/openssl@3/lib"
    LIBSODIUM_LDFLAGS="-L/opt/homebrew/lib"
    LIBOQS_RPATH="/opt/homebrew/Cellar/liboqs/0.14.0/lib"
    OPENSSL_RPATH="/opt/homebrew/opt/openssl@3/lib"
fi

# Export for gprbuild
export LIBOQS_LDFLAGS OPENSSL_LDFLAGS LIBSODIUM_LDFLAGS

# Step 1: Compile Objective-C LAContext helpers
echo "Compiling LAContext Objective-C helpers..."
clang -c -O2 -mmacosx-version-min=26.0 \
    src/bindings/lacontext_helpers.m \
    -o obj/lacontext_helpers.o

if [ ! -f obj/lacontext_helpers.o ]; then
    echo "ERROR: Failed to compile lacontext_helpers.m"
    exit 1
fi
echo "✓ LAContext helpers compiled"

# Step 2: Run gprbuild for Ada/C sources
echo "Building Ada/C sources with gprbuild..."
gprbuild -p -P sparkpass.gpr || echo "Build failed (expected - will link manually)"

# Step 3: Link with LAContext object and Foundation framework
echo "Linking with LAContext support..."
cd obj
/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin/gcc \
    sparkpass_main.o b__sparkpass_main.o \
    bindings.o bindings-keychain_darwin.o bindings-liboqs.o bindings-libsodium.o \
    bindings-openssl.o bindings-posix.o bindings-termios.o \
    sparkpass.o sparkpass-cli.o sparkpass-config.o sparkpass-crypto.o \
    sparkpass-platform.o sparkpass-types.o bindings-lacontext_darwin.o \
    sparkpass-cli-password_input.o sparkpass-crypto-random.o sparkpass-crypto-zeroize.o \
    sparkpass-crypto-aes_gcm_siv.o sparkpass-crypto-argon2id.o sparkpass-crypto-hkdf.o \
    sparkpass-crypto-liboqs.o sparkpass-crypto-mldsa.o sparkpass-crypto-mlkem.o \
    sparkpass-crypto-nonce.o sparkpass-crypto-reedsolomon.o sparkpass-crypto-shamir.o \
    sparkpass-crypto-wrapping.o sparkpass-crypto-self_test.o \
    sparkpass-platform-keychain.o \
    sparkpass-vault-header.o sparkpass-vault-storage.o sparkpass-vault.o \
    sparkpass-vault-keyarena.o sparkpass-vault-policy.o sparkpass-cli-device.o \
    lacontext_helpers.o libsparkpass.a \
    $LIBOQS_LDFLAGS -loqs \
    $OPENSSL_LDFLAGS -lssl -lcrypto \
    $LIBSODIUM_LDFLAGS -lsodium \
    -framework CoreFoundation -framework Security -framework LocalAuthentication -framework Foundation \
    -Wl,-no_warn_duplicate_libraries -Wl,-stack_size,0x4000000 -mmacosx-version-min=26.0 \
    -L/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib/ \
    -static-libgcc \
    /Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib/libgnarl.a \
    /Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib/libgnat.a \
    -Wl,-rpath,$LIBOQS_RPATH \
    -Wl,-rpath,$OPENSSL_RPATH \
    -Wl,-rpath,@executable_path/..//obj \

# Deduplicate LC_RPATH entries
if [ -x scripts/rpath-dedupe.sh ]; then
  scripts/rpath-dedupe.sh bin/sparkpass_main || true
fi

    -Wl,-rpath,@executable_path/../..//.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib \
    -o ../bin/sparkpass_main

cd ..

if [ ! -f bin/sparkpass_main ]; then
    echo "ERROR: Failed to create binary"
    exit 1
fi

echo "✓ SparkPass built successfully with LAContext support"
echo ""
echo "Binary: bin/sparkpass_main"
ls -lh bin/sparkpass_main
