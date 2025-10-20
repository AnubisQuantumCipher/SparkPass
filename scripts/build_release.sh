#!/usr/bin/env bash
#
# Reproducible Build Script for SparkPass
# Generates deterministic binaries with checksums for release
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
PROJECT_NAME="sparkpass"
VERSION=$(grep '^version = ' alire.toml | cut -d'"' -f2)
PLATFORM=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)
BUILD_DIR="dist"
RELEASE_NAME="${PROJECT_NAME}-v${VERSION}-${PLATFORM}-${ARCH}"

echo -e "${GREEN}=== SparkPass Reproducible Build ===${NC}"
echo "Version: ${VERSION}"
echo "Platform: ${PLATFORM}"
echo "Architecture: ${ARCH}"
echo

# Clean previous builds
echo -e "${YELLOW}Cleaning previous build artifacts...${NC}"
rm -rf obj/ bin/ "${BUILD_DIR}/"
mkdir -p "${BUILD_DIR}"

# Set up Alire environment
echo -e "${YELLOW}Setting up Alire environment...${NC}"
if ! command -v alr &> /dev/null; then
    echo -e "${RED}Error: Alire not found. Please install from https://alire.ada.dev${NC}"
    exit 1
fi

# Verify toolchain
echo -e "${YELLOW}Verifying toolchain versions...${NC}"
alr toolchain --select gnat_native=14.2.1 gprbuild=24.0.1

# Compile Objective-C helper (macOS-specific)
if [ "${PLATFORM}" = "darwin" ]; then
    echo -e "${YELLOW}Compiling macOS Objective-C bindings...${NC}"
    mkdir -p obj
    clang -c -o obj/lacontext_helpers.o src/bindings/lacontext_helpers.m \
        -framework Foundation -framework LocalAuthentication
fi

# Build with Alire (ensures reproducible dependencies)
echo -e "${YELLOW}Building SparkPass with Alire...${NC}"
alr build --release

# Verify binary was created
if [ ! -f "bin/sparkpass_main" ]; then
    echo -e "${RED}Error: Binary not created${NC}"
    exit 1
fi

# Strip debug symbols for smaller binary (optional)
echo -e "${YELLOW}Stripping debug symbols...${NC}"
strip bin/sparkpass_main

# Fix RPATH issues for standalone binary (macOS)
if [ "${PLATFORM}" = "darwin" ]; then
    echo -e "${YELLOW}Fixing RPATH for standalone binary...${NC}"

    # Get all RPATHs
    RPATHS=$(otool -l bin/sparkpass_main | grep -A2 LC_RPATH | grep path | awk '{print $2}')

    # Remove all toolchain-specific RPATHs
    for rpath in $RPATHS; do
        if [[ "$rpath" == *"alire"* ]] || [[ "$rpath" == *"toolchains"* ]]; then
            echo "  Removing RPATH: $rpath"
            install_name_tool -delete_rpath "$rpath" bin/sparkpass_main 2>/dev/null || true
        fi
    done

    # Verify system libraries are accessible
    echo -e "${YELLOW}Verifying library dependencies...${NC}"
    otool -L bin/sparkpass_main | grep -v ":" | while read -r lib rest; do
        if [[ "$lib" == /opt/homebrew/* ]]; then
            echo "  Warning: Homebrew dependency: $lib"
        elif [[ "$lib" == /usr/local/* ]]; then
            echo "  Warning: /usr/local dependency: $lib"
        fi
    done
fi

# Test binary functionality BEFORE packaging
echo -e "${YELLOW}Testing binary functionality...${NC}"
TEST_OUTPUT=$(./bin/sparkpass_main --version 2>&1)
if [ $? -ne 0 ]; then
    echo -e "${RED}Error: Binary execution failed${NC}"
    echo "Output: $TEST_OUTPUT"
    exit 1
fi

# Check for version string in output
if ! echo "$TEST_OUTPUT" | grep -q "SparkPass version"; then
    echo -e "${RED}Error: Binary did not produce version output${NC}"
    echo "Output: $TEST_OUTPUT"
    exit 1
fi

echo -e "${GREEN}Binary test passed: --version works${NC}"

# Quick init test with timeout
echo -e "${YELLOW}Testing vault initialization (30s timeout)...${NC}"
TEST_VAULT="/tmp/sparkpass_build_test_$$.spass"
rm -f "$TEST_VAULT"

# Run with timeout and capture output
if timeout 30 bash -c "echo -e 'test_password_12345\ntest_password_12345' | ./bin/sparkpass_main init '$TEST_VAULT' 2>&1" > /tmp/init_test.log; then
    if [ -f "$TEST_VAULT" ]; then
        echo -e "${GREEN}Binary test passed: vault init works${NC}"
        rm -f "$TEST_VAULT"
    else
        echo -e "${RED}Warning: init command succeeded but vault file not created${NC}"
        cat /tmp/init_test.log
    fi
else
    echo -e "${RED}Error: Binary init test failed or timed out${NC}"
    echo "Last 20 lines of output:"
    tail -20 /tmp/init_test.log
    exit 1
fi

rm -f /tmp/init_test.log

# Create distribution directory
DIST_PATH="${BUILD_DIR}/${RELEASE_NAME}"
mkdir -p "${DIST_PATH}"

# Copy binary
cp bin/sparkpass_main "${DIST_PATH}/sparkpass"
chmod +x "${DIST_PATH}/sparkpass"

# Copy documentation
cp README.md "${DIST_PATH}/"
cp LICENSE "${DIST_PATH}/"
mkdir -p "${DIST_PATH}/docs"
cp -r docs/FORMAL_VERIFICATION.md "${DIST_PATH}/docs/"

# Create tarball
echo -e "${YELLOW}Creating release tarball...${NC}"
cd "${BUILD_DIR}"
tar -czf "${RELEASE_NAME}.tar.gz" "${RELEASE_NAME}"
cd ..

# Generate checksums
echo -e "${YELLOW}Generating checksums...${NC}"
cd "${BUILD_DIR}"
shasum -a 256 "${RELEASE_NAME}.tar.gz" > "${RELEASE_NAME}.tar.gz.sha256"
cd ..

# Display results
echo
echo -e "${GREEN}=== Build Complete ===${NC}"
echo "Release package: ${BUILD_DIR}/${RELEASE_NAME}.tar.gz"
echo "SHA256 checksum: ${BUILD_DIR}/${RELEASE_NAME}.tar.gz.sha256"
echo
echo "Checksum:"
cat "${BUILD_DIR}/${RELEASE_NAME}.tar.gz.sha256"
echo
echo -e "${GREEN}Binary is ready for GitHub release${NC}"
