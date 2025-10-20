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
