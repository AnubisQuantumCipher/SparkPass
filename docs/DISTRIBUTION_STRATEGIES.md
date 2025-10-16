# SparkPass Distribution Strategies

## Overview

SparkPass depends on Homebrew libraries (liboqs, openssl@3, libsodium) which complicates distribution. This document outlines strategies for distributing SparkPass to end users.

## Current Linking Model

SparkPass uses **dynamic linking** to Homebrew libraries:

```bash
$ otool -L bin/sparkpass_main
/opt/homebrew/opt/openssl@3/lib/libssl.3.dylib
/opt/homebrew/opt/openssl@3/lib/libcrypto.3.dylib
/opt/homebrew/opt/libsodium/lib/libsodium.26.dylib
```

This means users must have these Homebrew packages installed.

## Distribution Strategy 1: Homebrew Installation (RECOMMENDED)

**Best approach for CLI tools with dependencies**

### Advantages
- Handles all dependencies automatically
- Users familiar with Homebrew expect this workflow
- Automatic updates via `brew upgrade`
- Properly integrated into macOS ecosystem

### Implementation

Create a Homebrew formula that:
1. Downloads signed/notarized SparkPass binary
2. Declares dependencies on liboqs, openssl@3, libsodium
3. Installs to `/opt/homebrew/bin/sparkpass` (or `/usr/local/bin`)

**Example Formula** (sparkpass.rb):

```ruby
class Sparkpass < Formula
  desc "Quantum-resistant password manager with Touch ID support"
  homepage "https://github.com/yourusername/sparkpass"
  url "https://github.com/yourusername/sparkpass/releases/download/v1.0.0/sparkpass-1.0.0-macos-arm64.tar.gz"
  sha256 "YOUR_SHA256_CHECKSUM"
  license "YOUR_LICENSE"

  depends_on "liboqs"
  depends_on "openssl@3"
  depends_on "libsodium"
  depends_on arch: :arm64
  depends_on :macos => :big_sur

  def install
    bin.install "sparkpass"
  end

  test do
    system "#{bin}/sparkpass", "--version"
  end
end
```

**Tap Creation:**

```bash
# Create your own tap
gh repo create homebrew-sparkpass --public
git clone https://github.com/yourusername/homebrew-sparkpass
cd homebrew-sparkpass
mkdir Formula
cp sparkpass.rb Formula/

# Users install with:
brew tap yourusername/sparkpass
brew install sparkpass
```

### User Installation Steps

```bash
# 1. Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# 2. Add your tap
brew tap yourusername/sparkpass

# 3. Install SparkPass (dependencies installed automatically)
brew install sparkpass

# 4. Use SparkPass
sparkpass --help
```

## Distribution Strategy 2: Bundle Dependencies (Static Linking)

**For standalone distribution without Homebrew requirement**

### Advantages
- Single binary distribution
- No external dependencies required
- Works on systems without Homebrew

### Disadvantages
- Larger binary size
- Must rebuild dependencies as static libraries
- More complex build process
- Security updates require full rebuild

### Implementation

Rebuild dependencies as static libraries:

```bash
# Build static liboqs
cd /tmp
git clone https://github.com/open-quantum-safe/liboqs.git
cd liboqs
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release \
      -DBUILD_SHARED_LIBS=OFF \
      -DCMAKE_INSTALL_PREFIX=/usr/local/sparkpass-deps \
      ..
make && sudo make install

# Similar for openssl and libsodium
# Then update sparkpass.gpr to link statically
```

Update `sparkpass.gpr`:

```ada
package Linker is
   for Default_Switches ("Ada") use (
      "/usr/local/sparkpass-deps/lib/liboqs.a",
      "/usr/local/sparkpass-deps/lib/libssl.a",
      "/usr/local/sparkpass-deps/lib/libcrypto.a",
      "/usr/local/sparkpass-deps/lib/libsodium.a",
      "-framework", "CoreFoundation",
      "-framework", "Security",
      "-framework", "LocalAuthentication",
      "-framework", "Foundation",
      "-Wl,-stack_size,0x4000000"
   );
end Linker;
```

## Distribution Strategy 3: Bundle Dynamic Libraries

**Package Homebrew dylibs with SparkPass**

### Advantages
- Standalone package (no Homebrew needed)
- Smaller than static linking
- Can be signed and notarized as a bundle

### Disadvantages
- Must bundle and sign each dylib
- Complex code signing (must sign dependencies too)
- Rpath management required

### Implementation

```bash
#!/bin/bash
# Bundle Homebrew libraries with SparkPass

DIST_DIR="dist/sparkpass-bundle"
mkdir -p "$DIST_DIR/lib"

# Copy SparkPass binary
cp bin/sparkpass_main "$DIST_DIR/sparkpass"

# Copy required dylibs
cp /opt/homebrew/opt/openssl@3/lib/libssl.3.dylib "$DIST_DIR/lib/"
cp /opt/homebrew/opt/openssl@3/lib/libcrypto.3.dylib "$DIST_DIR/lib/"
cp /opt/homebrew/opt/libsodium/lib/libsodium.26.dylib "$DIST_DIR/lib/"
cp /opt/homebrew/opt/liboqs/lib/liboqs.5.dylib "$DIST_DIR/lib/"

# Sign each dylib
for dylib in "$DIST_DIR"/lib/*.dylib; do
    codesign --sign "$SIGNING_IDENTITY" \
        --options runtime \
        --timestamp \
        --force \
        "$dylib"
done

# Update rpaths in binary to use bundled libs
install_name_tool -add_rpath "@executable_path/lib" "$DIST_DIR/sparkpass"

# Update dylib paths in binary
install_name_tool -change \
    /opt/homebrew/opt/openssl@3/lib/libssl.3.dylib \
    @rpath/libssl.3.dylib \
    "$DIST_DIR/sparkpass"

# Repeat for all dependencies...

# Sign the modified binary
codesign --sign "$SIGNING_IDENTITY" \
    --options runtime \
    --entitlements signing/sparkpass.entitlements \
    --force \
    --timestamp \
    "$DIST_DIR/sparkpass"

# Create distribution archive
cd dist
tar -czf sparkpass-bundle-1.0.0.tar.gz sparkpass-bundle/
```

## Distribution Strategy 4: Docker/Containerization

**For cross-platform consistency (not typical for macOS CLI)**

Not recommended for SparkPass due to:
- Touch ID/Face ID requires native macOS execution
- Container overhead unnecessary for CLI tool
- Users expect native binaries on macOS

## Distribution Strategy 5: App Bundle (.app)

**Wrap as macOS application bundle**

### Advantages
- Familiar macOS distribution format
- Can bundle all dependencies in Frameworks folder
- DMG distribution feels native

### Disadvantages
- Overhead for a CLI tool
- Users still need Terminal to use it
- More complex than necessary

### Implementation

Create bundle structure:

```
SparkPass.app/
├── Contents/
│   ├── Info.plist
│   ├── MacOS/
│   │   └── sparkpass (wrapper script)
│   ├── Frameworks/
│   │   ├── libssl.3.dylib
│   │   ├── libcrypto.3.dylib
│   │   └── libsodium.26.dylib
│   └── Resources/
│       └── sparkpass_main (actual binary)
```

## Recommended Approach for SparkPass

**Use Strategy 1: Homebrew Distribution**

### Rationale
1. **User Expectations**: Users of CLI security tools typically use Homebrew
2. **Dependency Management**: Homebrew handles updates for liboqs, openssl, etc.
3. **Security Updates**: Users get security patches via `brew upgrade`
4. **Simplicity**: No rpath manipulation or library bundling
5. **Standard Practice**: Matches distribution of similar tools (age, pass, etc.)

### Installation Instructions for Users

```bash
# Install via Homebrew
brew tap yourusername/sparkpass
brew install sparkpass

# Verify installation
sparkpass --version

# First run setup
sparkpass init
```

### Fallback: Manual Installation

For users without Homebrew or in enterprise environments:

```bash
# 1. Install dependencies
brew install liboqs openssl@3 libsodium

# 2. Download signed binary
curl -LO https://github.com/yourusername/sparkpass/releases/download/v1.0.0/sparkpass-1.0.0-macos-arm64.tar.gz

# 3. Verify checksum
shasum -a 256 -c sparkpass-1.0.0-checksums.txt

# 4. Extract and install
tar -xzf sparkpass-1.0.0-macos-arm64.tar.gz
sudo cp sparkpass-1.0.0/sparkpass /usr/local/bin/
sudo chmod 755 /usr/local/bin/sparkpass

# 5. Verify code signature
codesign --verify --verbose /usr/local/bin/sparkpass
spctl --assess --type execute --verbose /usr/local/bin/sparkpass
```

## Code Signing Considerations

### Entitlements for Bundled Libraries

If you bundle dylibs (Strategy 3), each library must be signed:

```bash
# Sign each dylib
codesign --sign "$IDENTITY" --timestamp --force lib/libssl.3.dylib
codesign --sign "$IDENTITY" --timestamp --force lib/libcrypto.3.dylib
codesign --sign "$IDENTITY" --timestamp --force lib/libsodium.26.dylib
```

The main binary's entitlements must include:

```xml
<key>com.apple.security.cs.disable-library-validation</key>
<true/>
```

This allows loading of your signed libraries that don't match your Team ID.

### Hardened Runtime with Dynamic Libraries

Apple's Hardened Runtime can restrict dylib loading. The entitlements file handles this:

```xml
<!-- Allow loading of Homebrew dylibs -->
<key>com.apple.security.cs.allow-dyld-environment-variables</key>
<true/>
<key>com.apple.security.cs.disable-library-validation</key>
<true/>
```

**Security Note**: `disable-library-validation` allows loading of unsigned/differently-signed libraries. This is safe for SparkPass because:
1. We control the Homebrew dependency versions
2. Users install via trusted channels (Homebrew, GitHub Releases)
3. The main binary is signed and verified by Gatekeeper

## Testing Distribution Packages

### Test on Clean System

```bash
# Simulate fresh install
# 1. Create new macOS VM or use a test Mac
# 2. DO NOT install development tools
# 3. Only install Homebrew
# 4. Install SparkPass via your distribution method
# 5. Verify it runs without errors
```

### Test Notarization Ticket

```bash
# Download your distribution
curl -LO https://github.com/yourusername/sparkpass/releases/download/v1.0.0/sparkpass-1.0.0-macos-arm64.tar.gz

# Extract
tar -xzf sparkpass-1.0.0-macos-arm64.tar.gz

# Set quarantine bit (simulates download from internet)
xattr -w com.apple.quarantine "0081;$(date +%s);Safari;UUID" sparkpass-1.0.0/sparkpass

# Try to run - should work without warnings if notarized
./sparkpass-1.0.0/sparkpass --help
```

## Continuous Distribution

### GitHub Actions Workflow

See `GITHUB_ACTIONS.md` for automated build, sign, notarize, and release workflow.

### Automated Homebrew Tap Updates

Use `brew bump-formula-pr` to automate formula updates:

```bash
# After creating a new release
brew bump-formula-pr \
  --url=https://github.com/yourusername/sparkpass/archive/v1.0.1.tar.gz \
  --sha256=NEW_CHECKSUM \
  sparkpass
```

## Conclusion

For SparkPass, **Homebrew distribution (Strategy 1)** provides the best balance of:
- User experience (familiar installation method)
- Security (signed, notarized, with proper dependency management)
- Maintainability (updates via Homebrew)
- Simplicity (no complex bundling or static linking)

The signing and notarization workflow ensures that users can trust SparkPass downloads, while Homebrew handles the complexity of dependency management.
