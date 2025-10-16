# SparkPass Makefile
# Convenient build, test, and release targets

.PHONY: all build clean sign notarize release verify clean-dist install test test-crypto test-mlkem-self test-mlkem-nist test-mldsa-nist test-vault-properties test-vault-corruption test-vault-fuzzer test-timing-attacks test-sidechannels help

# Version and architecture (can be overridden)
VER ?= 1.0.0
ARCH ?= $(shell uname -m)

# Paths
BINARY := bin/sparkpass_main
DIST_DIR := dist

# Default target
all: build

# Build SparkPass
build:
	@echo "=== Building SparkPass ==="
	./build.sh

# Clean build artifacts
clean:
	@echo "=== Cleaning build artifacts ==="
	rm -rf obj/*.o obj/*.ali $(BINARY)
	@echo "✓ Clean complete"

# Clean distribution files
clean-dist:
	@echo "=== Cleaning distribution directory ==="
	rm -rf $(DIST_DIR)
	@echo "✓ Distribution directory cleaned"

# Sign the built binary (without notarization)
sign: build
	@echo "=== Code Signing Only ==="
	@if [ ! -f $(BINARY) ]; then \
		echo "ERROR: Binary not found. Run 'make build' first."; \
		exit 1; \
	fi
	@echo "Signing binary with Hardened Runtime..."
	codesign --remove-signature $(BINARY) 2>/dev/null || true
	codesign --force \
		--timestamp \
		--options runtime \
		--entitlements signing/sparkpass.entitlements \
		--sign "Developer ID Application: AnubisQuantumCipher (E9VB3VKZKH)" \
		--verbose \
		$(BINARY)
	@echo "✓ Binary signed successfully"
	@echo ""
	@echo "Verifying signature..."
	@codesign --verify --deep --strict --verbose=2 $(BINARY)
	@echo "✓ Signature verified"

# Full sign and notarize workflow
notarize:
	@echo "=== Sign and Notarize (Version: $(VER), Arch: $(ARCH)) ==="
	@SPARKPASS_ARCH=$(ARCH) ./scripts/sign-and-notarize.sh $(VER)

# Complete release workflow (alias for notarize)
release: notarize
	@echo "✓ Release complete"

# Verify signatures and notarization
verify:
	@echo "=== Verifying Signatures ==="
	@if [ ! -f $(BINARY) ]; then \
		echo "ERROR: Binary not found: $(BINARY)"; \
		echo "Run 'make build' first."; \
		exit 1; \
	fi
	@./scripts/verify-signature.sh $(BINARY)
	@echo ""
	@if [ -f $(DIST_DIR)/sparkpass-$(VER)-installer.pkg ]; then \
		echo "=== Verifying PKG Installer ==="; \
		echo ""; \
		echo "PKG Signature:"; \
		pkgutil --check-signature $(DIST_DIR)/sparkpass-$(VER)-installer.pkg | head -10; \
		echo ""; \
		echo "Stapler Validation:"; \
		xcrun stapler validate $(DIST_DIR)/sparkpass-$(VER)-installer.pkg; \
		echo ""; \
		echo "Gatekeeper Assessment:"; \
		spctl -a -vvv -t install $(DIST_DIR)/sparkpass-$(VER)-installer.pkg 2>&1 | head -5; \
		echo ""; \
		echo "✓ PKG verification complete"; \
	else \
		echo "Note: PKG installer not found. Run 'make release' to create distribution packages."; \
	fi

# Install to /usr/local/bin (local testing only, unsigned)
install: build
	@echo "=== Installing SparkPass to /usr/local/bin ==="
	sudo cp bin/sparkpass_main /usr/local/bin/sparkpass
	sudo chmod 755 /usr/local/bin/sparkpass
	@echo "✓ Installed to /usr/local/bin/sparkpass"

# Run all tests (cryptographic + vault tests + timing tests + side-channel tests)
test: test-crypto test-vault-properties test-vault-corruption test-vault-fuzzer test-timing-attacks test-sidechannels
	@echo "=== All Tests Complete ==="

# Run complete cryptographic test suite
test-crypto: test-mlkem-self test-mlkem-nist test-mldsa-nist
	@echo "✓ Cryptographic test suite complete"

# Test ML-KEM-1024 self-consistency (internal validation)
test-mlkem-self:
	@echo "=== Testing ML-KEM-1024 (Self-Consistency) ==="
	@if [ ! -f test/test_mlkem_kat ]; then \
		echo "Compiling ML-KEM self-test..."; \
		PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:/usr/bin:/bin:$$PATH" \
		gnatmake -Iobj -Isrc/sparkpass -Isrc/sparkpass/vault -Isrc/sparkpass/crypto -Isrc/crypto -Isrc/vault -Isrc/bindings -Isrc/sparkpass/cli -Isrc/sparkpass/platform \
		-gnat2020 -o test/test_mlkem_kat test/test_mlkem_kat.adb \
		-largs obj/libsparkpass.a -L/opt/homebrew/lib -loqs -lssl -lcrypto -lsodium \
		-framework CoreFoundation -framework Security -framework LocalAuthentication -framework Foundation 2>&1 | grep -v "warning:" || true; \
	fi
	@test/test_mlkem_kat

# Test ML-KEM-1024 with official NIST FIPS 203 vectors
test-mlkem-nist:
	@echo "=== Testing ML-KEM-1024 (NIST FIPS 203 KAT) ==="
	@if [ ! -f test/test_mlkem_nist ]; then \
		echo "✗ ERROR: Test binary not found."; \
		echo "  Run: make build && compile test manually"; \
		exit 1; \
	fi
	@cd test && ./test_mlkem_nist

# Test ML-DSA-87 with official NIST FIPS 204 vectors
test-mldsa-nist:
	@echo "=== Testing ML-DSA-87 (NIST FIPS 204 KAT) ==="
	@if [ ! -f test/test_mldsa_nist ]; then \
		echo "✗ ERROR: Test binary not found."; \
		echo "  Run: make build && compile test manually"; \
		exit 1; \
	fi
	@cd test && ./test_mldsa_nist

# Test vault state machine properties
test-vault-properties:
	@echo "=== Testing Vault State Machine Properties ==="
	@if [ ! -f test/test_vault_properties ]; then \
		echo "Compiling property-based tests..."; \
		PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:/usr/bin:/bin:$$PATH" \
		gnatmake -Iobj -Isrc/sparkpass -Isrc/sparkpass/vault -Isrc/sparkpass/crypto -Isrc/vault -Isrc/bindings -Isrc/sparkpass/cli -Isrc/sparkpass/platform \
		-gnat2020 -o test/test_vault_properties test/test_vault_properties.adb \
		-largs obj/libsparkpass.a obj/lacontext_helpers.o -L/opt/homebrew/lib -loqs -lssl -lcrypto -lsodium \
		-framework CoreFoundation -framework Security -framework LocalAuthentication -framework Foundation \
		-Wl,-stack_size,0x4000000 2>&1 | grep -v "warning:" || true; \
	fi
	@test/test_vault_properties

# Test vault corruption resilience
test-vault-corruption:
	@echo "=== Testing Vault Corruption Resilience ==="
	@if [ ! -f test/test_vault_corruption ]; then \
		echo "Compiling corruption injection tests..."; \
		PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:/usr/bin:/bin:$$PATH" \
		gnatmake -Iobj -Isrc/sparkpass -Isrc/sparkpass/vault -Isrc/sparkpass/crypto -Isrc/vault -Isrc/bindings -Isrc/sparkpass/cli -Isrc/sparkpass/platform \
		-gnat2020 -o test/test_vault_corruption test/test_vault_corruption.adb \
		-largs obj/libsparkpass.a obj/lacontext_helpers.o -L/opt/homebrew/lib -loqs -lssl -lcrypto -lsodium \
		-framework CoreFoundation -framework Security -framework LocalAuthentication -framework Foundation \
		-Wl,-stack_size,0x4000000 2>&1 | grep -v "warning:" || true; \
	fi
	@test/test_vault_corruption

# Test vault file format fuzzer
test-vault-fuzzer:
	@echo "=== Testing Vault File Format Fuzzing ==="
	@if [ ! -f test/test_vault_fuzzer ]; then \
		echo "Compiling vault fuzzer..."; \
		PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:/usr/bin:/bin:$$PATH" \
		gnatmake -Iobj -Isrc/sparkpass -Isrc/sparkpass/vault -Isrc/sparkpass/crypto -Isrc/vault -Isrc/bindings -Isrc/sparkpass/cli -Isrc/sparkpass/platform \
		-gnat2020 -o test/test_vault_fuzzer test/test_vault_fuzzer.adb \
		-largs obj/libsparkpass.a obj/lacontext_helpers.o -L/opt/homebrew/lib -loqs -lssl -lcrypto -lsodium \
		-framework CoreFoundation -framework Security -framework LocalAuthentication -framework Foundation \
		-Wl,-stack_size,0x4000000 2>&1 | grep -v "warning:" || true; \
	fi
	@test/test_vault_fuzzer

# Test timing attack resistance (constant-time operations)
test-timing-attacks:
	@echo "=== Testing Timing Attack Resistance ==="
	@if [ ! -f test/test_timing_attacks ]; then \
		echo "Compiling timing attack tests..."; \
		PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:/usr/bin:/bin:$$PATH" \
		gnatmake -Iobj -Isrc/sparkpass -Isrc/sparkpass/vault -Isrc/sparkpass/crypto -Isrc/vault -Isrc/bindings -Isrc/sparkpass/cli -Isrc/sparkpass/platform \
		-gnat2020 -o test/test_timing_attacks test/test_timing_attacks.adb \
		-largs obj/libsparkpass.a obj/lacontext_helpers.o -L/opt/homebrew/lib -loqs -lssl -lcrypto -lsodium \
		-framework CoreFoundation -framework Security -framework LocalAuthentication -framework Foundation \
		-Wl,-stack_size,0x4000000 2>&1 | grep -v "warning:" || true; \
	fi
	@test/test_timing_attacks

# Test side-channel resistance (cache, branch, memory, speculative execution)
test-sidechannels:
	@echo "=== Testing Side-Channel Resistance ==="
	@if [ ! -f test/test_sidechannels ]; then \
		echo "Compiling side-channel analysis tests..."; \
		PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:/usr/bin:/bin:$$PATH" \
		gnatmake -Iobj -Isrc/sparkpass -Isrc/sparkpass/vault -Isrc/sparkpass/crypto -Isrc/vault -Isrc/bindings -Isrc/sparkpass/cli -Isrc/sparkpass/platform \
		-gnat2020 -o test/test_sidechannels test/test_sidechannels.adb \
		-largs obj/libsparkpass.a obj/lacontext_helpers.o -L/opt/homebrew/lib -loqs -lssl -lcrypto -lsodium \
		-framework CoreFoundation -framework Security -framework LocalAuthentication -framework Foundation \
		-Wl,-stack_size,0x4000000 2>&1 | grep -v "warning:" || true; \
	fi
	@test/test_sidechannels

# Display help
help:
	@echo "SparkPass Makefile Targets:"
	@echo ""
	@echo "BUILD TARGETS:"
	@echo "  make build            - Build SparkPass binary"
	@echo "  make clean            - Remove build artifacts (keeps dist/)"
	@echo "  make clean-dist       - Remove distribution directory"
	@echo "  make install          - Install unsigned binary to /usr/local/bin (dev only)"
	@echo ""
	@echo "RELEASE TARGETS:"
	@echo "  make sign             - Sign binary with Hardened Runtime (no notarization)"
	@echo "  make notarize         - Build, sign, notarize, and staple (complete workflow)"
	@echo "  make release          - Alias for 'make notarize'"
	@echo "  make verify           - Verify signatures and notarization"
	@echo ""
	@echo "TEST TARGETS:"
	@echo "  make test                    - Run all test suites (crypto + vault + timing + side-channels)"
	@echo "  make test-crypto             - Run cryptographic test suite (self + NIST)"
	@echo "  make test-mlkem-self         - Test ML-KEM-1024 self-consistency"
	@echo "  make test-mlkem-nist         - Test ML-KEM-1024 NIST FIPS 203 compliance"
	@echo "  make test-mldsa-nist         - Test ML-DSA-87 NIST FIPS 204 compliance"
	@echo "  make test-vault-properties   - Test vault state machine invariants"
	@echo "  make test-vault-corruption   - Test vault corruption resilience"
	@echo "  make test-vault-fuzzer       - Test vault file format fuzzing (1000 mutations)"
	@echo "  make test-timing-attacks     - Test timing attack resistance (constant-time ops)"
	@echo "  make test-sidechannels       - Test side-channel resistance (cache, branch, memory, spectre)"
	@echo ""
	@echo "VARIABLES:"
	@echo "  VER=1.0.0             - Version number for release (default: 1.0.0)"
	@echo "  ARCH=arm64            - Architecture (default: auto-detected with uname -m)"
	@echo ""
	@echo "EXAMPLES:"
	@echo "  make build"
	@echo "  make sign"
	@echo "  make release VER=1.0.0"
	@echo "  make release VER=1.1.0 ARCH=x86_64"
	@echo "  make verify"
	@echo "  make test"
	@echo ""
	@echo "RELEASE WORKFLOW:"
	@echo "  1. make build          # Build and test locally"
	@echo "  2. make test           # Run full test suite"
	@echo "  3. make release VER=1.0.0  # Sign, notarize, staple"
	@echo "  4. make verify         # Verify all signatures"
	@echo "  5. Upload dist/*.zip and dist/*.pkg to GitHub Release"
	@echo ""
	@echo "For detailed documentation, see NOTARIZATION.md"
