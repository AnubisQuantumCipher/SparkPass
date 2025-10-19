# Makefile for Argon2id test vectors
#
# This builds the standalone test program test_argon2id_vectors
# which validates the SparkPass Argon2id implementation against
# reference test vectors.

.PHONY: all clean run

# Detect GNAT installation
GNAT := $(shell which gnat 2>/dev/null || which gnatmake 2>/dev/null || which gprbuild 2>/dev/null)

all:
	@echo "================================================================"
	@echo "Building Argon2id Test Vector Validation"
	@echo "================================================================"
	@echo ""
ifeq ($(GNAT),)
	@echo "ERROR: No GNAT Ada compiler found!"
	@echo ""
	@echo "To build this test, you need GNAT (Ada compiler)."
	@echo "Install with:"
	@echo "  - macOS: brew install gcc (includes gnat)"
	@echo "  - Ubuntu: apt install gnat gprbuild"
	@echo ""
	@echo "Without compilation, the test vectors have been generated and"
	@echo "verified manually using the reference argon2-cffi library."
	@echo ""
	@echo "All 5 test vectors are available in:"
	@echo "  test_argon2id_vectors.adb"
	@echo ""
	@exit 1
else
	@echo "Using compiler: $(GNAT)"
	@echo ""
	cd .. && gprbuild -P test/test_argon2id.gpr -p
	@echo ""
	@echo "Build complete! Executable: test/bin/test_argon2id_vectors"
	@echo ""
endif

run: all
	@echo "================================================================"
	@echo "Running Argon2id Test Vector Validation"
	@echo "================================================================"
	@echo ""
	./bin/test_argon2id_vectors
	@echo ""

clean:
	@echo "Cleaning test build artifacts..."
	rm -rf obj bin
	@echo "Done."
