#!/bin/bash
#
# SparkPass Automated Smoke Test Suite
#
# Tests core functionality: create, unlock, add, get, list, remove
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SPARKPASS="$PROJECT_ROOT/bin/sparkpass_main"
TEST_VAULT="$PROJECT_ROOT/vaults/smoke_test.spass"
TEST_PASSWORD="smoke_test_password_12345"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_header() {
    echo ""
    echo -e "${BLUE}===${NC} $1 ${BLUE}===${NC}"
}

print_test() {
    echo -e "${YELLOW}→${NC} $1"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

# Test counter
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

run_test() {
    local test_name="$1"
    TESTS_RUN=$((TESTS_RUN + 1))
    print_test "Test $TESTS_RUN: $test_name"
}

test_passed() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    print_success "PASSED"
}

test_failed() {
    local reason="$1"
    TESTS_FAILED=$((TESTS_FAILED + 1))
    print_error "FAILED: $reason"
}

cleanup() {
    rm -f "$TEST_VAULT"
    security delete-generic-password -s "com.sparkpass.vault" -a "$TEST_VAULT" 2>/dev/null || true
}

# Start tests
print_header "SparkPass Smoke Test Suite"
echo ""
echo "Binary: $SPARKPASS"
echo "Vault: $TEST_VAULT"
echo "Password: $TEST_PASSWORD"

# Clean up any existing test data
cleanup

# Verify binary exists
if [ ! -f "$SPARKPASS" ]; then
    print_error "Binary not found: $SPARKPASS"
    echo "Run './scripts/build-and-sign.sh' first"
    exit 1
fi

print_success "Binary found"

# Test 1: Version check
print_header "Test 1: Version Check"
run_test "sparkpass_main --version"

if OUTPUT=$($SPARKPASS --version 2>&1); then
    if echo "$OUTPUT" | grep -q "SparkPass version"; then
        test_passed
        echo "  Output: $(echo "$OUTPUT" | head -1)"
    else
        test_failed "Unexpected version output"
        echo "$OUTPUT"
    fi
else
    test_failed "Command failed"
fi

# Test 2: Create vault
print_header "Test 2: Create Vault"
run_test "sparkpass_main init $TEST_VAULT"

if OUTPUT=$(printf "%s\n%s\n" "$TEST_PASSWORD" "$TEST_PASSWORD" | $SPARKPASS init "$TEST_VAULT" 2>&1); then
    if echo "$OUTPUT" | grep -q "vault initialized"; then
        test_passed
        if [ -f "$TEST_VAULT" ]; then
            echo "  Vault created: $TEST_VAULT"
            echo "  Size: $(du -h "$TEST_VAULT" | cut -f1)"
            echo "  Permissions: $(stat -f "%Sp" "$TEST_VAULT" 2>/dev/null || stat -c "%a" "$TEST_VAULT" 2>/dev/null)"
        fi
    else
        test_failed "No success message in output"
        echo "$OUTPUT"
    fi
else
    test_failed "Init command failed"
    echo "$OUTPUT"
fi

# Test 3: Unlock with password
print_header "Test 3: Unlock with Password"
run_test "sparkpass_main unlock $TEST_VAULT"

START_TIME=$(date +%s)
if OUTPUT=$(echo "$TEST_PASSWORD" | $SPARKPASS unlock "$TEST_VAULT" 2>&1); then
    END_TIME=$(date +%s)
    DURATION=$((END_TIME - START_TIME))

    if echo "$OUTPUT" | grep -q "password accepted"; then
        test_passed
        echo "  Duration: ${DURATION}s (expected ~2-3s for Argon2id)"
        if echo "$OUTPUT" | grep -q "biometric unlock enabled"; then
            echo "  Biometric cache: ENABLED ✓"
        else
            echo "  Biometric cache: NOT ENABLED (expected if Touch ID not enrolled)"
        fi
    else
        test_failed "No success message"
        echo "$OUTPUT"
    fi
else
    test_failed "Unlock failed"
    echo "$OUTPUT"
fi

# Test 4: Add entry
print_header "Test 4: Add Entry"
run_test "sparkpass_main add $TEST_VAULT test_label"

if OUTPUT=$(printf "test_secret_value\n%s\n" "$TEST_PASSWORD" | $SPARKPASS add "$TEST_VAULT" test_label 2>&1); then
    if echo "$OUTPUT" | grep -q "entry added"; then
        test_passed
    else
        test_failed "No success message"
        echo "$OUTPUT"
    fi
else
    test_failed "Add command failed"
    echo "$OUTPUT"
fi

# Test 5: List entries
print_header "Test 5: List Entries"
run_test "sparkpass_main ls $TEST_VAULT"

if OUTPUT=$(echo "$TEST_PASSWORD" | $SPARKPASS ls "$TEST_VAULT" 2>&1); then
    if echo "$OUTPUT" | grep -q "test_label"; then
        test_passed
        echo "  Entries found: $(echo "$OUTPUT" | grep -c "test_label" || echo "0")"
    else
        test_failed "Label not found in list"
        echo "$OUTPUT"
    fi
else
    test_failed "List command failed"
    echo "$OUTPUT"
fi

# Test 6: Get entry
print_header "Test 6: Get Entry"
run_test "sparkpass_main get $TEST_VAULT test_label"

if OUTPUT=$(printf "%s\ny\n" "$TEST_PASSWORD" | $SPARKPASS get "$TEST_VAULT" test_label 2>&1); then
    if echo "$OUTPUT" | grep -q "test_secret_value"; then
        test_passed
    else
        test_failed "Secret value not retrieved"
        echo "$OUTPUT"
    fi
else
    test_failed "Get command failed"
    echo "$OUTPUT"
fi

# Test 7: Add second entry
print_header "Test 7: Add Second Entry"
run_test "sparkpass_main add $TEST_VAULT github"

if OUTPUT=$(printf "ghp_test_token_12345\n%s\n" "$TEST_PASSWORD" | $SPARKPASS add "$TEST_VAULT" github 2>&1); then
    if echo "$OUTPUT" | grep -q "entry added"; then
        test_passed
    else
        test_failed "No success message"
        echo "$OUTPUT"
    fi
else
    test_failed "Add command failed"
    echo "$OUTPUT"
fi

# Test 8: Verify multiple entries
print_header "Test 8: Verify Multiple Entries"
run_test "sparkpass_main ls $TEST_VAULT (should show 2 entries)"

if OUTPUT=$(echo "$TEST_PASSWORD" | $SPARKPASS ls "$TEST_VAULT" 2>&1); then
    local count=$(echo "$OUTPUT" | grep -E "(test_label|github)" | wc -l)
    if [ "$count" -ge 2 ]; then
        test_passed
        echo "  Entries found: $count"
    else
        test_failed "Expected 2 entries, found $count"
        echo "$OUTPUT"
    fi
else
    test_failed "List command failed"
    echo "$OUTPUT"
fi

# Test 9: Remove entry
print_header "Test 9: Remove Entry"
run_test "sparkpass_main rm $TEST_VAULT test_label"

if OUTPUT=$(echo "$TEST_PASSWORD" | $SPARKPASS rm "$TEST_VAULT" test_label 2>&1); then
    if echo "$OUTPUT" | grep -q "entry removed"; then
        test_passed
    else
        test_failed "No success message"
        echo "$OUTPUT"
    fi
else
    test_failed "Remove command failed"
    echo "$OUTPUT"
fi

# Test 10: Verify removal
print_header "Test 10: Verify Entry Removed"
run_test "sparkpass_main ls $TEST_VAULT (should show 1 entry)"

if OUTPUT=$(echo "$TEST_PASSWORD" | $SPARKPASS ls "$TEST_VAULT" 2>&1); then
    if echo "$OUTPUT" | grep -q "github" && ! echo "$OUTPUT" | grep -q "test_label"; then
        test_passed
        echo "  'github' still exists, 'test_label' removed"
    else
        test_failed "Entry removal not verified"
        echo "$OUTPUT"
    fi
else
    test_failed "List command failed"
    echo "$OUTPUT"
fi

# Test 11: Doctor command
print_header "Test 11: Doctor Command"
run_test "sparkpass_main doctor $TEST_VAULT"

if OUTPUT=$($SPARKPASS doctor "$TEST_VAULT" 2>&1); then
    if echo "$OUTPUT" | grep -q "Vault fingerprint"; then
        test_passed
        echo "  Vault fingerprint found"
    else
        test_failed "No fingerprint in output"
        echo "$OUTPUT"
    fi
else
    test_failed "Doctor command failed"
    echo "$OUTPUT"
fi

# Test 12: Wrong password
print_header "Test 12: Wrong Password Rejection"
run_test "sparkpass_main unlock $TEST_VAULT (wrong password)"

if OUTPUT=$(echo "wrong_password_123" | $SPARKPASS unlock "$TEST_VAULT" 2>&1); then
    if echo "$OUTPUT" | grep -q "authentication failed"; then
        test_passed
        echo "  Wrong password correctly rejected"
    else
        test_failed "Wrong password was accepted!"
        echo "$OUTPUT"
    fi
else
    # Command should fail with wrong password
    if echo "$?" | grep -q "1"; then
        test_passed
        echo "  Wrong password correctly rejected"
    else
        test_failed "Unexpected error"
    fi
fi

# Summary
print_header "Test Summary"
echo ""
echo "Tests run:    $TESTS_RUN"
echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    print_success "ALL TESTS PASSED!"
    echo ""
    echo "SparkPass is working correctly."
    echo ""
    echo "Next steps:"
    echo "  - Run performance benchmark: ./scripts/benchmark.sh"
    echo "  - Read deployment guide: DEPLOYMENT.md"
    echo "  - Test on Mac with Touch ID enrolled"
else
    print_error "SOME TESTS FAILED"
    echo ""
    echo "Review the failed tests above for details."
    exit 1
fi

# Cleanup
print_header "Cleanup"
cleanup
print_success "Test vault removed"

exit 0
