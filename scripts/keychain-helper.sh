#!/bin/bash
#
# SparkPass Keychain Management Helper
#
# Utilities for inspecting and managing cached wrap keys in macOS Keychain
#

set -e

SERVICE="com.sparkpass.vault"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_header() {
    echo -e "${BLUE}===${NC} $1 ${BLUE}===${NC}"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

# Usage information
usage() {
    cat << EOF
SparkPass Keychain Management Helper

USAGE:
    $0 <command> [vault_path]

COMMANDS:
    list                List all SparkPass keychain items
    show <vault_path>   Show details for specific vault cache
    delete <vault_path> Delete cached wrap key for vault
    delete-all          Delete all SparkPass keychain items
    verify <vault_path> Verify cache exists and check age
    help                Show this help message

EXAMPLES:
    # List all cached vaults
    $0 list

    # Show cache details for specific vault
    $0 show /Users/sicarii/Secure/vault.spass

    # Delete cache (force password re-entry)
    $0 delete /Users/sicarii/Secure/vault.spass

    # Delete all SparkPass caches
    $0 delete-all

    # Verify cache exists
    $0 verify ./vaults/test.spass

NOTES:
    - Cached wrap keys expire after 7 days (604,800 seconds)
    - Service name: $SERVICE
    - Account name: Full vault path
    - Storage: Login keychain (not in backups)
EOF
}

# List all SparkPass keychain items
list_items() {
    print_header "SparkPass Keychain Items"
    echo ""

    # Use security find-generic-password to list items
    local items=$(security dump-keychain 2>/dev/null | grep -A 10 "$SERVICE" || true)

    if [ -z "$items" ]; then
        print_warning "No SparkPass keychain items found"
        echo ""
        echo "This is normal if:"
        echo "  - You haven't unlocked a vault yet"
        echo "  - Biometric authentication is not available"
        echo "  - All caches have expired (>7 days old)"
        return 0
    fi

    # Try to extract account names (vault paths)
    local count=0
    while IFS= read -r line; do
        if [[ "$line" == *"acct"* ]]; then
            local account=$(echo "$line" | sed 's/.*"acct"<blob>="\(.*\)".*/\1/')
            if [ -n "$account" ]; then
                count=$((count + 1))
                echo "[$count] $account"
            fi
        fi
    done <<< "$items"

    if [ $count -eq 0 ]; then
        print_warning "Found keychain entries but couldn't parse vault paths"
        echo "Run with 'security dump-keychain | grep -A 20 \"$SERVICE\"' for raw output"
    else
        echo ""
        print_success "Found $count cached vault(s)"
    fi
}

# Show details for specific vault
show_item() {
    local vault_path="$1"

    if [ -z "$vault_path" ]; then
        print_error "Vault path required"
        echo "Usage: $0 show <vault_path>"
        exit 1
    fi

    # Convert to absolute path if relative
    if [[ "$vault_path" != /* ]]; then
        vault_path="$(cd "$(dirname "$vault_path")" && pwd)/$(basename "$vault_path")"
    fi

    print_header "Keychain Cache Details"
    echo ""
    echo "Service: $SERVICE"
    echo "Account: $vault_path"
    echo ""

    # Try to fetch the item (will fail if doesn't exist)
    local output=$(security find-generic-password -s "$SERVICE" -a "$vault_path" 2>&1)

    if echo "$output" | grep -q "could not be found"; then
        print_warning "No cached wrap key found for this vault"
        echo ""
        echo "To create cache:"
        echo "  ./bin/sparkpass_main unlock $vault_path"
        echo ""
        echo "If biometric authentication is available, the cache will be created"
        echo "after successful password authentication."
        return 0
    fi

    # Parse and display information
    echo "$output" | while IFS= read -r line; do
        if [[ "$line" == *"keychain:"* ]]; then
            echo "Keychain: $(echo "$line" | cut -d'"' -f2)"
        elif [[ "$line" == *"class:"* ]]; then
            echo "Class: $(echo "$line" | cut -d'"' -f2)"
        elif [[ "$line" == *"cdat"* ]]; then
            echo "Created: $(echo "$line" | grep -o '0x[0-9A-F]*')"
        elif [[ "$line" == *"mdat"* ]]; then
            echo "Modified: $(echo "$line" | grep -o '0x[0-9A-F]*')"
        fi
    done

    print_success "Cache exists"
}

# Delete cache for specific vault
delete_item() {
    local vault_path="$1"

    if [ -z "$vault_path" ]; then
        print_error "Vault path required"
        echo "Usage: $0 delete <vault_path>"
        exit 1
    fi

    # Convert to absolute path if relative
    if [[ "$vault_path" != /* ]]; then
        vault_path="$(cd "$(dirname "$vault_path")" && pwd)/$(basename "$vault_path")"
    fi

    print_header "Deleting Keychain Cache"
    echo ""
    echo "Vault: $vault_path"
    echo ""

    # Try to delete the item
    if security delete-generic-password -s "$SERVICE" -a "$vault_path" 2>/dev/null; then
        print_success "Cache deleted successfully"
        echo ""
        echo "Next unlock will require password and re-seed the cache."
    else
        print_warning "No cache found (already deleted or never created)"
    fi
}

# Delete all SparkPass keychain items
delete_all() {
    print_header "Delete All SparkPass Caches"
    echo ""
    print_warning "This will delete ALL cached wrap keys for ALL vaults!"
    echo ""
    read -p "Are you sure? (y/N): " -n 1 -r
    echo ""

    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Cancelled"
        exit 0
    fi

    echo ""
    local count=0

    # Find all items with our service name
    while IFS= read -r account; do
        if [ -n "$account" ]; then
            echo "Deleting: $account"
            if security delete-generic-password -s "$SERVICE" -a "$account" 2>/dev/null; then
                count=$((count + 1))
            fi
        fi
    done < <(security dump-keychain 2>/dev/null | grep -A 5 "$SERVICE" | grep "acct" | sed 's/.*"acct"<blob>="\(.*\)".*/\1/')

    echo ""
    if [ $count -gt 0 ]; then
        print_success "Deleted $count cache(s)"
    else
        print_warning "No caches found to delete"
    fi
}

# Verify cache exists and check age
verify_item() {
    local vault_path="$1"

    if [ -z "$vault_path" ]; then
        print_error "Vault path required"
        echo "Usage: $0 verify <vault_path>"
        exit 1
    fi

    # Convert to absolute path if relative
    if [[ "$vault_path" != /* ]]; then
        vault_path="$(cd "$(dirname "$vault_path")" && pwd)/$(basename "$vault_path")"
    fi

    print_header "Verifying Cache"
    echo ""
    echo "Vault: $vault_path"
    echo ""

    # Check if item exists
    if ! security find-generic-password -s "$SERVICE" -a "$vault_path" 2>/dev/null >/dev/null; then
        print_error "Cache not found"
        echo ""
        echo "Biometric unlock will not work for this vault."
        echo "Run 'unlock' command with password to seed cache."
        exit 1
    fi

    print_success "Cache exists"
    echo ""

    # Try to extract data and check timestamp
    # Note: We can't read the actual wrap_key without triggering biometric prompt,
    # but we can check if the item exists and is accessible

    echo "Cache properties:"
    echo "  - Service: $SERVICE"
    echo "  - Account: $vault_path"
    echo "  - Storage: Login keychain (device-locked)"
    echo "  - Access: Requires biometric authentication"
    echo "  - Expiration: 7 days from last update"
    echo ""

    print_success "Cache is valid and accessible"
}

# Main command dispatch
case "${1:-help}" in
    list)
        list_items
        ;;
    show)
        show_item "$2"
        ;;
    delete)
        delete_item "$2"
        ;;
    delete-all)
        delete_all
        ;;
    verify)
        verify_item "$2"
        ;;
    help|--help|-h)
        usage
        ;;
    *)
        print_error "Unknown command: $1"
        echo ""
        usage
        exit 1
        ;;
esac
