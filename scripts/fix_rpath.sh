#!/bin/bash
# Post-build script to fix duplicate RPATH entries
# macOS dyld aborts on duplicate RPATHs, so we need to remove them

BINARY="bin/sparkpass_main"

if [ ! -f "$BINARY" ]; then
    echo "Error: Binary not found at $BINARY"
    exit 1
fi

echo "Fixing duplicate RPATH entries in $BINARY..."

# Find all duplicate RPATHs
DUPLICATES=$(otool -l "$BINARY" | grep -A 2 LC_RPATH | grep "path " | awk '{print $2}' | sort | uniq -d)

if [ -z "$DUPLICATES" ]; then
    echo "No duplicate RPATHs found - binary is clean"
    exit 0
fi

# Remove one instance of each duplicate
for RPATH in $DUPLICATES; do
    echo "  Removing duplicate: $RPATH"
    install_name_tool -delete_rpath "$RPATH" "$BINARY" 2>&1 || true
done

echo "✓ RPATH cleanup complete"
