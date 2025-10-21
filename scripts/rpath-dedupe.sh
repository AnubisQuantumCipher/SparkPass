#!/bin/sh
# Deduplicate LC_RPATH entries in a Mach-O binary (macOS)
# Usage: rpath-dedupe.sh /path/to/binary
set -eu
BIN_PATH="${1:-}"
if [ -z "$BIN_PATH" ] || [ ! -f "$BIN_PATH" ]; then
  echo "Usage: $0 /path/to/binary" >&2
  exit 2
fi
# Delete all existing LC_RPATH entries
otool -l "$BIN_PATH" | awk '/LC_RPATH/{f=1;next} f&&/path/{print $2; f=0}' | while IFS= read -r p; do
  install_name_tool -delete_rpath "$p" "$BIN_PATH" 2>/dev/null || true
done
# Re-add minimal rpaths once
install_name_tool -add_rpath /opt/homebrew/opt/openssl@3/lib "$BIN_PATH" 2>/dev/null || true
install_name_tool -add_rpath /opt/homebrew/Cellar/liboqs/0.14.0/lib "$BIN_PATH" 2>/dev/null || true
exit 0
