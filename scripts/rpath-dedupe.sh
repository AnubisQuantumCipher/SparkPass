#!/usr/bin/env bash
set -euo pipefail

BIN_PATH="${1:-}"
if [[ -z "${BIN_PATH}" || ! -f "${BIN_PATH}" ]]; then
  echo "Usage: $0 /path/to/binary" >&2
  exit 2
fi

# Collect all existing LC_RPATH entries
mapfile -t RPATHS < <(otool -l "$BIN_PATH" | awk '/LC_RPATH/{show=1;next} show&&/path/{print $2; show=0}')

# Delete all current rpaths (safe if path not found)
for p in "${RPATHS[@]}"; do
  install_name_tool -delete_rpath "$p" "$BIN_PATH" 2>/dev/null || true
done

# Minimal set to re-add; customize as needed
ESSENTIAL=(
  "/opt/homebrew/opt/openssl@3/lib"
  "/opt/homebrew/Cellar/liboqs/0.14.0/lib"
)

# Add GNAT adalib if present in environment
GNAT_ADALIB_DIR="${GNAT_NATIVE_ALIRE_PREFIX:-}"/lib/gcc/aarch64-apple-darwin*/14.2.0/adalib
if ls $GNAT_ADALIB_DIR >/dev/null 2>&1; then
  ESSENTIAL+=("$(ls -d $GNAT_ADALIB_DIR | head -n1)")
fi

# Re-add unique essentials
declare -A SEEN
for p in "${ESSENTIAL[@]}"; do
  [[ -z "$p" ]] && continue
  if [[ -z "${SEEN[$p]:-}" ]]; then
    install_name_tool -add_rpath "$p" "$BIN_PATH" 2>/dev/null || true
    SEEN[$p]=1
  fi
done

exit 0
