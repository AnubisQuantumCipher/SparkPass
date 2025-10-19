#!/usr/bin/env python3
"""
Generate Argon2id test vectors for SparkPass validation.

This script uses the 'argon2-cffi' Python library (which wraps the reference
C implementation) to generate test vectors matching SparkPass's configuration:
  - Algorithm: Argon2id
  - Memory: 16 MiB (16,384 KiB)
  - Iterations: 4
  - Parallelism: 1
  - Output: 32 bytes
  - Version: 0x13 (19)

Install dependencies:
  $ pip3 install argon2-cffi

Usage:
  $ python3 generate_test_vectors.py

The script will output hex-formatted test vectors suitable for pasting into
test_argon2id_vectors.adb.
"""

import sys

try:
    from argon2 import low_level
    from argon2 import Type
except ImportError:
    print("Error: argon2-cffi not installed", file=sys.stderr)
    print("Install with: pip3 install argon2-cffi", file=sys.stderr)
    sys.exit(1)

# SparkPass Argon2id parameters
MEMORY_COST = 16384  # KiB (16 MiB)
TIME_COST = 4
PARALLELISM = 1
HASH_LEN = 32
ARGON2_VERSION = 19  # 0x13


def bytes_to_ada_hex(data, name="Data"):
    """Convert bytes to Ada hex array format."""
    hex_bytes = [f"16#{b:02x}#" for b in data]

    # Format as Ada array with 8 bytes per line
    lines = []
    for i in range(0, len(hex_bytes), 8):
        chunk = hex_bytes[i:i+8]
        line = ", ".join(chunk)
        if i + 8 < len(hex_bytes):
            line += ","
        lines.append(f"         {line}")

    return "\n".join(lines)


def generate_vector(test_num, password, salt, description):
    """Generate a single Argon2id test vector."""

    # Ensure inputs are bytes
    if isinstance(password, str):
        password = password.encode('utf-8')
    if isinstance(salt, str):
        salt = salt.encode('utf-8')

    # Pad or truncate salt to 32 bytes (SparkPass.Config.Argon2_Salt_Length)
    if len(salt) < 32:
        salt = salt + bytes([0] * (32 - len(salt)))
    elif len(salt) > 32:
        salt = salt[:32]

    # Generate hash using Argon2id
    try:
        hash_output = low_level.hash_secret_raw(
            secret=password,
            salt=salt,
            time_cost=TIME_COST,
            memory_cost=MEMORY_COST,
            parallelism=PARALLELISM,
            hash_len=HASH_LEN,
            type=Type.ID,  # Argon2id
            version=ARGON2_VERSION
        )
    except Exception as e:
        print(f"Error generating hash: {e}", file=sys.stderr)
        return

    # Print test vector in Ada format
    print(f"\n   --  Test {test_num}: {description}")
    print(f"   --  Password: {password!r} ({len(password)} bytes)")
    print(f"   --  Salt: {salt!r} (first 32 bytes)")
    print(f"   --  Expected: {hash_output.hex()}")
    print()
    print(f"   procedure Test_Vector_{test_num} is")
    print(f"      Password : constant Byte_Array := (")
    print(bytes_to_ada_hex(password, "Password"))
    print(f"      );")
    print(f"      Salt : constant Salt_Array := (")
    print(bytes_to_ada_hex(salt, "Salt"))
    print(f"      );")
    print(f"      Expected : constant Key_Array := (")
    print(bytes_to_ada_hex(hash_output, "Expected"))
    print(f"      );")
    print(f"   begin")
    print(f"      Run_Test (\"{description}\", Password, Salt, Expected);")
    print(f"   end Test_Vector_{test_num};")
    print()


def main():
    """Generate all test vectors."""

    print("=" * 78)
    print("SparkPass Argon2id Test Vector Generator")
    print("=" * 78)
    print()
    print(f"Configuration:")
    print(f"  Memory:      {MEMORY_COST} KiB (16 MiB)")
    print(f"  Iterations:  {TIME_COST}")
    print(f"  Parallelism: {PARALLELISM}")
    print(f"  Output:      {HASH_LEN} bytes")
    print(f"  Version:     {ARGON2_VERSION} (0x13)")
    print()
    print("Generating test vectors...")
    print()

    # Test Vector 1: Simple ASCII password
    generate_vector(
        test_num=1,
        password="password",
        salt="somesaltSOMESALTsomesaltSOMESALT",
        description="password/somesalt"
    )

    # Test Vector 2: Longer passphrase
    generate_vector(
        test_num=2,
        password="correct horse battery staple",
        salt=bytes(range(1, 33)),  # 0x01..0x20
        description="long password/hex salt"
    )

    # Test Vector 3: Minimal password
    generate_vector(
        test_num=3,
        password="a",
        salt=bytes([0] * 32),
        description="minimal password/zero salt"
    )

    # Test Vector 4: UTF-8 password
    generate_vector(
        test_num=4,
        password="π√∞",  # UTF-8: CF 80 E2 88 9A E2 88 9E
        salt=bytes([0xff] * 32),
        description="UTF-8 password/max salt"
    )

    # Test Vector 5: Long password
    generate_vector(
        test_num=5,
        password="The quick brown fox jumps over the lazy dog. Jackdaws love my bi",
        salt=bytes([0xaa, 0x55] * 16),  # Alternating pattern
        description="long password/alternating salt"
    )

    print("=" * 78)
    print("Generation complete!")
    print()
    print("Next steps:")
    print("  1. Copy the generated test procedures above")
    print("  2. Replace the corresponding procedures in test_argon2id_vectors.adb")
    print("  3. Rebuild and run the test suite")
    print("=" * 78)


if __name__ == "__main__":
    main()
