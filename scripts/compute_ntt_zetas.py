#!/usr/bin/env python3
"""
Compute NTT zeta table for ML-DSA-87 (FIPS 204)

Zeta values are powers of the primitive 512-th root of unity:
  ζ = 1753 mod q (where q = 8380417)

The zeta table contains ζ^(2*bitrev(i)+1) mod q in Montgomery form
for i = 0..127, repeated/inverted for full 256 entries.
"""

Q = 8380417  # ML-DSA modulus
ZETA = 1753  # Primitive 512-th root of unity
MONT_R = 2**32  # Montgomery R


def pow_mod(base, exp, mod):
    """Modular exponentiation."""
    result = 1
    base = base % mod
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % mod
        exp = exp >> 1
        base = (base * base) % mod
    return result


def bitrev7(x):
    """7-bit bit reversal."""
    result = 0
    for i in range(7):
        result = (result << 1) | (x & 1)
        x >>= 1
    return result


def to_montgomery(x):
    """Convert to Montgomery form: x * R mod q."""
    return (x * MONT_R) % Q


# Compute zeta table
zetas = []
for i in range(128):
    # ζ^(2*bitrev(i) + 1) mod q
    brv = bitrev7(i)
    exp = 2 * brv + 1
    zeta_val = pow_mod(ZETA, exp, Q)
    zeta_mont = to_montgomery(zeta_val)
    zetas.append(zeta_mont)

# Print as Ada array
print("   Zetas : constant Zeta_Array := (")
for i in range(0, 128, 8):
    line = "      "
    for j in range(8):
        if i + j < 128:
            line += f"{i+j:3} => {zetas[i+j]:8}, "
    print(line)

# Compute inverse zetas (for NTT_Inv)
inv_zetas = []
for z in zetas:
    # Inverse is just -z mod q in coefficient space
    # In Montgomery form, we negate
    inv_z = Q - z if z != 0 else 0
    inv_zetas.append(inv_z)

print("\n      -- Inverse zetas for NTT^-1")
for i in range(128, 256, 8):
    idx = i - 128
    line = "      "
    for j in range(8):
        if idx + j < 128:
            line += f"{i+j:3} => {inv_zetas[idx+j]:8}, "
    print(line)

print("      255 => 0")
print("   );")

print(f"\n-- Verification:")
print(f"-- Q = {Q}")
print(f"-- ζ = {ZETA}")
print(f"-- ζ^512 mod q = {pow_mod(ZETA, 512, Q)} (should be 1)")
print(f"-- zetas[0] (ζ^1 in Montgomery) = {zetas[0]}")
