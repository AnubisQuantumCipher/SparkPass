# SparkPass Policy CLI Guide

**Version**: 1.0
**Date**: 2025-10-16
**Audience**: End Users, System Administrators

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Policy Concepts](#policy-concepts)
3. [CLI Commands](#cli-commands)
4. [Policy Specifications](#policy-specifications)
5. [Common Use Cases](#common-use-cases)
6. [Security Best Practices](#security-best-practices)
7. [Troubleshooting](#troubleshooting)
8. [Advanced Examples](#advanced-examples)

---

## Quick Start

### Set Passphrase-Only Policy (Default)

```bash
sparkpass policy set --primary "passphrase-only"
```

**What it does**: Only your master passphrase can unlock the vault. No recovery words, no Shamir shares, no Touch ID.

**When to use**: Maximum security, you have strong password manager (like KeePassXC) to store passphrase.

---

### Enable Touch ID Fast Unlock

```bash
sparkpass policy set --primary "passphrase-only" \
                     --fast "touchid" \
                     --ttl 15m \
                     --scope read-only
```

**What it does**: After entering passphrase once, Touch ID can unlock vault for 15 minutes (read-only access).

**When to use**: Convenient for quick password lookups on trusted device.

---

### View Current Policy

```bash
sparkpass policy show
```

**Output**:
```
Current Policy:
  Primary: Passphrase required
  Fast Unlock: Touch ID enabled (15 min TTL, read-only)
```

---

## Policy Concepts

### Primary Policy (What Unlocks Your Vault)

Your vault can be unlocked by:
1. **Passphrase** (Wrap A): Master password you created
2. **Recovery Words** (Wrap B): BIP39 mnemonic (12 or 24 words)
3. **Shamir Shares** (Wrap C-N): k-of-n threshold secret sharing

**Key Rule**: At least ONE of these must be configured for software-only availability.

---

### Fast Unlock Policy (Touch ID Acceleration)

**Optional** feature that allows Touch ID to unlock vault without entering passphrase:
- **Requirements**: Touch ID MUST be used with passphrase (never biometric alone)
- **TTL**: Time-to-live (how long Touch ID remains valid after passphrase entry)
- **Scope**: What Touch ID can do (read passwords vs. modify vault)

**Security**: Touch ID is NOT a replacement for passphrase, only a convenience layer.

---

### Policy Enforcement

SparkPass enforces policies using **SPARK-verified contracts**:
- [OK] Invalid policies rejected at creation (cannot lock yourself out)
- [OK] Touch ID always requires passphrase (biometric never alone)
- [OK] TTL bounded to 24 hours maximum (prevents indefinite access)
- [OK] Software-only availability guaranteed (no hardware dependencies)

---

## CLI Commands

### `sparkpass policy set`

Set vault unlock policy.

**Syntax**:
```bash
sparkpass policy set [--primary SPEC] [--fast SPEC] [--ttl DURATION] [--scope SCOPE]
```

**Options**:
- `--primary SPEC`: Primary unlock policy (see [Policy Specifications](#policy-specifications))
- `--fast SPEC`: Fast unlock policy (`touchid` | `disable`)
- `--ttl DURATION`: Time-to-live for fast unlock (`5m`, `15m`, `1h`, `24h`)
- `--scope SCOPE`: Access scope for fast unlock (`read-only` | `full`)

**Examples**:
```bash
# Passphrase-only (maximum security)
sparkpass policy set --primary "passphrase-only"

# Passphrase OR recovery words (password reset capability)
sparkpass policy set --primary "passphrase-or-recovery"

# 2-of-3 Shamir secret sharing (distributed custody)
sparkpass policy set --primary "2-of-3-shamir"

# Enable Touch ID fast unlock (15 min, read-only)
sparkpass policy set --primary "passphrase-only" \
                     --fast "touchid" \
                     --ttl 15m \
                     --scope read-only
```

---

### `sparkpass policy show`

Display current policy in human-readable format.

**Syntax**:
```bash
sparkpass policy show [--json]
```

**Options**:
- `--json`: Output policy as JSON for scripting

**Example Output** (default format):
```
Current Vault Policy:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Primary Unlock Method:
  Passphrase required (Wrap A)

Fast Unlock:
  Enabled: Yes
  Method: Touch ID (Wrap D)
  Also requires: Passphrase
  TTL: 15 minutes
  Scope: Read-only access

Software-Only Availability: [PASS] Yes
Security Level: High
```

**Example Output** (JSON format):
```json
{
  "primary": {
    "require_passphrase": true,
    "require_recovery": false,
    "allow_shamir": false,
    "shamir_threshold": 0
  },
  "fast": {
    "enabled": true,
    "require_touchid": true,
    "also_passphrase": true,
    "ttl_minutes": 15,
    "scope": "read_only"
  }
}
```

---

### `sparkpass policy validate`

Validate a policy specification without applying it.

**Syntax**:
```bash
sparkpass policy validate SPEC
```

**Examples**:
```bash
# Valid policy
$ sparkpass policy validate "passphrase-only"
[PASS] Policy is valid

# Invalid policy (Touch ID alone)
$ sparkpass policy validate "touchid-only"
✗ Policy is invalid: Touch ID requires passphrase (biometric never alone)

# Invalid policy (Shamir threshold too high)
$ sparkpass policy validate "11-of-10-shamir"
✗ Policy is invalid: Shamir threshold exceeds total shares (k > n)
```

---

### `sparkpass policy reset`

Reset policy to default (passphrase-only).

**Syntax**:
```bash
sparkpass policy reset [--confirm]
```

**Options**:
- `--confirm`: Skip confirmation prompt (for scripting)

**Example**:
```bash
$ sparkpass policy reset
WARNING: This will reset your policy to default (passphrase-only).
Touch ID fast unlock will be DISABLED.
Are you sure? (yes/no): yes
[PASS] Policy reset to default
```

---

## Policy Specifications

### Primary Policy Specifications

#### `passphrase-only`

**Meaning**: Only passphrase (Wrap A) can unlock vault.

**Example**:
```bash
sparkpass policy set --primary "passphrase-only"
```

**When to use**:
- Maximum security
- Single-user vault
- Strong passphrase stored in password manager

**Security**: Highest (single factor, but strong KDF)

---

#### `passphrase-or-recovery`

**Meaning**: Either passphrase (Wrap A) OR recovery words (Wrap B) can unlock.

**Example**:
```bash
sparkpass policy set --primary "passphrase-or-recovery"
```

**When to use**:
- Want password reset capability
- Recovery words stored securely (paper backup, metal plate)

**Security**: High (both factors independent, separate storage)

---

#### `passphrase-and-recovery`

**Meaning**: BOTH passphrase AND recovery words required to unlock.

**Example**:
```bash
sparkpass policy set --primary "passphrase-and-recovery"
```

**When to use**:
- Ultra-high security (2FA)
- Vault contains extremely sensitive data
- Willing to manage two secrets

**Security**: Maximum (compromise of one factor insufficient)

---

#### `k-of-n-shamir`

**Meaning**: k-of-n Shamir secret sharing (threshold cryptography).

**Syntax**: `k-of-n-shamir` where 1 ≤ k ≤ n ≤ 10

**Examples**:
```bash
# 2-of-3: Any 2 of 3 shares unlock vault
sparkpass policy set --primary "2-of-3-shamir"

# 3-of-5: Any 3 of 5 shares unlock vault
sparkpass policy set --primary "3-of-5-shamir"

# 5-of-7: Any 5 of 7 shares unlock vault
sparkpass policy set --primary "5-of-7-shamir"
```

**When to use**:
- Distributed custody (family, business partners)
- No single point of failure
- Threshold access control (e.g., 2 board members to unlock company vault)

**Security**: Very high (compromise of k-1 shares insufficient)

**Caution**: Must securely distribute shares to n parties.

---

### Fast Unlock Specifications

#### `touchid`

**Meaning**: Enable Touch ID fast unlock (requires passphrase).

**Example**:
```bash
sparkpass policy set --fast "touchid" --ttl 15m --scope read-only
```

**Requirements**:
- Primary policy must include passphrase
- macOS with Touch ID hardware
- TTL must be specified

**Security**: Medium (biometric + TTL bounded)

---

#### `disable`

**Meaning**: Disable Touch ID fast unlock.

**Example**:
```bash
sparkpass policy set --fast "disable"
```

**When to use**:
- Maximum security (always require passphrase)
- Shared device (don't want Touch ID cached)

---

### TTL (Time-To-Live) Specifications

**Format**: `<number><unit>`

**Units**:
- `s` = seconds
- `m` = minutes
- `h` = hours

**Examples**:
- `30s` = 30 seconds
- `5m` = 5 minutes
- `15m` = 15 minutes (recommended default)
- `30m` = 30 minutes
- `1h` = 1 hour
- `2h` = 2 hours
- `24h` = 24 hours (maximum)

**Constraints**:
- Minimum: 1 minute (prevents accidental unlock)
- Maximum: 24 hours (NIST SP 800-63B recommendation)

---

### Scope Specifications

#### `read-only`

**Meaning**: Touch ID can read passwords but cannot modify vault.

**Example**:
```bash
sparkpass policy set --fast "touchid" --ttl 15m --scope read-only
```

**Allowed Operations**:
- [OK] Read passwords
- [OK] Copy passwords to clipboard
- [OK] Search entries
- [FAIL] Add/edit/delete passwords
- [FAIL] Change master passphrase
- [FAIL] Modify policy

**When to use**:
- Quick password lookups
- Minimize damage if device stolen during TTL window

---

#### `full`

**Meaning**: Touch ID can read AND modify vault.

**Example**:
```bash
sparkpass policy set --fast "touchid" --ttl 30m --scope full
```

**Allowed Operations**:
- [OK] Read passwords
- [OK] Add/edit/delete passwords
- [OK] Change master passphrase
- [OK] Modify policy
- [OK] All vault operations

**When to use**:
- Extended work session
- Trusted device in secure location

**Security**: Use shorter TTL (≤ 30 minutes) for full access.

---

## Common Use Cases

### Use Case 1: Personal User (Maximum Convenience)

**Scenario**: Single user on trusted MacBook, wants quick password access.

**Policy**:
```bash
sparkpass policy set --primary "passphrase-only" \
                     --fast "touchid" \
                     --ttl 15m \
                     --scope read-only
```

**Benefits**:
- Unlock with Touch ID for 15 minutes after passphrase entry
- Read-only prevents accidental modifications
- Falls back to passphrase if Touch ID unavailable

---

### Use Case 2: Personal User (Maximum Security)

**Scenario**: Single user, stores extremely sensitive data (bank accounts, crypto seeds).

**Policy**:
```bash
sparkpass policy set --primary "passphrase-only" \
                     --fast "disable"
```

**Benefits**:
- Always requires passphrase (no Touch ID)
- No TTL window for stolen device attack
- Highest security, slightly less convenient

---

### Use Case 3: Password Reset Capability

**Scenario**: User wants backup unlock method in case passphrase forgotten.

**Setup**:
1. Generate recovery words during vault creation
2. Store recovery words securely (paper backup in safe)
3. Set policy to allow either factor

**Policy**:
```bash
sparkpass policy set --primary "passphrase-or-recovery"
```

**Benefits**:
- Can unlock with passphrase OR recovery words
- If passphrase forgotten, use recovery words
- Recovery words stored offline (immune to computer compromise)

**Caution**: Recovery words are equivalent to passphrase. Store securely!

---

### Use Case 4: Shared Family Vault (2-of-3 Threshold)

**Scenario**: Married couple + trusted friend hold vault access. Any 2 can unlock.

**Setup**:
1. Generate 3 Shamir shares during vault creation
2. Give Share 1 to spouse A, Share 2 to spouse B, Share 3 to friend
3. Set policy to require any 2 shares

**Policy**:
```bash
sparkpass policy set --primary "2-of-3-shamir"
```

**Benefits**:
- No single point of failure (1 share lost → still recoverable)
- Requires collusion of 2 parties (compromise protection)
- Flexible access (any 2 of 3 can unlock)

**Use Cases**:
- Estate planning (family can recover vault if user dies)
- Business continuity (key employees can access company vault)

---

### Use Case 5: Corporate Vault (3-of-5 Board Approval)

**Scenario**: Company vault requires 3 of 5 board members to unlock.

**Setup**:
1. Generate 5 Shamir shares
2. Distribute to 5 board members
3. Set policy to require 3 shares

**Policy**:
```bash
sparkpass policy set --primary "3-of-5-shamir"
```

**Benefits**:
- Distributed custody (no single person controls vault)
- Threshold approval (3-person majority required)
- Tolerates absences (only need 3 of 5 present)

**Compliance**: Meets multi-person integrity requirements (e.g., PCI DSS 3.6.6)

---

## Security Best Practices

### 1. Passphrase Strength

**Minimum**: 8 characters (NIST SP 800-63B)
**Recommended**: 15+ characters or 5+ word diceware passphrase

**Good Examples**:
- `correct-horse-battery-staple-mountain` (5-word diceware)
- `Tr0ub4dor&3.extended.version` (complex password + extension)

**Bad Examples**:
- `password123` (common password, easily guessed)
- `qwerty` (keyboard pattern, dictionary word)

**SparkPass Defense**: Argon2id with 1 GiB memory + 3 iterations (≈2 seconds on M1 Mac)

---

### 2. Recovery Words Storage

**Recommended**:
- [OK] Write on paper, store in safe/safety deposit box
- [OK] Stamp on metal plate (fireproof, waterproof)
- [OK] Use BIP39 standard (2048-word dictionary, 11 bits entropy per word)

**DO NOT**:
- [FAIL] Store in cloud (Dropbox, Google Drive)
- [FAIL] Take photo with phone (vulnerable to backup compromise)
- [FAIL] Email to yourself (plaintext exposure)

---

### 3. Touch ID TTL Selection

**Guidelines**:
- **5-15 minutes**: Read-only access (password lookups)
- **30 minutes**: Full access on trusted device
- **1-2 hours**: Extended work session (rare, high-trust environment)
- **24 hours**: DO NOT USE (too long, violates NIST recommendations)

**Risk Analysis**:
- Short TTL = More passphrase prompts (annoying but secure)
- Long TTL = Device stolen during window (attacker gains access)

**Recommendation**: Start with 15 minutes read-only, adjust based on usage patterns.

---

### 4. Shamir Share Distribution

**Best Practices**:
- [OK] Give shares to geographically distributed parties (prevents simultaneous compromise)
- [OK] Use trusted individuals (family, close friends, lawyers)
- [OK] Inform shareholders of their role and importance
- [OK] Store share backup separately from primary (redundancy)

**DO NOT**:
- [FAIL] Store multiple shares in same location (defeats threshold property)
- [FAIL] Email shares (plaintext exposure)
- [FAIL] Give all shares to same person (defeats purpose)

**Threshold Selection**:
- `2-of-3`: Good for couples (either spouse + trusted friend)
- `3-of-5`: Good for families (majority rule)
- `5-of-7`: Good for corporations (quorum requirements)

---

### 5. Policy Change Auditing

**Recommendation**: After changing policy, verify:
```bash
# Show current policy
sparkpass policy show

# Test unlock with expected factors
sparkpass unlock --passphrase  # Should succeed
sparkpass unlock --touchid     # Should succeed if fast unlock enabled
```

**Caution**: Policy changes are IMMEDIATELY effective. Test thoroughly!

---

## Troubleshooting

### Problem: "Touch ID requires passphrase (biometric never alone)"

**Cause**: Trying to enable Touch ID without passphrase in primary policy.

**Solution**:
```bash
# Correct: Include passphrase in primary policy
sparkpass policy set --primary "passphrase-only" --fast "touchid" --ttl 15m
```

**Rationale**: Biometrics are NOT secrets (NIST SP 800-63B). Always require passphrase.

---

### Problem: "TTL exceeds 24 hours (max 1440 minutes)"

**Cause**: Specified TTL greater than 24 hours.

**Solution**:
```bash
# Bad: 48 hours (rejected)
sparkpass policy set --ttl 48h

# Good: 24 hours (maximum)
sparkpass policy set --ttl 24h
```

**Rationale**: NIST SP 800-63B Section 7.2 requires reauthentication ≤ 12 hours. 24 hours is SparkPass maximum.

---

### Problem: "Shamir threshold exceeds total shares (k > n)"

**Cause**: k-of-n where k > n (mathematically impossible).

**Solution**:
```bash
# Bad: 5-of-3 (cannot get 5 shares from 3 total)
sparkpass policy set --primary "5-of-3-shamir"

# Good: 2-of-3 (can get 2 shares from 3 total)
sparkpass policy set --primary "2-of-3-shamir"
```

---

### Problem: "No unlock method enabled (software-only failed)"

**Cause**: Policy has all unlock methods disabled.

**Solution**: This should be prevented by type predicates, but if encountered:
```bash
# Reset to default policy
sparkpass policy reset --confirm
```

**Explanation**: SparkPass ensures at least one software-only unlock method is always available.

---

### Problem: "Policy validation failed: Invalid serialization"

**Cause**: Vault file corrupted or tampered with.

**Solutions**:
1. **Check backups**: Restore from recent backup
2. **Verify integrity**: `sparkpass verify --check-signature`
3. **Force default policy**: `sparkpass policy reset --force`

**Warning**: Forcing default policy may lock you out if you don't have passphrase.

---

## Advanced Examples

### Example 1: Gradually Relaxing Policy

**Scenario**: Start with maximum security, gradually add convenience features.

**Phase 1 - Maximum Security**:
```bash
sparkpass policy set --primary "passphrase-only" --fast "disable"
```

**Phase 2 - Add Touch ID (read-only)**:
```bash
sparkpass policy set --primary "passphrase-only" \
                     --fast "touchid" \
                     --ttl 15m \
                     --scope read-only
```

**Phase 3 - Increase TTL**:
```bash
sparkpass policy set --primary "passphrase-only" \
                     --fast "touchid" \
                     --ttl 30m \
                     --scope read-only
```

**Phase 4 - Add Full Access** (optional):
```bash
sparkpass policy set --primary "passphrase-only" \
                     --fast "touchid" \
                     --ttl 30m \
                     --scope full
```

**Recommendation**: Stop at Phase 2 or 3 for best security/convenience balance.

---

### Example 2: Emergency Access (Dead Man's Switch)

**Scenario**: Grant trusted person access to vault if you're incapacitated.

**Setup**:
1. Generate 2-of-2 Shamir shares
2. Keep Share 1 yourself
3. Give Share 2 to trusted person (sealed envelope)
4. Instruct them to open envelope only in emergency

**Policy**:
```bash
sparkpass policy set --primary "2-of-2-shamir"
```

**Normal Operation**:
- You have both shares (full access)

**Emergency**:
- Trusted person opens envelope, has Share 2
- With your Share 1 (from computer/safe), they can unlock vault

**Alternative**: Use `1-of-2-shamir` if you want them to have independent access.

---

### Example 3: Temporary High-Security Mode

**Scenario**: Going to high-risk location (border crossing, protest), want maximum security.

**Before Trip**:
```bash
# Disable Touch ID, require passphrase only
sparkpass policy set --primary "passphrase-only" --fast "disable"

# Lock vault
sparkpass lock
```

**During Trip**:
- Vault requires passphrase on every access
- No Touch ID cache (immune to device seizure)

**After Trip**:
```bash
# Re-enable Touch ID
sparkpass policy set --primary "passphrase-only" \
                     --fast "touchid" \
                     --ttl 15m \
                     --scope read-only
```

---

### Example 4: Scripted Policy Management

**Scenario**: DevOps team manages multiple vaults, wants consistent policy.

**Script** (`set_company_policy.sh`):
```bash
#!/bin/bash
# Set company-standard policy for all vaults

VAULT_DIR="/opt/company/vaults"

for vault in "$VAULT_DIR"/*.vault; do
  echo "Setting policy for $vault..."

  sparkpass --vault "$vault" policy set \
    --primary "3-of-5-shamir" \
    --fast "disable"

  # Verify policy
  sparkpass --vault "$vault" policy show --json | \
    jq -e '.primary.allow_shamir and .primary.shamir_threshold == 3'

  if [ $? -ne 0 ]; then
    echo "ERROR: Policy validation failed for $vault"
    exit 1
  fi
done

echo "All vault policies updated successfully"
```

**Usage**:
```bash
chmod +x set_company_policy.sh
./set_company_policy.sh
```

---

### Example 5: JSON Policy Management

**Scenario**: Export/import policies for backup or migration.

**Export Policy**:
```bash
sparkpass policy show --json > my_vault_policy.json
```

**Edit Policy** (in text editor or jq):
```bash
jq '.fast.ttl_minutes = 30' my_vault_policy.json > modified_policy.json
```

**Import Policy**:
```bash
sparkpass policy import modified_policy.json
```

**Validate Before Import**:
```bash
sparkpass policy validate --json modified_policy.json
```

---

## Command Reference Summary

| Command | Description | Example |
|---------|-------------|---------|
| `policy set` | Set vault policy | `sparkpass policy set --primary "passphrase-only"` |
| `policy show` | Display current policy | `sparkpass policy show` |
| `policy show --json` | Display policy as JSON | `sparkpass policy show --json` |
| `policy validate` | Validate policy spec | `sparkpass policy validate "2-of-3-shamir"` |
| `policy reset` | Reset to default | `sparkpass policy reset` |
| `policy import` | Import policy from JSON | `sparkpass policy import policy.json` |
| `policy export` | Export policy to JSON | `sparkpass policy export > policy.json` |

---

## Quick Reference: Policy Specifications

| Specification | Meaning | Security | Convenience |
|---------------|---------|----------|-------------|
| `passphrase-only` | Only passphrase | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| `passphrase-or-recovery` | Passphrase OR recovery | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| `passphrase-and-recovery` | Passphrase AND recovery | ⭐⭐⭐⭐⭐ | ⭐⭐ |
| `2-of-3-shamir` | 2 of 3 shares | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| `3-of-5-shamir` | 3 of 5 shares | ⭐⭐⭐⭐⭐ | ⭐⭐ |
| `+ touchid (15m, read)` | Touch ID acceleration | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| `+ touchid (30m, full)` | Touch ID full access | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |

**Legend**:
- ⭐⭐⭐⭐⭐ = Excellent
- ⭐⭐⭐⭐ = Very Good
- ⭐⭐⭐ = Good
- ⭐⭐ = Fair

---

## Getting Help

**CLI Help**:
```bash
sparkpass policy --help
sparkpass policy set --help
sparkpass policy show --help
```

**Documentation**:
- Design Document: `/Users/sicarii/SparkPass/docs/POLICY_ENGINE_DESIGN.md`
- Source Code: `/Users/sicarii/SparkPass/src/sparkpass/vault/sparkpass-vault-policy.ads`

**Community**:
- GitHub Issues: https://github.com/sparkpass/sparkpass/issues
- Security Email: sic.tau@pm.me

---

**End of Guide**
