# SparkPass Non-Interactive Usage Guide

## Overview

SparkPass now supports **three password input methods** to enable automation and scripting while maintaining security:

1. **Interactive TTY** (default) - Secure prompts with echo disabled
2. **Stdin pipe/redirect** - For shell scripts and automation
3. **Environment variable** - For testing and controlled environments

## Password Input Methods

### Method 1: Interactive (Default)

When you run SparkPass from a terminal (TTY), passwords are prompted securely with echo disabled.

```bash
./bin/sparkpass_main init my_vault.spass
# Prompts: Enter password: ****
# Prompts: Confirm password: ****
```

**Pros:**
- Most secure - password never appears in process listings or logs
- No command history pollution
- Works out of the box

**Cons:**
- Cannot be automated
- Requires human interaction

---

### Method 2: Stdin Pipe/Redirect

For scripts and automation, you can pipe or redirect passwords from stdin:

```bash
# Using echo (simple but visible in process list)
echo "my_secure_password" | ./bin/sparkpass_main unlock vault.spass

# Using heredoc (better - not in process list)
./bin/sparkpass_main unlock vault.spass <<EOF
my_secure_password
EOF

# Using password file (recommended for automation)
./bin/sparkpass_main unlock vault.spass < password.txt

# For commands requiring multiple passwords (init, add)
echo "password1
password2" | ./bin/sparkpass_main init vault.spass
```

**Pros:**
- Fully automated
- No environment variable pollution
- Works in non-interactive environments (CI/CD, cron, etc.)

**Cons:**
- Password may appear in shell history (`echo` method)
- Requires careful scripting to avoid leaks

**Best practices:**
```bash
# Store password in a file with secure permissions
echo "my_password" > /tmp/vault_password
chmod 600 /tmp/vault_password

# Use the password file
./bin/sparkpass_main unlock vault.spass < /tmp/vault_password

# Clean up
rm /tmp/vault_password
```

---

### Method 3: Environment Variable

For testing and automation in controlled environments:

```bash
# Set password in environment
export SPARKPASS_PASSWORD="my_secure_password"

# Run commands (no prompts, no stdin needed)
./bin/sparkpass_main unlock vault.spass
./bin/sparkpass_main ls vault.spass
./bin/sparkpass_main get vault.spass github

# Unset when done
unset SPARKPASS_PASSWORD
```

**Pros:**
- Simplest for automation
- Works for all commands automatically
- No stdin complications

**Cons:**
- **SECURITY WARNING**: Environment variables are visible in process listings (`ps auxe`)
- Inherited by child processes
- May appear in logs

**When to use:**
- Local testing only
- Isolated CI/CD containers (where process isolation is guaranteed)
- Scripts running in secure, single-user environments

**When NOT to use:**
- Multi-user systems
- Production servers
- Any environment where `ps` output is accessible to others

---

## Complete Examples

### Example 1: Automated Vault Creation

```bash
#!/bin/bash
# create_vault.sh

VAULT_PATH="backups/passwords.spass"
PASSWORD="$(openssl rand -base64 32)"  # Generate strong password

# Create vault non-interactively
echo "$PASSWORD
$PASSWORD" | ./bin/sparkpass_main init "$VAULT_PATH"

# Store password securely (e.g., in separate encrypted file)
echo "$PASSWORD" | gpg --encrypt --recipient your@email.com > vault_password.gpg

# Clean up
unset PASSWORD
```

### Example 2: Batch Entry Addition

```bash
#!/bin/bash
# add_entries.sh

VAULT="my_vault.spass"
PASSWORD_FILE="/secure/vault_password.txt"

# Add multiple entries
while IFS=',' read -r label secret; do
    echo "$secret
$(<$PASSWORD_FILE)" | ./bin/sparkpass_main add "$VAULT" "$label"
done < entries.csv
```

`entries.csv`:
```
github_token,ghp_abc123...
aws_key,AKIAIOSFODNN7EXAMPLE
db_password,super_secret_123
```

### Example 3: CI/CD Integration

```yaml
# .github/workflows/deploy.yml
name: Deploy with SparkPass

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v2

      - name: Unlock vault and get secrets
        env:
          SPARKPASS_PASSWORD: ${{ secrets.VAULT_PASSWORD }}
        run: |
          ./bin/sparkpass_main unlock vault.spass

          # Get secrets and export to environment
          GITHUB_TOKEN=$(echo "$SPARKPASS_PASSWORD" | ./bin/sparkpass_main get vault.spass github | grep '[PASS]' | cut -d' ' -f2-)
          export GITHUB_TOKEN

          # Use secrets for deployment
          ./deploy.sh
```

### Example 4: Automated Backup Script

```bash
#!/bin/bash
# backup_vault.sh

set -e

VAULT="/data/vault.spass"
BACKUP_DIR="/backups/vault-$(date +%Y%m%d)"
PASSWORD_FILE="/secure/.vault_pass"

mkdir -p "$BACKUP_DIR"

# Export recovery file
./bin/sparkpass_main export "$VAULT" < "$PASSWORD_FILE"

# Copy vault and recovery to backup location
cp "$VAULT" "$BACKUP_DIR/"
cp "$VAULT.recovery" "$BACKUP_DIR/"

# Encrypt backup
tar czf "$BACKUP_DIR.tar.gz" "$BACKUP_DIR"
gpg --encrypt --recipient backup@example.com "$BACKUP_DIR.tar.gz"

# Clean up
rm -rf "$BACKUP_DIR" "$BACKUP_DIR.tar.gz"
```

### Example 5: Password Rotation Script

```bash
#!/bin/bash
# rotate_keys.sh

VAULT="vault.spass"

# Read password from secure location
PASSWORD=$(<~/.vault_password)

# Rotate master key
echo "$PASSWORD" | ./bin/sparkpass_main rotate "$VAULT"

# Export new recovery file
echo "$PASSWORD" | ./bin/sparkpass_main export "$VAULT"

# Backup new recovery file securely
cp "$VAULT.recovery" "/secure/backups/recovery-$(date +%Y%m%d).spass"

unset PASSWORD
```

---

## Security Considerations

### Priority Order (Most to Least Secure)

1. **Interactive TTY** - Password never leaves terminal, not in process list or history
2. **Stdin from secure file** - Password in file (0600 permissions), not in process list
3. **Stdin from heredoc** - Not in process list, but may be in shell history
4. **Stdin from echo** - Visible in process list briefly, in shell history
5. **Environment variable** - Visible in process list, inherited by children, may persist

### Recommendations

**For manual use:**
- Use interactive mode (default)

**For scripts on secure systems:**
- Use stdin from password file with 0600 permissions
- Store password file outside vault directory
- Clean up password file after use

**For CI/CD:**
- Use environment variable (container isolation provides security)
- Use GitHub Secrets, GitLab CI/CD Variables, or similar
- Never commit passwords to repository

**For production automation:**
- Use stdin from password manager (e.g., `pass`, `vault`)
- Example: `pass show vault/password | sparkpass unlock vault.spass`
- Integrate with system keychain/secrets manager

### Common Pitfalls

1. **Shell history pollution**
   ```bash
   # BAD - password in history
   echo "password123" | sparkpass unlock vault.spass

   # GOOD - no history entry (space prefix)
    echo "password123" | sparkpass unlock vault.spass

   # BETTER - from file
   sparkpass unlock vault.spass < password.txt
   ```

2. **Process listing exposure**
   ```bash
   # BAD - visible in ps output
   export SPARKPASS_PASSWORD="password123"

   # BETTER - stdin from secure source
   pass show vault/password | sparkpass unlock vault.spass
   ```

3. **Temporary file cleanup**
   ```bash
   # Always clean up
   trap 'rm -f /tmp/password.txt' EXIT
   echo "password" > /tmp/password.txt
   sparkpass unlock vault.spass < /tmp/password.txt
   ```

---

## Testing Non-Interactive Mode

To verify non-interactive mode works:

```bash
# Test 1: Create vault with stdin
echo "test_password_12345
test_password_12345" | ./bin/sparkpass_main init test.spass

# Test 2: Unlock with env var
export SPARKPASS_PASSWORD="test_password_12345"
./bin/sparkpass_main unlock test.spass
unset SPARKPASS_PASSWORD

# Test 3: Add entry with stdin
echo "secret_value
test_password_12345" | ./bin/sparkpass_main add test.spass test_label

# Test 4: List entries with env var
export SPARKPASS_PASSWORD="test_password_12345"
./bin/sparkpass_main ls test.spass
unset SPARKPASS_PASSWORD

# Test 5: Get entry with stdin
echo "test_password_12345" | ./bin/sparkpass_main get test.spass test_label

# Cleanup
rm test.spass
```

---

## Comparison with anubis-spark

SparkPass now matches anubis-spark in terms of automation capability:

| Feature | anubis-spark | SparkPass (new) | SparkPass (old) |
|---------|--------------|-----------------|-----------------|
| Interactive password | [PASS] | [PASS] | [PASS] |
| Stdin pipe/redirect | [PASS] | [PASS] | ✗ |
| Environment variable | [PASS] | [PASS] | ✗ |
| CI/CD friendly | [PASS] | [PASS] | ✗ |
| TTY detection | [PASS] | [PASS] | ✗ |
| Security | Good | Better (Argon2id) | Best (but unusable) |

**Key improvement**: SparkPass now combines world-class security (Argon2id 1GiB, ML-KEM-1024, SPARK verification) with practical usability for automation.

---

## Technical Details

### TTY Detection

SparkPass automatically detects if stdin is a TTY using `isatty(0)`:

- **TTY detected** → Interactive mode (echo disabled with termios)
- **No TTY** → Non-interactive mode (read from stdin directly)

This means you don't need any flags or special syntax - it "just works":

```bash
# Automatically uses interactive mode
./bin/sparkpass_main unlock vault.spass

# Automatically uses non-interactive mode
echo "password" | ./bin/sparkpass_main unlock vault.spass
```

### Password Priority

If multiple password sources are available, SparkPass uses this priority:

1. `SPARKPASS_PASSWORD` environment variable (if set)
2. Stdin (pipe/redirect) if not a TTY
3. Interactive prompt if TTY

Example:
```bash
# This uses env var, ignoring stdin
export SPARKPASS_PASSWORD="env_password"
echo "stdin_password" | ./bin/sparkpass_main unlock vault.spass
# → Uses "env_password"
```

---

## Troubleshooting

### Problem: "Could not disable echo" error

**Cause**: Running in non-TTY environment without stdin redirect

**Solution**: Use stdin or environment variable:
```bash
# Instead of:
./bin/sparkpass_main unlock vault.spass  # Fails in cron/CI

# Use:
echo "$PASSWORD" | ./bin/sparkpass_main unlock vault.spass
```

### Problem: Command hangs waiting for input

**Cause**: Forgot to provide password in non-interactive environment

**Solution**: Check that stdin or env var is set:
```bash
# Debug: check if TTY
if [ -t 0 ]; then
    echo "stdin is a TTY - interactive mode"
else
    echo "stdin is NOT a TTY - need to provide password"
fi
```

### Problem: Password visible in process list

**Cause**: Using `echo` or environment variable

**Solution**: Use heredoc or file redirect:
```bash
# Instead of:
export SPARKPASS_PASSWORD="visible_password"

# Use:
./bin/sparkpass_main unlock vault.spass <<< "password"
# or
./bin/sparkpass_main unlock vault.spass < password_file
```

---

## Migration from Interactive-Only

If you have existing scripts that used `expect`, you can now simplify them:

### Before (with expect):
```bash
#!/usr/bin/expect -f
set password "my_password"
spawn ./bin/sparkpass_main unlock vault.spass
expect "Enter password: "
send "$password\\r"
expect eof
```

### After (pure bash):
```bash
#!/bin/bash
PASSWORD="my_password"
echo "$PASSWORD" | ./bin/sparkpass_main unlock vault.spass
```

**Result**: Simpler, faster, no expect dependency, more maintainable.

---

## Summary

**SparkPass now supports both worlds:**

- **Interactive security**: Argon2id 1GiB KDF + ML-KEM-1024 + SPARK formal verification
- **Automation friendly**: Stdin, env vars, CI/CD, cron jobs, scripts

Choose the password method that matches your security/convenience trade-off:

- Maximum security: Interactive TTY
- Automation: Stdin from secure file or password manager
- Testing only: Environment variable

All methods maintain the same cryptographic security - the difference is only in how the password reaches SparkPass.
