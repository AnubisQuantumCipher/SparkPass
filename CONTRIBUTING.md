# Contributing to SparkPass

Thank you for your interest in contributing to SparkPass! As a post-quantum cryptographic password manager, we maintain high standards for code quality, security, and correctness.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Environment](#development-environment)
- [Contribution Guidelines](#contribution-guidelines)
- [Security Policy](#security-policy)
- [Testing Requirements](#testing-requirements)
- [Pull Request Process](#pull-request-process)
- [Code Review Standards](#code-review-standards)

## Code of Conduct

Be respectful, professional, and constructive in all interactions. This is a security-critical project serving users who depend on cryptographic correctness.

## Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/SparkPass.git
   cd SparkPass
   ```
3. **Add upstream remote**:
   ```bash
   git remote add upstream https://github.com/ORIGINAL_OWNER/SparkPass.git
   ```
4. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```

## Development Environment

### Required Tools

- **GNAT Ada Compiler** 14.2.1+ (via Alire)
- **GPRbuild** 24.0.1+ (via Alire)
- **Alire** package manager (`alr`)
- **macOS** 13.0+ (for Touch ID support)
- **Xcode Command Line Tools** (for LAContext bindings)

### Dependencies

```bash
brew install liboqs openssl@3 libsodium
alr toolchain --select
```

### Building

```bash
./build.sh
```

## Contribution Guidelines

### What We Accept

[OK] **Security Improvements**
- Constant-time algorithm implementations
- Timing attack mitigations
- Side-channel analysis and fixes
- Cryptographic correctness improvements

[OK] **Bug Fixes**
- Memory safety issues
- SPARK contract violations
- Zeroization bugs
- Platform compatibility issues

[OK] **Documentation**
- Code comments with security rationale
- API documentation
- Security considerations
- Testing documentation

[OK] **Tests**
- Unit tests for cryptographic primitives
- Property-based tests for state machines
- Timing attack validation tests
- Fuzzing harnesses

### What We Don't Accept

[FAIL] **Breaking Changes** without prior discussion
[FAIL] **Non-constant-time** implementations of security-critical code
[FAIL] **Cryptographic algorithm changes** without peer review and citations
[FAIL] **GUI/Web interfaces** (SparkPass is CLI-only by design)
[FAIL] **Network features** (offline-only architecture)
[FAIL] **Telemetry or analytics** (privacy-first design)

## Security Policy

### Reporting Security Vulnerabilities

**DO NOT** open public issues for security vulnerabilities.

Email security reports to: **sic.tau@pm.me**

Include:
- Description of the vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (if any)

We aim to respond within 48 hours and provide a fix within 7 days for critical issues.

### Security Review Requirements

All cryptographic code changes require:

1. **Peer Review** by at least one cryptography expert
2. **SPARK Verification** (where applicable)
3. **Timing Attack Tests** with <5% variance
4. **Side-Channel Analysis** documentation
5. **References** to academic papers or standards (NIST FIPS, IETF RFCs, etc.)

## Testing Requirements

### Before Submitting a PR

Run the full test suite:

```bash
make test
```

This includes:
- **Cryptographic tests** (ML-KEM-1024, ML-DSA-87)
- **Property-based tests** (vault state machine)
- **Corruption resilience tests** (6 scenarios)
- **Fuzzing tests** (1000 mutations)
- **Timing attack tests** (constant-time validation)
- **Side-channel tests** (cache, branch, memory, spectre)

### Test Coverage Requirements

- **Security-critical code**: 100% coverage
- **Cryptographic primitives**: NIST KAT compliance
- **Constant-time operations**: <5% timing variance
- **Error handling**: All error paths tested

### Writing New Tests

Follow existing test patterns in `test/`:
- Use descriptive test names
- Document expected behavior
- Include failure scenarios
- Add security rationale comments

## Pull Request Process

### 1. Prepare Your Changes

```bash
# Update from upstream
git fetch upstream
git rebase upstream/main

# Run tests
make test

# Check SPARK contracts (if applicable)
gnatprove -P sparkpass.gpr
```

### 2. Commit Guidelines

**Commit Message Format:**
```
<type>: <summary>

<body>

<footer>
```

**Types:**
- `sec:` Security improvements
- `fix:` Bug fixes
- `feat:` New features
- `test:` Test additions
- `docs:` Documentation
- `perf:` Performance improvements

**Example:**
```
sec: Implement constant-time entry lookup

Replace early-exit loop with full iteration to prevent
position-based timing leaks. Always decrypt all entry keys
to eliminate conditional decryption timing variance.

Fixes 18% timing variance discovered in timing attack tests.
See test/test_timing_attacks.adb for validation.

References:
- Kocher (1996): Timing Attacks on Implementations of Diffie-Hellman, RSA, DSS
- TESTING.md: Timing Attack Validation section
```

### 3. Open Pull Request

**PR Title:** Same as commit summary
**PR Description:** Include:
- What changed and why
- Security implications
- Test results (paste `make test` output)
- References to issues (if any)

### 4. Code Review

Expect reviews focusing on:
- **Cryptographic correctness**
- **Memory safety** (SPARK contracts, zeroization)
- **Constant-time** properties
- **Error handling** completeness
- **Documentation** quality

## Code Review Standards

### For Reviewers

Check for:

#### Security
- [ ] Constant-time implementations verified
- [ ] Zeroization on all error paths
- [ ] No conditional branches on secrets
- [ ] SPARK contracts enforced (where applicable)

#### Correctness
- [ ] NIST compliance (for crypto primitives)
- [ ] State machine invariants preserved
- [ ] Error codes correctly propagated
- [ ] Edge cases handled

#### Quality
- [ ] Code comments explain security rationale
- [ ] Academic references cited
- [ ] Tests added for new code
- [ ] Documentation updated

### For Contributors

Address review feedback by:
1. Making requested changes in new commits
2. Replying to each comment
3. Pushing updates to the same branch
4. Re-requesting review when ready

## Development Tips

### SPARK Mode

SparkPass uses SPARK for formal verification of security-critical code:

```ada
pragma SPARK_Mode (On);

procedure Secure_Operation (Input : Byte_Array)
  with Pre  => Input'Length > 0,
       Post => (for all I in Result'Range => Result (I) = 0);
```

Learn SPARK: https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/

### Constant-Time Programming

Always iterate full length, never early-exit:

```ada
--  [PASS] GOOD: Constant-time
for Index in 1 .. Entry_Count loop
   if Match (Index) then
      Result := Get_Data (Index);
   end if;
   --  NO EXIT - continue to prevent timing leaks
end loop;

--  ✗ BAD: Timing leak
for Index in 1 .. Entry_Count loop
   if Match (Index) then
      Result := Get_Data (Index);
      exit;  -- Leaks position info via timing
   end if;
end loop;
```

### Zeroization

Always wipe secrets on ALL paths:

```ada
declare
   Secret : Key_Array := (others => 0);
begin
   Derive_Key (Secret);
   --  ... use secret ...
   Zeroize.Wipe (Secret);  --  Normal path
exception
   when others =>
      Zeroize.Wipe (Secret);  --  Error path
      raise;
end;
```

## Questions?

Contact the maintainer: **sic.tau@pm.me**

Read the docs:
- [README.md](README.md) - Project overview
- [SECURITY.md](SECURITY.md) - Security architecture
- [TESTING.md](TESTING.md) - Testing guide
- [DEPLOYMENT.md](DEPLOYMENT.md) - Deployment guide

Thank you for helping make SparkPass more secure!
