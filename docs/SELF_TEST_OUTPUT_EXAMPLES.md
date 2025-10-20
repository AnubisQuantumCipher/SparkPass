# SparkPass Self-Test Output Examples

This document provides reference examples of self-test outputs in various modes and scenarios.

## Table of Contents

1. [Fast Mode (Success)](#fast-mode-success)
2. [Comprehensive Mode (Success)](#comprehensive-mode-success)
3. [Verbose Mode](#verbose-mode)
4. [JSON Mode](#json-mode)
5. [Failure Scenarios](#failure-scenarios)
6. [CI/CD Integration Examples](#cicd-integration-examples)

---

## Fast Mode (Success)

Default mode, completes in < 5 seconds:

```
$ sparkpass self-test

SparkPass v1.0 Self-Test
================================================================================

[PASS] ALL TESTS PASSED

[1/4] Cryptographic Primitives

  [PASS] Argon2id KDF                [ 2.847 s]
  [PASS] HKDF-SHA-384
  [PASS] AES-256-GCM-SIV
  [PASS] ML-KEM-1024
  [PASS] ML-DSA-87
  [PASS] Random (CSPRNG)
    Tamper detection: [PASS] detected

[2/4] SparkPass Cryptography

  [PASS] Shamir Secret Sharing
  ⊙ Reed-Solomon FEC
  [PASS] Nonce Derivation
  [PASS] Key Wrapping
  [PASS] Zeroization

[3/4] Vault Operations

  [PASS] Key-Arena
  [PASS] Policy Engine

[4/4] Platform Integration

  ⊙ Platform-specific tests

================================================================================
Total Duration:  3.301 s
LibOQS: [PASS]

$ echo $?
0
```

**Notes**:
- Reed-Solomon skipped (⊙) to keep test duration under 5 seconds
- Platform tests skipped (not required for pass)
- Exit code 0 indicates all required tests passed

---

## Comprehensive Mode (Success)

Includes all tests, completes in < 15 seconds:

```
$ sparkpass self-test --comprehensive

SparkPass v1.0 Self-Test
================================================================================

[PASS] ALL TESTS PASSED

[1/4] Cryptographic Primitives

  [PASS] Argon2id KDF                [ 2.938 s]
  [PASS] HKDF-SHA-384
  [PASS] AES-256-GCM-SIV
  [PASS] ML-KEM-1024
  [PASS] ML-DSA-87
  [PASS] Random (CSPRNG)
    Tamper detection: [PASS] detected

[2/4] SparkPass Cryptography

  [PASS] Shamir Secret Sharing
  [PASS] Reed-Solomon FEC
  [PASS] Nonce Derivation
  [PASS] Key Wrapping
  [PASS] Zeroization

[3/4] Vault Operations

  [PASS] Key-Arena
  [PASS] Policy Engine

[4/4] Platform Integration

  ⊙ Platform-specific tests

================================================================================
Total Duration:  4.123 s
LibOQS: [PASS]

$ echo $?
0
```

**Notes**:
- Reed-Solomon now tested ([PASS])
- Slightly longer duration due to RS decoding (~50ms overhead)

---

## Verbose Mode

Shows usage hints and legend:

```
$ sparkpass self-test --verbose

SparkPass v1.0 Self-Test
================================================================================

[PASS] ALL TESTS PASSED

[1/4] Cryptographic Primitives

  [PASS] Argon2id KDF                [ 2.847 s]
  [PASS] HKDF-SHA-384
  [PASS] AES-256-GCM-SIV
  [PASS] ML-KEM-1024
  [PASS] ML-DSA-87
  [PASS] Random (CSPRNG)
    Tamper detection: [PASS] detected

[2/4] SparkPass Cryptography

  [PASS] Shamir Secret Sharing
  ⊙ Reed-Solomon FEC
  [PASS] Nonce Derivation
  [PASS] Key Wrapping
  [PASS] Zeroization

[3/4] Vault Operations

  [PASS] Key-Arena
  [PASS] Policy Engine

[4/4] Platform Integration

  ⊙ Platform-specific tests

================================================================================
Total Duration:  3.301 s
LibOQS: [PASS]

Legend: [PASS] Passed  ✗ Failed  ⊙ Skipped

To run comprehensive tests (including Reed-Solomon):
  sparkpass self-test --comprehensive

For JSON output (CI/CD integration):
  sparkpass self-test --json
```

---

## JSON Mode

Machine-readable output for CI/CD:

```json
$ sparkpass self-test --comprehensive --json
{
  "sparkpass_version": "2.0.8",
  "test_mode": "COMPREHENSIVE",
  "timestamp": " 1729123456",
  "system": {
    "os": "macOS",
    "arch": "arm64"
  },
  "results": {
    "passed": TRUE,
    "duration_seconds":  4.123000000 s
  },
  "tests": {
    "liboqs": TRUE,
    "argon2id": "SUCCEEDED",
    "hkdf": "SUCCEEDED",
    "aes_gcm_siv": "SUCCEEDED",
    "ml_kem": "SUCCEEDED",
    "ml_dsa": "SUCCEEDED",
    "random": "SUCCEEDED",
    "shamir": "SUCCEEDED",
    "reed_solomon": "SUCCEEDED",
    "nonce": "SUCCEEDED",
    "wrapping": "SUCCEEDED",
    "zeroization": "SUCCEEDED",
    "key_arena": "SUCCEEDED",
    "policy": "SUCCEEDED"
  }
}
```

**Parsing in CI/CD**:

```bash
# Extract pass/fail status
PASSED=$(sparkpass self-test --json | jq -r '.results.passed')
if [ "$PASSED" != "TRUE" ]; then
    echo "Tests failed!"
    exit 1
fi

# Extract duration for performance tracking
DURATION=$(sparkpass self-test --json | jq -r '.results.duration_seconds')
echo "Self-tests completed in ${DURATION}"

# Check specific test
ML_KEM=$(sparkpass self-test --json | jq -r '.tests.ml_kem')
if [ "$ML_KEM" != "SUCCEEDED" ]; then
    echo "ML-KEM test failed!"
    exit 1
fi
```

---

## Failure Scenarios

### Scenario 1: Argon2id Failure (Memory Allocation)

```
$ sparkpass self-test

SparkPass v1.0 Self-Test
================================================================================

✗ TESTS FAILED

[1/4] Cryptographic Primitives

  ✗ Argon2id KDF                [ 0.124 s, reduced params]
  ✗ HKDF-SHA-384
  ✗ AES-256-GCM-SIV
  ✗ ML-KEM-1024
  ✗ ML-DSA-87
  [PASS] Random (CSPRNG)
    Tamper detection: ✗ not detected

[2/4] SparkPass Cryptography

  ✗ Shamir Secret Sharing
  ⊙ Reed-Solomon FEC
  ✗ Nonce Derivation
  ✗ Key Wrapping
  ✗ Zeroization

[3/4] Vault Operations

  ✗ Key-Arena
  ✗ Policy Engine

[4/4] Platform Integration

  ⊙ Platform-specific tests

================================================================================
Total Duration:  0.235 s
LibOQS: [PASS]

$ echo $?
1
```

**Diagnosis**:
- Argon2id failed in both strong (1 GiB) and fallback (64 MiB) modes
- Cascade failure: All tests depending on Argon2id also failed
- Check available memory: `free -m` (Linux) or `vm_stat` (macOS)

**Action**: Increase available memory or close other applications

---

### Scenario 2: ML-DSA Tamper Detection Failure (Critical)

```
$ sparkpass self-test

SparkPass v1.0 Self-Test
================================================================================

✗ TESTS FAILED

[1/4] Cryptographic Primitives

  [PASS] Argon2id KDF                [ 2.847 s]
  [PASS] HKDF-SHA-384
  [PASS] AES-256-GCM-SIV
  [PASS] ML-KEM-1024
  [PASS] ML-DSA-87
  [PASS] Random (CSPRNG)
    Tamper detection: ✗ not detected

[2/4] SparkPass Cryptography

  [PASS] Shamir Secret Sharing
  ⊙ Reed-Solomon FEC
  [PASS] Nonce Derivation
  [PASS] Key Wrapping
  [PASS] Zeroization

[3/4] Vault Operations

  [PASS] Key-Arena
  [PASS] Policy Engine

[4/4] Platform Integration

  ⊙ Platform-specific tests

================================================================================
Total Duration:  3.301 s
LibOQS: [PASS]

$ echo $?
1
```

**Diagnosis**:
- ML-DSA signature creation succeeded
- ML-DSA signature verification succeeded
- **CRITICAL**: Tampered signature was not rejected
- This indicates broken signature verification

**Action**:
1. **Stop using this build immediately** (security critical)
2. Rebuild from clean state: `make clean && make`
3. Verify liboqs version: `brew info liboqs` (should be ≥ 0.9.0)
4. If problem persists, report security issue

---

### Scenario 3: Nonce Collision (Critical)

```
$ sparkpass self-test

SparkPass v1.0 Self-Test
================================================================================

✗ TESTS FAILED

[1/4] Cryptographic Primitives

  [PASS] Argon2id KDF                [ 2.847 s]
  [PASS] HKDF-SHA-384
  [PASS] AES-256-GCM-SIV
  [PASS] ML-KEM-1024
  [PASS] ML-DSA-87
  [PASS] Random (CSPRNG)
    Tamper detection: [PASS] detected

[2/4] SparkPass Cryptography

  [PASS] Shamir Secret Sharing
  ⊙ Reed-Solomon FEC
  ✗ Nonce Derivation
  ✗ Key Wrapping
  ✗ Zeroization

[3/4] Vault Operations

  ✗ Key-Arena
  ✗ Policy Engine

[4/4] Platform Integration

  ⊙ Platform-specific tests

================================================================================
Total Duration:  3.301 s
LibOQS: [PASS]

$ echo $?
1
```

**Diagnosis**:
- Nonce derivation produced collision (different inputs → same nonce)
- Key wrapping failed (depends on nonces)
- **CRITICAL**: Nonce reuse breaks AEAD security

**Action**:
1. **Stop using this build immediately** (security critical)
2. Check HKDF implementation: `git diff src/sparkpass/crypto/sparkpass-crypto-hkdf.adb`
3. Verify RNG is working: `sparkpass self-test --verbose | grep Random`
4. Rebuild and retest
5. If problem persists, report security issue

---

### Scenario 4: Reed-Solomon Decode Failure

```
$ sparkpass self-test --comprehensive

SparkPass v1.0 Self-Test
================================================================================

✗ TESTS FAILED

[1/4] Cryptographic Primitives

  [PASS] Argon2id KDF                [ 2.847 s]
  [PASS] HKDF-SHA-384
  [PASS] AES-256-GCM-SIV
  [PASS] ML-KEM-1024
  [PASS] ML-DSA-87
  [PASS] Random (CSPRNG)
    Tamper detection: [PASS] detected

[2/4] SparkPass Cryptography

  [PASS] Shamir Secret Sharing
  ✗ Reed-Solomon FEC
  [PASS] Nonce Derivation
  [PASS] Key Wrapping
  [PASS] Zeroization

[3/4] Vault Operations

  [PASS] Key-Arena
  [PASS] Policy Engine

[4/4] Platform Integration

  ⊙ Platform-specific tests

================================================================================
Total Duration:  4.123 s
LibOQS: [PASS]

$ echo $?
1
```

**Diagnosis**:
- Reed-Solomon encoding succeeded
- Reed-Solomon decoding failed (could not correct 8 errors)
- Indicates GF(256) arithmetic error or table corruption

**Action**:
1. Check for bit flips: `md5sum src/sparkpass/crypto/sparkpass-crypto-reedsolomon.adb`
2. Rebuild: `gprbuild -P sparkpass.gpr -f`
3. If problem persists, check memory integrity (possible hardware issue)

---

### Scenario 5: Policy Validation Failure (Security)

```
$ sparkpass self-test

SparkPass v1.0 Self-Test
================================================================================

✗ TESTS FAILED

[1/4] Cryptographic Primitives

  [PASS] Argon2id KDF                [ 2.847 s]
  [PASS] HKDF-SHA-384
  [PASS] AES-256-GCM-SIV
  [PASS] ML-KEM-1024
  [PASS] ML-DSA-87
  [PASS] Random (CSPRNG)
    Tamper detection: [PASS] detected

[2/4] SparkPass Cryptography

  [PASS] Shamir Secret Sharing
  ⊙ Reed-Solomon FEC
  [PASS] Nonce Derivation
  [PASS] Key Wrapping
  [PASS] Zeroization

[3/4] Vault Operations

  [PASS] Key-Arena
  ✗ Policy Engine

[4/4] Platform Integration

  ⊙ Platform-specific tests

================================================================================
Total Duration:  3.301 s
LibOQS: [PASS]

$ echo $?
1
```

**Diagnosis**:
- Policy engine validation failed
- Possible causes:
  - Invalid policy accepted (security invariant violated)
  - Touch ID alone allowed (violates software-only availability)
  - Serialization round-trip corrupted policy

**Action**:
1. Check policy module: `git diff src/sparkpass/vault/sparkpass-vault-policy.adb`
2. Verify Touch ID policy: Test should reject Touch ID alone
3. Rebuild and retest: `make clean && make test`
4. If problem persists, report security issue

---

## CI/CD Integration Examples

### GitHub Actions (Full Example)

```yaml
name: SparkPass Self-Test

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  self-test:
    runs-on: macos-latest

    steps:
      - name: Checkout repository
        uses: actions/checkout@v3

      - name: Install dependencies
        run: |
          brew update
          brew install gnat gprbuild liboqs openssl libsodium
          alr get

      - name: Build SparkPass
        run: |
          gprbuild -P sparkpass.gpr
          ls -lh bin/sparkpass

      - name: Run fast self-test
        id: fast_test
        run: |
          ./bin/sparkpass self-test
          echo "status=$?" >> $GITHUB_OUTPUT

      - name: Run comprehensive self-test
        id: comprehensive_test
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        run: |
          ./bin/sparkpass self-test --comprehensive --json > test_results.json
          cat test_results.json
          echo "status=$?" >> $GITHUB_OUTPUT

      - name: Parse test results
        if: steps.comprehensive_test.outcome == 'success'
        run: |
          PASSED=$(jq -r '.results.passed' test_results.json)
          DURATION=$(jq -r '.results.duration_seconds' test_results.json)

          echo "Tests passed: $PASSED"
          echo "Duration: $DURATION seconds"

          if [ "$PASSED" != "TRUE" ]; then
            echo "::error::Self-tests failed"
            exit 1
          fi

      - name: Upload test results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: test-results
          path: test_results.json

      - name: Comment PR with results
        if: github.event_name == 'pull_request' && always()
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(fs.readFileSync('test_results.json', 'utf8'));

            const comment = `## SparkPass Self-Test Results

            - **Status**: ${results.results.passed ? '[OK] Passed' : '[FAIL] Failed'}
            - **Duration**: ${results.results.duration_seconds} seconds
            - **Mode**: ${results.test_mode}

            <details>
            <summary>Individual Test Results</summary>

            \`\`\`json
            ${JSON.stringify(results.tests, null, 2)}
            \`\`\`

            </details>`;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });
```

---

### GitLab CI (Example)

```yaml
# .gitlab-ci.yml

stages:
  - build
  - test
  - deploy

variables:
  SPARKPASS_VERSION: "2.0.8"

build:
  stage: build
  image: ubuntu:22.04
  before_script:
    - apt-get update
    - apt-get install -y gnat gprbuild liboqs-dev libssl-dev libsodium-dev
  script:
    - gprbuild -P sparkpass.gpr
    - strip bin/sparkpass
  artifacts:
    paths:
      - bin/sparkpass
    expire_in: 1 week

test:fast:
  stage: test
  dependencies:
    - build
  script:
    - ./bin/sparkpass self-test
  allow_failure: false

test:comprehensive:
  stage: test
  dependencies:
    - build
  script:
    - ./bin/sparkpass self-test --comprehensive --json > test_results.json
    - cat test_results.json
  artifacts:
    reports:
      junit: test_results.json
    paths:
      - test_results.json
  allow_failure: false

deploy:production:
  stage: deploy
  dependencies:
    - build
    - test:comprehensive
  only:
    - main
  script:
    - echo "Deploying to production..."
    - ./scripts/deploy.sh
```

---

### Jenkins Pipeline (Example)

```groovy
pipeline {
    agent any

    environment {
        SPARKPASS_VERSION = '2.0.8'
    }

    stages {
        stage('Build') {
            steps {
                sh 'gprbuild -P sparkpass.gpr'
            }
        }

        stage('Fast Self-Test') {
            steps {
                script {
                    def result = sh(
                        script: './bin/sparkpass self-test',
                        returnStatus: true
                    )
                    if (result != 0) {
                        error("Fast self-test failed")
                    }
                }
            }
        }

        stage('Comprehensive Self-Test') {
            when {
                branch 'main'
            }
            steps {
                sh './bin/sparkpass self-test --comprehensive --json > test_results.json'
                archiveArtifacts artifacts: 'test_results.json', fingerprint: true

                script {
                    def results = readJSON file: 'test_results.json'
                    if (!results.results.passed) {
                        error("Comprehensive self-test failed")
                    }

                    echo "Self-tests passed in ${results.results.duration_seconds} seconds"
                }
            }
        }

        stage('Deploy') {
            when {
                branch 'main'
            }
            steps {
                sh './scripts/deploy.sh'
            }
        }
    }

    post {
        always {
            cleanWs()
        }
        failure {
            emailext(
                subject: "SparkPass Build Failed: ${env.JOB_NAME} - ${env.BUILD_NUMBER}",
                body: "Self-tests failed. Check ${env.BUILD_URL} for details.",
                to: 'team@example.com'
            )
        }
    }
}
```

---

## Performance Benchmarks

### Fast Mode (Apple M1 Max, 64 GB RAM)

```
Argon2id:              2.847s (87% of total)
HKDF:                  0.001s
AES-GCM-SIV:           0.003s
ML-KEM:                0.125s
ML-DSA:                0.234s
Random:                0.000s
Shamir:                0.012s
Nonce:                 0.002s
Wrapping:              0.008s
Key-Arena:             0.008s
Policy:                0.001s
Zeroization:           0.060s

Total:                 3.301s
```

---

### Comprehensive Mode (Apple M1 Max, 64 GB RAM)

```
Argon2id:              2.938s (71% of total)
HKDF:                  0.001s
AES-GCM-SIV:           0.003s
ML-KEM:                0.125s
ML-DSA:                0.234s
Random:                0.000s
Shamir:                0.012s
Reed-Solomon:          0.054s (added in comprehensive)
Nonce:                 0.002s
Wrapping:              0.008s
Key-Arena:             0.008s
Policy:                0.001s
Zeroization:           0.737s

Total:                 4.123s
```

**Notes**:
- Argon2id dominates execution time (1 GiB memory, 4 iterations)
- Reed-Solomon adds ~50ms (encoding + decoding + correction)
- Zeroization is instant (compiler optimizations verify memory clearing)

---

### Low-Memory System (2 GB RAM, Intel i5)

```
Argon2id:              1.234s (64 MiB fallback, reduced params)
[Other tests...]

Total:                 1.845s
```

**Notes**:
- Argon2id falls back to 64 MiB memory when system has < 1 GiB available
- Faster execution but less secure (acceptable for testing, not production)

---

## Interpreting Results

### Green Flags (Everything OK)

- [OK] All tests show [PASS] (passed)
- [OK] Tamper detection: [PASS] detected
- [OK] Exit code: 0
- [OK] Duration: < 5 seconds (fast) or < 15 seconds (comprehensive)
- [OK] Argon2id used strong params (1 GiB memory)

---

### Yellow Flags (Non-Critical Issues)

- [WARN] Reed-Solomon skipped (fast mode) — Use `--comprehensive` for full coverage
- [WARN] Platform tests skipped — Not required, but Touch ID unavailable
- [WARN] Argon2id used reduced params — System has low memory, acceptable for testing

---

### Red Flags (Critical Failures)

- 🚨 Tamper detection: ✗ not detected — **Security critical**, rebuild immediately
- 🚨 Nonce derivation: ✗ — **Security critical**, nonce collision detected
- 🚨 Policy engine: ✗ — **Security critical**, policy validation broken
- 🚨 Exit code: 1 — One or more tests failed, do not deploy
- 🚨 Zeroization: ✗ — Secrets not wiped, memory leak risk

---

## Support

For issues with self-tests:

- **Documentation**: [SELF_TEST_GUIDE.md](./SELF_TEST_GUIDE.md)
- **Issues**: https://github.com/sicarii/sparkpass/issues
- **Security**: https://github.com/sicarii/sparkpass/security (for critical failures)
