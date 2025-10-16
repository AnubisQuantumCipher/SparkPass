# SparkPass Verification & Test Notes

1. **Static proofs**: Run `gnatprove -P sparkpass.gpr --mode=check --level=2` once GNATprove is available.
2. **CLI smoke**: Exercise the passport lifecycle with a throwaway file:
   ```bash
   ./bin/sparkpass init /tmp/passport.vault demo-password
   ./bin/sparkpass bump /tmp/passport.vault demo-password
   ```
3. **Parser fuzzing**: Feed malformed inputs into `Vault.Storage.Load` using a separate Ada/SPARK harness or external fuzzer—ensure the routine returns `Integrity_Error` or `Format_Error` without reading beyond bounds.

Additional differential tests can be layered once the post-quantum primitives are bound to real implementations.
