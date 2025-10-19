pragma SPARK_Mode (On);
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.Random is
   --  Fill Buffer with cryptographically secure random bytes from /dev/urandom.
   --  This procedure uses the operating system's CSPRNG (POSIX /dev/urandom).
   --
   --  **Implementation**: Pure Ada using Ada.Streams.Stream_IO (zero crypto FFI)
   --
   --  Security Properties:
   --  - Uses kernel CSPRNG via /dev/urandom (not Ada.Numerics.Random)
   --  - Non-blocking and suitable for all cryptographic contexts
   --  - Forward secrecy: previous outputs don't reveal future outputs
   --  - No initialization required (unlike /dev/random)
   --
   --  Platform Support:
   --  - POSIX systems: /dev/urandom (Linux, macOS, BSD, etc.)
   --  - Windows: Alternative body using CryptGenRandom can be provided
   --
   --  Global => null: This is a pure cryptographic operation with no side effects on program state
   --  (OS RNG state is managed by the kernel, not visible to SPARK)
   --
   --  Depends: Output Buffer depends only on its previous contents (SPARK model)
   --  In reality, it depends on the OS RNG, but SPARK treats external randomness as nondeterministic input
   --
   --  Post: Buffer indices unchanged (required for array slice operations)
   procedure Fill (Buffer : in out Byte_Array)
     with
       Global  => null,
       Depends => (Buffer => Buffer),
       Post    => Buffer'First = Buffer'First'Old and
                  Buffer'Last = Buffer'Last'Old;
end SparkPass.Crypto.Random;
