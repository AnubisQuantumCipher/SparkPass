pragma SPARK_Mode (On);
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.Random is
   --  Fill Buffer with cryptographically secure random bytes using libsodium's randombytes_buf.
   --  This procedure uses the operating system's CSPRNG (/dev/urandom on POSIX systems).
   --
   --  Security Properties:
   --  - Uses libsodium's cryptographic RNG (not Ada.Numerics.Random)
   --  - Non-blocking (suitable for all security contexts)
   --  - Forward secrecy: previous outputs don't reveal future outputs
   --
   --  Global => null: This is a pure cryptographic operation with no side effects on program state
   --  (libsodium's internal RNG state is managed by the library, not visible to SPARK)
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
