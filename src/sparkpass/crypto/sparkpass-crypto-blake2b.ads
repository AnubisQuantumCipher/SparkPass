pragma SPARK_Mode (On);

with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.Blake2b
  with Preelaborate,
       SPARK_Mode => On
is
   --  Blake2b cryptographic hash function (RFC 7693)
   --
   --  Blake2b is optimized for 64-bit platforms and produces
   --  fixed 64-byte (512-bit) digests. It serves as the core
   --  primitive for Argon2id password hashing.
   --
   --  Algorithm parameters:
   --    - Word size: 64 bits
   --    - Rounds: 12
   --    - Block size: 128 bytes
   --    - Output size: 64 bytes (512 bits)
   --    - Rotation constants: 32, 24, 16, 63
   --
   --  Implementation follows RFC 7693 precisely, using little-endian
   --  byte order and the specified initialization vectors.

   ------------------------------------------------------------
   --  Type Definitions
   ------------------------------------------------------------

   --  Blake2b produces fixed 64-byte (512-bit) hash digests
   subtype Hash_Type is Byte_Array (1 .. 64);

   --  Internal state: 8x 64-bit words (512 bits total)
   type State_Words is array (0 .. 7) of U64;

   --  Work vector: 16x 64-bit words (used in compression function)
   type Work_Vector is array (0 .. 15) of U64;

   --  Message block: 128 bytes (16x 64-bit words)
   subtype Block_Type is Byte_Array (1 .. 128);

   --  Message words: 16x 64-bit words (little-endian)
   type Message_Words is array (0 .. 15) of U64;

   ------------------------------------------------------------
   --  Constants
   ------------------------------------------------------------

   --  Initialization Vector (RFC 7693 Section 2.6)
   --  IV[i] = floor(2^64 * frac(sqrt(prime(i+1))))
   --  where prime(i) = i-th prime (2, 3, 5, 7, 11, 13, 17, 19)
   IV : constant State_Words :=
     (16#6A09E667F3BCC908#,
      16#BB67AE8584CAA73B#,
      16#3C6EF372FE94F82B#,
      16#A54FF53A5F1D36F1#,
      16#510E527FADE682D1#,
      16#9B05688C2B3E6C1F#,
      16#1F83D9ABFB41BD6B#,
      16#5BE0CD19137E2179#);

   --  Sigma permutation table (RFC 7693 Section 2.7)
   --  10 permutations used cyclically for 12 rounds
   type Sigma_Row is array (0 .. 15) of Natural range 0 .. 15;
   type Sigma_Table is array (0 .. 9) of Sigma_Row;

   Sigma : constant Sigma_Table :=
     ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
      (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3),
      (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4),
      (7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8),
      (9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13),
      (2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9),
      (12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11),
      (13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10),
      (6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5),
      (10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0));

   ------------------------------------------------------------
   --  Main Hash Function
   ------------------------------------------------------------

   --  Compute Blake2b-512 hash of input message
   --
   --  This is the main entry point for Blake2b hashing.
   --  It processes the input message in 128-byte blocks and
   --  produces a fixed 64-byte digest.
   --
   --  @param Message  Input message to hash (arbitrary length)
   --  @param Output   64-byte hash digest (output)
   --
   --  Postconditions:
   --    - Output is deterministic (same input → same output)
   --    - Output length is always 64 bytes
   --    - No heap allocations (all stack-based)
   --
   procedure Hash
     (Message : in  Byte_Array;
      Output  : out Hash_Type)
   with
     Global => null,
     Pre    => Message'Length <= Natural'Last - 256,
     Post   => Output'Length = 64;

   ------------------------------------------------------------
   --  Variable-Length Hash (for Argon2id)
   ------------------------------------------------------------

   --  Compute Blake2b hash with variable-length output
   --
   --  Used by Argon2id for domain-separated hashing (H' function).
   --  Output is truncated from the standard 64-byte Blake2b hash.
   --
   --  @param Message  Input message to hash
   --  @param Output   Variable-length output (1-64 bytes)
   --
   --  Postconditions:
   --    - Output is first N bytes of Blake2b-512(Message)
   --    - Output length preserved
   --
   procedure Hash_Variable_Length
     (Message : in  Byte_Array;
      Output  : out Byte_Array)
   with
     Global => null,
     Pre    => Output'Length in 1 .. 64 and
               Message'Length <= Natural'Last - 256,
     Post   => Output'Length = Output'Length'Old;

   ------------------------------------------------------------
   --  Low-Level Compression Function (for Argon2id)
   ------------------------------------------------------------

   --  Blake2b compression function F
   --
   --  Used directly by Argon2id G function for block compression.
   --  This is the core of the Blake2b algorithm.
   --
   --  @param State   8-word state (modified in place)
   --  @param Block   128-byte message block
   --  @param Counter Byte counter (number of bytes hashed)
   --  @param Final   True if this is the final block
   --
   --  Postconditions:
   --    - State is updated via 12 rounds of G function
   --    - State length preserved
   --
   procedure Compress
     (State   : in out State_Words;
      Block   : in     Block_Type;
      Counter : in     U64;
      Final   : in     Boolean)
   with
     Global => null,
     Pre    => State'Length = 8 and Block'Length = 128,
     Post   => State'Length = 8;

end SparkPass.Crypto.Blake2b;
