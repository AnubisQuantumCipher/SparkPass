pragma SPARK_Mode (On);

with SparkPass.Crypto.Zeroize;
with SparkPass.Crypto.Argon2id.Types;     use SparkPass.Crypto.Argon2id.Types;
with SparkPass.Crypto.Argon2id.H0;        use SparkPass.Crypto.Argon2id.H0;
with SparkPass.Crypto.Argon2id.Init;      use SparkPass.Crypto.Argon2id.Init;
with SparkPass.Crypto.Argon2id.Fill;      use SparkPass.Crypto.Argon2id.Fill;
with SparkPass.Crypto.Argon2id.Finalize;  use SparkPass.Crypto.Argon2id.Finalize;

package body SparkPass.Crypto.Argon2id is

   ------------------------------------------------------------
   --  Derive (Complete Implementation - Phase 2.8)
   ------------------------------------------------------------

   --  Argon2id key derivation function (RFC 9106)
   --
   --  **Algorithm** (RFC 9106 Section 3):
   --    1. H₀ ← Initial hash from password, salt, and parameters
   --    2. Initialize first two blocks per lane from H₀
   --    3. Fill remaining memory using compression function G
   --    4. Iterate over memory t times (4 passes)
   --    5. Extract final hash from last block
   --
   --  **SparkPass Configuration**:
   --    - Parallelism p = 1 (single lane)
   --    - Iterations t = 4
   --    - Memory = 16 MiB (Test_Medium) or 128 MiB (Production)
   --    - Output = 32 bytes
   --
   --  **Security Properties**:
   --    - Memory-hard: Resistant to GPU/ASIC attacks
   --    - Side-channel resistant: Data-independent mode in first half of first pass
   --    - Deterministic: Same inputs always produce same output
   --
   --  **Source**: RFC 9106 Section 3

   procedure Derive
     (Password : Byte_Array;
      Params   : Parameters;
      Output   : out Key_Array;
      Success  : out Boolean)
   is
      --  H₀ initial hash (64 bytes, Blake2b-512 output)
      H0 : Byte_Array (1 .. 64) := (others => 0);

      --  Initial blocks for lane 0
      Init_Blocks : Initial_Blocks;

      --  Main memory state (Test_Medium: 16 MiB = 16,384 blocks)
      --  MUST be zero-initialized for determinism! Uninitialized memory causes
      --  non-deterministic output because Fill_Memory may read uninitialized blocks
      Memory : Memory_State := (others => (others => 0));

      --  Final output buffer (32 bytes)
      Final_Output : Byte_Array (1 .. 32);

   begin
      --  Initialize output to safe default (fail-closed)
      Output := (others => 0);
      Success := False;

      ------------------------------------------------------------
      --  Step 1: Compute H₀ initial hash (RFC 9106 Section 3.4)
      ------------------------------------------------------------

      --  H₀ = Blake2b-512(parameters || password || salt)
      --  Accept the requested parallelism from the caller but, for now, assert
      --  it matches the implementation's supported lane count.  This keeps the
      --  behaviour identical while laying the groundwork for future multi-lane
      --  support.
      declare
         Requested_Lanes : constant Positive :=
           Positive (Integer (Params.Parallelism));
      begin
         pragma Assert (Requested_Lanes = Parallelism);
         Compute_H0 (
            Password    => Password,
            Salt        => Params.Salt,
            Parallelism => Requested_Lanes,
            Tag_Length  => 32,  -- Output length (Key_Array'Length)
            Memory_KiB  => Positive (Params.Memory_Cost),
            Iterations  => Positive (Params.Iterations),
            H0_Out      => H0
         );
      end;

      ------------------------------------------------------------
      --  Step 2: Initialize first two blocks (RFC 9106 Section 3.4)
      ------------------------------------------------------------

      --  B[0][0] = H'(1024, H₀ || LE32(0) || LE32(0))
      --  B[0][1] = H'(1024, H₀ || LE32(1) || LE32(0))
      Generate_Initial_Blocks (
         H0     => H0,
         Lane   => 0,  -- SparkPass uses single lane (p=1)
         Output => Init_Blocks
      );

      --  Copy initial blocks to memory
      --  Note: Generate_Initial_Blocks postcondition proves Block_0 and Block_1
      --  are initialized, so SPARK can verify Memory(0) and Memory(1) are initialized
      Memory (0) := Init_Blocks.Block_0;
      Memory (1) := Init_Blocks.Block_1;

      ------------------------------------------------------------
      --  Step 3: Fill memory (RFC 9106 Section 3.1.2)
      ------------------------------------------------------------

      --  Perform t passes over memory, filling all blocks
      --  Pass 0: Fill blocks [2..Active_Blocks_Per_Lane-1]
      --  Pass 1-3: Re-process all blocks [0..Active_Blocks_Per_Lane-1]
      --
      --  Memory is fully zero-initialized at declaration (line 53), and blocks 0-1
      --  are explicitly set above. SPARK can now prove all initialization invariants
      --  without manual assumptions.

      Fill_Memory (Memory => Memory);

      ------------------------------------------------------------
      --  Step 4: Finalize - extract output (RFC 9106 Section 3.1.3)
      ------------------------------------------------------------

      --  C ← Memory[0][Active_Blocks_Per_Lane - 1]  (last block, p=1)
      --  Tag ← H'(C, 32)
      SparkPass.Crypto.Argon2id.Finalize.Finalize (
         Memory        => Memory,
         Output_Length => 32,
         Output        => Final_Output
      );

      --  Copy to output parameter
      Output := Final_Output;

      --  Success!
      Success := True;

      ------------------------------------------------------------
      --  Zeroize sensitive data
      ------------------------------------------------------------

      --  Clear all sensitive intermediate values
      SparkPass.Crypto.Zeroize.Wipe (H0);
      SparkPass.Crypto.Zeroize.Wipe (Final_Output);
      --  Note: Memory is not zeroized here (too large), but goes out of scope

   exception
      when others =>
         --  Fail-closed: zeroize on any error
         Output := (others => 0);
         SparkPass.Crypto.Zeroize.Wipe (H0);
         SparkPass.Crypto.Zeroize.Wipe (Final_Output);
         Success := False;
         raise;
   end Derive;

   ------------------------------------------------------------
   --  Zeroize
   ------------------------------------------------------------

   --  Securely erase key material from memory
   --
   --  Uses cryptographic zeroization (not compiler-optimizable)
   --  via SparkPass.Crypto.Zeroize.Wipe, which calls libsodium's
   --  sodium_memzero with memory barrier.
   --
   --  **Security Property**: Key material is overwritten with zeros
   --  **Side-Channel Resistance**: No data-dependent branches

   procedure Zeroize (Value : in out Key_Array) is
   begin
      SparkPass.Crypto.Zeroize.Wipe (Value);
   end Zeroize;

end SparkPass.Crypto.Argon2id;
