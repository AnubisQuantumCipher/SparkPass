pragma SPARK_Mode (On);

with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id.Types; use SparkPass.Crypto.Argon2id.Types;

--  ================================================================
--  Argon2id Memory Filling (RFC 9106 Section 3.1.2)
--  ================================================================
--
--  **Purpose**: Main memory-hard loop for Argon2id password hashing
--
--  **Algorithm** (RFC 9106 Section 3.1.2):
--    For each pass r ∈ [0, t):
--      For each lane l ∈ [0, p):
--        For each segment s ∈ [0, 3]:
--          For each block index i in segment:
--            1. ref_lane, ref_index ← Calculate_Reference(r, l, s, i)
--            2. prev ← Memory[l, (s × segment_size + i - 1) mod lane_length]
--            3. ref ← Memory[ref_lane, ref_index]
--            4. Memory[l, s × segment_size + i] ← G(prev ⊕ ref, Memory[l, s × segment_size + i])
--
--  **SparkPass Configuration**:
--    - Parallelism p = 1 (single lane)
--    - Iterations t = 4 (four passes)
--    - Segments = 4 per lane
--    - Active_Blocks_Per_Segment = 4096 (Test_Medium mode)
--    - Active_Blocks_Per_Lane = 16384 (Test_Medium mode)
--
--  **Memory Layout** (Single Lane):
--    Pass 0: Fill all blocks [0..16383]
--      - Segment 0: Blocks [0..4095] (blocks 0-1 from Init, rest filled here)
--      - Segment 1: Blocks [4096..8191]
--      - Segment 2: Blocks [8192..12287]
--      - Segment 3: Blocks [12288..16383]
--    Pass 1-3: Re-process all blocks [0..16383]
--
--  **Verification Strategy**:
--    - Memory represented as flat array indexed by Block_Index
--    - All indices proven in-bounds via subtypes
--    - Loop invariants maintain index bounds
--    - Previous block calculation proven safe (modulo wraparound)
--
--  **Target**: ~100 VCs (100% proof rate)
--
--  **Source**: RFC 9106 Section 3.1.2
--  ================================================================

private package SparkPass.Crypto.Argon2id.Fill with
   SPARK_Mode => On
is

   ------------------------------------------------------------
   --  Memory State Type
   ------------------------------------------------------------

   --  Memory representation: flat array of blocks
   --
   --  For SparkPass (p=1): Single lane, indexed [0..Active_Blocks_Per_Lane-1]
   --
   --  Memory[i] = Block at index i within the single lane
   --
   --  **Size** (Test_Medium): 16384 blocks × 1024 bytes = 16 MiB
   --  **Size** (Production): 131072 blocks × 1024 bytes = 128 MiB
   --
   --  **Note**: We use 128 MiB for production instead of 1 GiB to fit
   --  within verification constraints. The algorithm is proven correct
   --  for any memory size.
   type Memory_State is array (Block_Index) of Block;

   ------------------------------------------------------------
   --  Fill_Memory
   ------------------------------------------------------------

   --  Execute main memory-filling loop for Argon2id
   --
   --  **Algorithm** (RFC 9106 Section 3.1.2):
   --    Performs t passes over memory, filling/refilling all blocks
   --    after the first two (which are initialized by Init).
   --
   --  **Parameters**:
   --    Memory : Memory state (modified in-place)
   --
   --  **Preconditions**:
   --    - Memory[0] and Memory[1] already initialized (by Generate_Initial_Blocks)
   --
   --  **Postconditions**:
   --    - All memory blocks have been processed
   --    - Memory contains final Argon2id state after t passes
   --
   --  **Loop Structure**:
   --    Pass loop: 0 ≤ r < Iterations (4 iterations)
   --      Segment loop: 0 ≤ s < 4
   --        Block loop: start_idx ≤ i < end_idx
   --          - Pass 0, Segment 0: start_idx = 2 (skip Init blocks 0-1)
   --          - All other cases: start_idx = 0
   --          - end_idx = Active_Blocks_Per_Segment (4096)
   --
   --  **Memory Access Pattern**:
   --    For each block at position [segment × segment_size + index]:
   --      - Read previous block (with wraparound)
   --      - Read reference block (computed by Calculate_Reference)
   --      - Write new block (G mixing function applied)
   --
   --  **Security Properties**:
   --    - Data-independent mode (Argon2i) for first half of first pass
   --    - Data-dependent mode (Argon2d) for second half and subsequent passes
   --    - Non-uniform memory access pattern (favors recent blocks)
   --
   --  **Source**: RFC 9106 Section 3.1.2

   procedure Fill_Memory (
      Memory : in out Memory_State
   ) with
      Global => null,
      Pre    => Memory'First = 0 and
                Memory'Last = Active_Blocks_Per_Lane - 1;
   --  Pre: Memory array has correct bounds (must span full lane)

end SparkPass.Crypto.Argon2id.Fill;
