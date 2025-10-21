pragma SPARK_Mode (On);

with Spark_Argon2id.Internal_Types; use Spark_Argon2id.Internal_Types;
with Spark_Argon2id.Fill;  use Spark_Argon2id.Fill;

--  ================================================================
--  Argon2id Finalization (RFC 9106 Section 3.1.3)
--  ================================================================
--
--  **Purpose**: Extract final hash from memory state
--
--  **Algorithm** (RFC 9106 Section 3.1.3):
--
--    1. If parallelism p > 1:
--       C ← XOR of final blocks from all lanes:
--       C = Memory[0][lane_length-1] ⊕ Memory[1][lane_length-1] ⊕ ...
--
--    2. If parallelism p = 1 (SparkPass case):
--       C ← Memory[0][lane_length-1]  (just the last block)
--
--    3. Tag ← H'(C, output_length)  (Apply variable-length KDF)
--
--  Multi‑lane behavior:
--    - If Active_Lanes = 1: C ← Memory[0][lane_length − 1]
--    - If Active_Lanes > 1: C ← XOR of Memory[l][lane_length − 1] across lanes l ∈ [0, p)
--
--  **Implementation**:
--    Step 1: Extract_Final_Block → Get Memory[16,383]
--    Step 2: Block_To_Bytes → Convert Block to Byte_Array
--    Step 3: Finalize → Apply H'(block_bytes, 32) → Output_Key
--
--  **Security Properties**:
--    - Deterministic: Same memory state → same output
--    - One-way: Cannot reverse to find memory state
--    - Based on Blake2b-512 security (via H')
--
--  **SPARK Properties**:
--    - Pure functions (Global => null)
--    - Bounded types (all indices provable)
--    - Target: ~20 VCs (100%)
--
--  **Source**: RFC 9106 Section 3.1.3
--  ================================================================

private package Spark_Argon2id.Finalize with
   SPARK_Mode => On
is

   ------------------------------------------------------------
   --  Extract_Final_Block
   ------------------------------------------------------------

   --  Extract the final block from memory state (C in RFC)
   --
   --  **Algorithm** (RFC 9106 Section 3.1.3):
   --    For p=1: C = Memory[0][lane_length-1]
   --    For p>1: C = Memory[0][q-1] ⊕ Memory[1][q-1] ⊕ ... ⊕ Memory[p-1][q-1]
   --
   --  **SparkPass Case** (p=1):
   --    Simply return the last block: Memory[Active_Blocks_Per_Lane - 1]
   --
   --  **Parameters**:
   --    Memory : Final memory state after Fill_Memory
   --    Output : Final block (C)
   --
   --  **Preconditions**:
   --    - Memory has correct bounds [0..Active_Blocks_Per_Lane-1]
   --
   --  **Postconditions**:
   --    - Output contains last block
   --
   --  **Example** (Test_Medium: 16,384 blocks):
   --    Output := Memory[16,383]
   --
   --  **Source**: RFC 9106 Section 3.1.3

   procedure Extract_Final_Block (
      Memory                    : Memory_State;
      Active_Lanes              : Positive;
      Effective_Blocks_Per_Lane : Positive;
      Output                    : out Block
   ) with
      Global => null,
      Pre    => Active_Lanes in 1 .. Parallelism and then
                Effective_Blocks_Per_Lane in 1 .. Active_Blocks_Per_Lane,
      Post   => True;  -- Output is always extracted successfully

   ------------------------------------------------------------
   --  Block_To_Bytes
   ------------------------------------------------------------

   --  Convert Block (128 x U64) to Byte_Array (1024 bytes)
   --
   --  **Layout**: Little-endian conversion
   --    Block[0] → Bytes[1..8]    (word 0, 8 bytes)
   --    Block[1] → Bytes[9..16]   (word 1, 8 bytes)
   --    ...
   --    Block[127] → Bytes[1017..1024]  (word 127, 8 bytes)
   --
   --  **Parameters**:
   --    Input  : Block to convert (128 x U64)
   --    Output : Byte array (1024 bytes)
   --
   --  **Preconditions**:
   --    - Output'Length = 1024 (one block)
   --    - Output'First = 1 (for overflow proofs)
   --
   --  **Postconditions**:
   --    - Output length unchanged
   --    - Each U64 word converted to 8 bytes (little-endian)

  procedure Block_To_Bytes (
      Input  : Block;
      Output : out Byte_Array
   ) with
      Global => null,
      Relaxed_Initialization => Output,
      Pre    => Output'Length = Block_Size_Bytes and
                Output'First = 1,
      Post   => Output'Length = Block_Size_Bytes;

   ------------------------------------------------------------
   --  Finalize
   ------------------------------------------------------------

   --  Produce final output key from memory state
   --
   --  **Algorithm** (RFC 9106 Section 3.1.3):
   --    1. C ← Extract final block (last block for p=1)
   --    2. Convert C to byte array (1024 bytes)
   --    3. Tag ← H'(C, output_length)
   --
   --  **Parameters**:
   --    Memory        : Final memory state after Fill_Memory
   --    Output_Length : Desired output length in bytes
   --    Output        : Final derived key
   --
   --  **Preconditions**:
   --    - Memory has correct bounds
   --    - Output_Length matches Output'Length
   --
   --  **Postconditions**:
   --    - Output contains derived key
   --    - Output length unchanged
   --
--  Example:
--    C_Block := XOR over lanes of the last block of each lane
--    C_Bytes := Block_To_Bytes(C_Block)  -- 1024 bytes
--    Output  := H'(C_Bytes, Output_Length)
   --
   --  **Source**: RFC 9106 Section 3.1.3

   procedure Finalize (
      Memory                    : Memory_State;
      Active_Lanes              : Positive;
      Effective_Blocks_Per_Lane : Positive;
      Output_Length             : Positive;
      Output                    : out Byte_Array
   ) with
      Global => null,
      Pre    => Active_Lanes in 1 .. Parallelism and
                Effective_Blocks_Per_Lane in 1 .. Active_Blocks_Per_Lane and
                Output'Length = Output_Length and
                Output'First = 1,
      Post   => Output'Length = Output_Length;

end Spark_Argon2id.Finalize;
