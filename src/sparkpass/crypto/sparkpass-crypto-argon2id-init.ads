pragma SPARK_Mode (On);

with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id.Types; use SparkPass.Crypto.Argon2id.Types;

--  ================================================================
--  Argon2id Memory Initialization (RFC 9106 Section 3.4)
--  ================================================================
--
--  **Purpose**: Generate first two blocks per lane from H₀
--
--  **Algorithm** (RFC 9106 Section 3.4):
--    For each lane i ∈ [0, p):
--      B[i][0] = H'(1024, H₀ || LE32(0) || LE32(i))
--      B[i][1] = H'(1024, H₀ || LE32(1) || LE32(i))
--
--  **Example** (Parallelism = 1):
--    Lane 0:
--      B[0][0] = H'(1024, H₀ || [0,0,0,0] || [0,0,0,0])
--      B[0][1] = H'(1024, H₀ || [1,0,0,0] || [0,0,0,0])
--
--  **Security Properties**:
--    - Deterministic: Same H₀ always produces same blocks
--    - Pseudorandom: Blocks are indistinguishable from random
--    - Based on Blake2b-512 security properties
--
--  **SPARK Properties**:
--    - Pure function (Global => null)
--    - Bounded types (Lane_Index range proven)
--    - Target: 50/50 VCs (100%)
--
--  **Source**: RFC 9106 Section 3.4, Figure 5
--  ================================================================

private package SparkPass.Crypto.Argon2id.Init with
   SPARK_Mode => On
is

   ------------------------------------------------------------
   --  Initial_Blocks Type
   ------------------------------------------------------------

   --  Storage for first two blocks of a lane
   --
   --  For parallelism p, we generate 2p blocks total:
   --  - p lanes × 2 blocks/lane = 2p blocks
   --
   --  For SparkPass (p=1): Just 2 blocks (Block_0, Block_1)
   type Initial_Blocks is record
      Block_0 : Block := Zero_Block;  -- B[lane][0]
      Block_1 : Block := Zero_Block;  -- B[lane][1]
   end record;

   ------------------------------------------------------------
   --  Generate_Initial_Blocks
   ------------------------------------------------------------

   --  Generate first two blocks for a lane from H₀
   --
   --  **Algorithm** (RFC 9106 Section 3.4):
   --    Input = H₀ || LE32(block_index) || LE32(lane_index)
   --    Block = H'(1024, Input)
   --
   --  **Parameters**:
   --    H0         : Initial hash (64 bytes from H₀ computation)
   --    Lane       : Lane index (0 for SparkPass)
   --    Output     : Initial blocks structure (Block_0, Block_1)
   --
   --  **Preconditions**:
   --    - H0 length is exactly 64 bytes
   --    - Lane is valid (in Lane_Index range)
   --
   --  **Postconditions**:
   --    - Output blocks are filled with pseudorandom data
   --
   --  **Example** (Lane 0):
   --    H0_Input := H₀ || [0,0,0,0] || [0,0,0,0]  (72 bytes)
   --    Block_0 := H'(1024, H0_Input)
   --
   --    H1_Input := H₀ || [1,0,0,0] || [0,0,0,0]  (72 bytes)
   --    Block_1 := H'(1024, H1_Input)
   --
   --  **Source**: RFC 9106 Section 3.4

   procedure Generate_Initial_Blocks (
      H0     : Byte_Array;
      Lane   : Lane_Index;
      Output : out Initial_Blocks
   ) with
      Global => null,
      Pre    => H0'Length = 64 and H0'First = 1 and Lane in Lane_Index,
      Post   => Output.Block_0'Initialized and Output.Block_1'Initialized;
      --  Both blocks are fully initialized by H' variable-length hash function

end SparkPass.Crypto.Argon2id.Init;
