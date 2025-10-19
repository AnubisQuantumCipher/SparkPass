pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with SparkPass.Crypto.Argon2id.Index; use SparkPass.Crypto.Argon2id.Index;
with SparkPass.Crypto.Argon2id.Mix;   use SparkPass.Crypto.Argon2id.Mix;

package body SparkPass.Crypto.Argon2id.Fill with
   SPARK_Mode => On
is

   ------------------------------------------------------------
   --  Helper Functions
   ------------------------------------------------------------

   --  Calculate previous block index with wraparound
   --
   --  RFC 9106 Section 3.1.2: prev = Memory[l, (i - 1) mod lane_length]
   --
   --  For segment s, block i:
   --    current_index = s × segment_size + i
   --    prev_index = (current_index - 1) mod lane_length
   --
   --  Special case: First block of lane (i=0, s=0)
   --    Wraps to last block of lane
   --
   --  @param Segment Current segment [0..3]
   --  @param Index Block index within segment [0..segment_size)
   --  @return Previous block index [0..Active_Blocks_Per_Lane)
   function Calculate_Prev_Index (
      Segment : Segment_Index;
      Index   : Natural
   ) return Block_Index
   with
      Global => null,
      Pre    => Index < Active_Blocks_Per_Segment,
      Post   => Calculate_Prev_Index'Result in Block_Index;

   function Calculate_Prev_Index (
      Segment : Segment_Index;
      Index   : Natural
   ) return Block_Index
   is
      Current_Index : Natural;
      Prev_Index    : Block_Index;
   begin
      --  Calculate absolute block index
      Current_Index := Segment * Active_Blocks_Per_Segment + Index;

      --  Calculate previous index with wraparound
      --  Special case: If Current_Index = 0, wrap to last block
      if Current_Index = 0 then
         Prev_Index := Active_Blocks_Per_Lane - 1;
      else
         Prev_Index := Current_Index - 1;
      end if;

      return Prev_Index;
   end Calculate_Prev_Index;

   --  Calculate current block index (where we're writing)
   --
   --  @param Segment Current segment [0..3]
   --  @param Index Block index within segment [0..segment_size)
   --  @return Absolute block index [0..Active_Blocks_Per_Lane)
   function Calculate_Current_Index (
      Segment : Segment_Index;
      Index   : Natural
   ) return Block_Index
   with
      Global => null,
      Pre    => Index < Active_Blocks_Per_Segment,
      Post   => Calculate_Current_Index'Result in Block_Index;

   function Calculate_Current_Index (
      Segment : Segment_Index;
      Index   : Natural
   ) return Block_Index
   is
   begin
      return Segment * Active_Blocks_Per_Segment + Index;
   end Calculate_Current_Index;

   ------------------------------------------------------------
   --  Fill_Memory Implementation
   ------------------------------------------------------------

   procedure Fill_Memory (
      Memory : in out Memory_State
   )
   is
      --  Current position in algorithm
      Pos : Position;

      --  Address generator for Argon2i mode (data-independent indexing)
      --  Initialized with default values; will be properly initialized per-segment
      --  when in Data_Independent mode
      Address_State : Address_Generator_State := (
         Input_Block   => Zero_Block,
         Address_Block => Zero_Block,
         Counter       => 0
      );

      --  Reference calculation results
      Ref_Lane  : Lane_Index;
      Ref_Index : Block_Index;

      --  Block indices
      Prev_Index    : Block_Index;
      Current_Index : Block_Index;

      --  Blocks for mixing
      Prev_Block : Block;
      Ref_Block  : Block;
      Output_Block : Block;

      --  Loop bounds
      Start_Index : Natural;
      End_Index   : Natural;

   begin
      --  ================================================================
      --  RFC 9106 Section 3.1.2: Main Memory-Filling Loop
      --  ================================================================
      --
      --  For each pass r ∈ [0, t):
      --    For each segment s ∈ [0, 3]:
      --      For each lane l ∈ [0, p):  [SparkPass: l=0 only]
      --        For each block index i in segment:
      --          Process block at [l, s × segment_size + i]

      --  Initialize position
      Pos := Initial_Position;

      --  ================================================================
      --  Pass Loop: Iterate over all passes [0..t)
      --  ================================================================
      --  SparkPass: t = 4 (four passes over memory)

      for Pass in Pass_Index loop
         Pos.Pass := Pass;

         --  ============================================================
         --  Segment Loop: Iterate over 4 segments per pass
         --  ============================================================

         for Segment in Segment_Index loop
            Pos.Segment := Segment;

            --  Initialize address generator for this segment (Argon2i mode)
            --  Only needed for Pass 0, Segments 0-1 (data-independent mode)
            if Get_Indexing_Mode (Pos) = Data_Independent then
               Initialize_Address_Generator (Address_State, Pos);
            end if;

            --  Determine block range for this segment
            --  Special case: First segment of first pass starts at block 2
            --  (blocks 0-1 already filled by Initialize_Memory)
            if Pass = 0 and Segment = 0 then
               Start_Index := 2;
            else
               Start_Index := 0;
            end if;

            End_Index := Active_Blocks_Per_Segment;

            --  =========================================================
            --  Block Loop: Process each block in segment
            --  =========================================================

            for Index in Start_Index .. End_Index - 1 loop
               pragma Loop_Invariant (Pass in Pass_Index);
               pragma Loop_Invariant (Segment in Segment_Index);
               pragma Loop_Invariant (Pos.Pass = Pass);
               pragma Loop_Invariant (Pos.Segment = Segment);
               pragma Loop_Invariant (Pos.Lane = 0);
               pragma Loop_Invariant (Index >= Start_Index);
               pragma Loop_Invariant (Index < End_Index);
               pragma Loop_Invariant (Index < Active_Blocks_Per_Segment);
               pragma Loop_Invariant (End_Index = Active_Blocks_Per_Segment);
               pragma Loop_Invariant (
                  if Pass = 0 and Segment = 0 then
                     Start_Index = 2 and Index >= 2
                  else
                     Start_Index = 0
               );
               pragma Loop_Invariant (Memory'First = 0);
               pragma Loop_Invariant (Memory'Last = Active_Blocks_Per_Lane - 1);

               --  Update position
               Pos.Index := Index;

               --  Step 1: Calculate reference block (ref_lane, ref_index)
               --  Uses Calculate_Reference from Phase 2.6
               Calculate_Reference (
                  Pos           => Pos,
                  Index         => Index,
                  Prev_Block    => Memory (Calculate_Prev_Index (Segment, Index)),
                  Address_State => Address_State,
                  Ref_Lane      => Ref_Lane,
                  Ref_Index     => Ref_Index
               );

               --  Note: Ref_Lane is always 0 for SparkPass (p=1, single lane)
               --  Ref_Lane is computed but not used (warning is acceptable)

               --  Step 2: Get previous block
               --  prev = Memory[l, (s × segment_size + i - 1) mod lane_length]
               Prev_Index := Calculate_Prev_Index (Segment, Index);
               Prev_Block := Memory (Prev_Index);

               --  Step 3: Get reference block
               --  ref = Memory[ref_lane, ref_index]
               --  For SparkPass: ref_lane always = 0 (single lane)
               Ref_Block := Memory (Ref_Index);

               --  Step 4: Compute new block
               --  Memory[l, s × segment_size + i] ← G(prev ⊕ ref, Memory[l, s × segment_size + i])
               --
               --  RFC 9106 Section 3.5: G(X, Y) = P(X ⊕ Y) ⊕ X ⊕ Y
               --  For memory filling:
               --    X = prev ⊕ ref
               --    Y = current block content
               --
               --  Calculate absolute block index
               Current_Index := Calculate_Current_Index (Segment, Index);

               --  Apply G mixing function
               --  RFC 9106 Section 3.1.2:
               --    Pass 0:  B[i][j] = G(prev ⊕ ref)
               --    Pass 1+: B[i][j] = G(prev ⊕ ref) ⊕ B[i][j]
               G (
                  X      => Prev_Block,
                  Y      => Ref_Block,
                  Output => Output_Block
               );

               --  For Pass 1+, XOR with existing block content
               if Pass > 0 then
                  for Word_Idx in Block_Word_Index loop
                     pragma Loop_Invariant (Current_Index in Block_Index);
                     pragma Loop_Invariant (Memory'First = 0);
                     pragma Loop_Invariant (Memory'Last = Active_Blocks_Per_Lane - 1);

                     Output_Block (Word_Idx) := Output_Block (Word_Idx) xor Memory (Current_Index) (Word_Idx);
                  end loop;
               end if;

               --  Write result back to memory
               Memory (Current_Index) := Output_Block;

            end loop;  -- Block loop

         end loop;  -- Segment loop

      end loop;  -- Pass loop

   end Fill_Memory;

end SparkPass.Crypto.Argon2id.Fill;
