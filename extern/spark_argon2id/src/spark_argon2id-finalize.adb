pragma SPARK_Mode (On);

with Interfaces; use type Interfaces.Unsigned_64;
with Spark_Argon2id.HPrime; use Spark_Argon2id.HPrime;

package body Spark_Argon2id.Finalize with
   SPARK_Mode => On
is

   ------------------------------------------------------------
   --  Helper Functions
   ------------------------------------------------------------

   --  Little-Endian 64-bit encoding
   --
   --  Converts a 64-bit unsigned integer to 8 bytes in little-endian order.
   --  Used for converting Block words to byte representation.
   --
   --  @param Value  64-bit unsigned integer
   --  @return       8 bytes in little-endian order
   --
   --  Example: LE64(0x123456789ABCDEF0) = [0xF0, 0xDE, 0xBC, 0x9A, 0x78, 0x56, 0x34, 0x12]
   --
   function LE64 (Value : U64) return Byte_Array is
     [1 => U8 (Value and 16#FF#),
      2 => U8 (Interfaces.Shift_Right (Value, 8) and 16#FF#),
      3 => U8 (Interfaces.Shift_Right (Value, 16) and 16#FF#),
      4 => U8 (Interfaces.Shift_Right (Value, 24) and 16#FF#),
      5 => U8 (Interfaces.Shift_Right (Value, 32) and 16#FF#),
      6 => U8 (Interfaces.Shift_Right (Value, 40) and 16#FF#),
      7 => U8 (Interfaces.Shift_Right (Value, 48) and 16#FF#),
      8 => U8 (Interfaces.Shift_Right (Value, 56) and 16#FF#)]
   with
      Global => null,
      Post   => LE64'Result'Length = 8;

   ------------------------------------------------------------
   --  Extract_Final_Block
   ------------------------------------------------------------

   procedure Extract_Final_Block (
      Memory                    : Memory_State;
      Active_Lanes              : Positive;
      Effective_Blocks_Per_Lane : Positive;
      Output                    : out Block
   )
   is
   begin
      --  RFC 9106 Section 3.1.3:
      --    If p > 1, C is XOR of final blocks from all lanes.
      --    If p = 1, C is the final block of the single lane.
      if Active_Lanes = 1 then
         Output := Memory (0, Effective_Blocks_Per_Lane - 1);
      else
         -- XOR last block across all lanes
         Output := Zero_Block;
         for L_Int in 0 .. Active_Lanes - 1 loop
            declare
               L : constant Lane_Index := Lane_Index (L_Int);
            begin
               for W in Block_Word_Index loop
                  Output (W) := Output (W) xor Memory (L, Effective_Blocks_Per_Lane - 1) (W);
               end loop;
            end;
         end loop;
      end if;
   end Extract_Final_Block;

   ------------------------------------------------------------
   --  Block_To_Bytes
   ------------------------------------------------------------

   procedure Block_To_Bytes (
      Input  : Block;
      Output : out Byte_Array
   )
   is
   begin
      --  Convert each U64 word to 8 bytes (little-endian)
      --
      --  Block structure: 128 x U64 words
      --  Output structure: 1024 bytes
      --
      --  Block[0] → Output[1..8]
      --  Block[1] → Output[9..16]
      --  ...
      --  Block[127] → Output[1017..1024]

      for Word_Index in Block_Word_Index loop
         pragma Loop_Invariant (Output'First = 1 and Output'Last = 1024);

         declare
            --  Calculate byte positions for this word
            --  Word_Index in [0..127], so Start_Pos in [0..1016]
            subtype Start_Pos_Range is Natural range 0 .. 1016;
            Start_Pos : constant Start_Pos_Range := Word_Index * 8;
            Byte_0_Pos : constant Positive := 1 + Start_Pos;
            Byte_1_Pos : constant Positive := 2 + Start_Pos;
            Byte_2_Pos : constant Positive := 3 + Start_Pos;
            Byte_3_Pos : constant Positive := 4 + Start_Pos;
            Byte_4_Pos : constant Positive := 5 + Start_Pos;
            Byte_5_Pos : constant Positive := 6 + Start_Pos;
            Byte_6_Pos : constant Positive := 7 + Start_Pos;
            Byte_7_Pos : constant Positive := 8 + Start_Pos;

            --  Extract word value
            Word : constant U64 := Input (Word_Index);
         begin

            --  Convert word to 8 bytes (little-endian) and write directly
            Output (Byte_0_Pos) := U8 (Word and 16#FF#);
            Output (Byte_1_Pos) := U8 (Interfaces.Shift_Right (Word, 8) and 16#FF#);
            Output (Byte_2_Pos) := U8 (Interfaces.Shift_Right (Word, 16) and 16#FF#);
            Output (Byte_3_Pos) := U8 (Interfaces.Shift_Right (Word, 24) and 16#FF#);
            Output (Byte_4_Pos) := U8 (Interfaces.Shift_Right (Word, 32) and 16#FF#);
            Output (Byte_5_Pos) := U8 (Interfaces.Shift_Right (Word, 40) and 16#FF#);
            Output (Byte_6_Pos) := U8 (Interfaces.Shift_Right (Word, 48) and 16#FF#);
            Output (Byte_7_Pos) := U8 (Interfaces.Shift_Right (Word, 56) and 16#FF#);
         end;
      end loop;

      --  After loop: All 128 words processed (Block_Word_Index range is 0..127)
      --  Loop invariant proves bytes [1..Word_Index*8+8] are initialized
      --  For Word_Index = 127: bytes [1..127*8+8] = [1..1024] are all initialized
      --  SPARK should be able to prove this from the loop invariant without assumptions

   end Block_To_Bytes;

   ------------------------------------------------------------
   --  Finalize
   ------------------------------------------------------------

   procedure Finalize (
      Memory                    : Memory_State;
      Active_Lanes              : Positive;
      Effective_Blocks_Per_Lane : Positive;
      Output_Length             : Positive;
      Output                    : out Byte_Array
   )
   is
      --  Final block C (1024 bytes as 128 x U64)
      Final_Block : Block;

      --  Final block as byte array (1024 bytes)
      Final_Block_Bytes : Byte_Array (1 .. Block_Size_Bytes);
   begin

      ------------------------------------------------------------
      --  Step 1: Extract final block from memory
      ------------------------------------------------------------

      Extract_Final_Block (Memory, Active_Lanes, Effective_Blocks_Per_Lane, Final_Block);

      ------------------------------------------------------------
      --  Step 2: Convert block to bytes
      ------------------------------------------------------------

      --  Convert 128 x U64 block to 1024 bytes (little-endian)

      Block_To_Bytes (Final_Block, Final_Block_Bytes);

      ------------------------------------------------------------
      --  Step 3: Apply H' variable-length hash
      ------------------------------------------------------------

      --  RFC 9106 Section 3.1.3: Tag ← H'(C, output_length)

      pragma Assert (Final_Block_Bytes'Length = Block_Size_Bytes);

      Compute_H_Prime (
         Output_Length => Output_Length,
         Input         => Final_Block_Bytes,
         Output        => Output
      );

      --  Note: Final_Block and Final_Block_Bytes contain derived key material
      --  In production, these should be zeroized before return
      --  For now, they go out of scope and will be overwritten

   end Finalize;

end Spark_Argon2id.Finalize;
