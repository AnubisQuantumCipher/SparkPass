--  Test initial block generation
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id.Types; use SparkPass.Crypto.Argon2id.Types;
with SparkPass.Crypto.Argon2id.H0;
with SparkPass.Crypto.Argon2id.Init;

procedure Test_Init_Blocks is

   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Put_Hex;

   procedure Put_Block_First_64_Bytes (B : Block) is
      Bytes : Byte_Array (1 .. 64);
      Idx : Positive := 1;
   begin
      --  Convert first 8 words (64 bytes) to bytes
      for Word_Idx in 0 .. 7 loop
         declare
            W : constant U64 := B (Word_Idx);
         begin
            for Byte_Idx in 0 .. 7 loop
               Bytes (Idx) := U8 (Shift_Right (W, Byte_Idx * 8) and 16#FF#);
               Idx := Idx + 1;
            end loop;
         end;
      end loop;

      --  Print first 64 bytes
      for Byte of Bytes loop
         Put_Hex (Byte);
      end loop;
   end Put_Block_First_64_Bytes;

   Password : constant Byte_Array := (
      Unsigned_8(Character'Pos('p')),
      Unsigned_8(Character'Pos('a')),
      Unsigned_8(Character'Pos('s')),
      Unsigned_8(Character'Pos('s')),
      Unsigned_8(Character'Pos('w')),
      Unsigned_8(Character'Pos('o')),
      Unsigned_8(Character'Pos('r')),
      Unsigned_8(Character'Pos('d'))
   );

   Salt : constant Byte_Array := (
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  -- "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#,  -- "SOMESALT"
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  -- "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#   -- "SOMESALT"
   );

   H0_Output : Byte_Array (1 .. 64);
   Init_Blocks : SparkPass.Crypto.Argon2id.Init.Initial_Blocks;

begin
   Put_Line ("Testing Argon2id Initial Block Generation");
   Put_Line ("Parameters: m=16384, t=4, p=1, password='password', salt=32 bytes");
   New_Line;

   --  Compute H0
   SparkPass.Crypto.Argon2id.H0.Compute_H0 (
      Password    => Password,
      Salt        => Salt,
      Parallelism => 1,
      Tag_Length  => 32,
      Memory_KiB  => 16384,
      Iterations  => 4,
      H0_Out      => H0_Output
   );

   Put_Line ("H0 computed (64 bytes)");

   --  Generate initial blocks
   SparkPass.Crypto.Argon2id.Init.Generate_Initial_Blocks (
      H0     => H0_Output,
      Lane   => 0,
      Output => Init_Blocks
   );

   Put ("Block[0][0:63]: ");
   Put_Block_First_64_Bytes (Init_Blocks.Block_0);
   New_Line;

   Put ("Block[1][0:63]: ");
   Put_Block_First_64_Bytes (Init_Blocks.Block_1);
   New_Line;

end Test_Init_Blocks;
