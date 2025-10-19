with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id.HPrime;

procedure Test_HPrime_Input is

   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Put_Hex;

   --  Build the exact input we saw in the debug output
   Input : Byte_Array (1 .. 1024);
   Output : Byte_Array (1 .. 32);

begin
   --  Fill with the final block bytes from Test 1
   Input := (
      16#4b#, 16#5e#, 16#7b#, 16#d2#, 16#f3#, 16#2f#, 16#d2#, 16#f3#,
      16#e0#, 16#d0#, 16#af#, 16#dc#, 16#08#, 16#66#, 16#27#, 16#6c#,
      16#41#, 16#32#, 16#8b#, 16#b7#, 16#ff#, 16#ec#, 16#80#, 16#0a#,
      16#f9#, 16#5a#, 16#c7#, 16#3e#, 16#b3#, 16#11#, 16#6a#, 16#b4#,
      16#06#, 16#7b#, 16#2e#, 16#47#, 16#03#, 16#f0#, 16#b8#, 16#67#,
      16#70#, 16#74#, 16#75#, 16#ef#, 16#ff#, 16#d4#, 16#2b#, 16#d3#,
      16#e9#, 16#34#, 16#b2#, 16#86#, 16#b3#, 16#76#, 16#0d#, 16#06#,
      16#59#, 16#62#, 16#81#, 16#80#, 16#cf#, 16#f6#, 16#c8#, 16#a6#,
      others => 0  --  Rest filled with zeros
   );

   Put_Line ("Testing H'(32, Final_Block)...");
   Put ("Input[1..64]: ");
   for I in 1 .. 64 loop
      Put_Hex (Input (I));
   end loop;
   New_Line;

   HPrime.Compute_H_Prime (
      Output_Length => 32,
      Input         => Input,
      Output        => Output
   );

   Put ("H' Output: ");
   for B of Output loop
      Put_Hex (B);
   end loop;
   New_Line;

end Test_HPrime_Input;
