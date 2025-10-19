with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Blake2b;

procedure Test_Blake2b_Simple is

   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Put_Hex;

   Output : SparkPass.Crypto.Blake2b.Hash_Type;

begin
   --  Test "abc"
   declare
      Input : constant Byte_Array := (16#61#, 16#62#, 16#63#);
   begin
      SparkPass.Crypto.Blake2b.Hash (Input, Output);
      Put_Line ("SparkPass Blake2b-512(""abc""):");
      for B of Output loop
         Put_Hex (B);
      end loop;
      New_Line;
   end;

   --  Test empty string
   declare
      Input : constant Byte_Array (1 .. 0) := (others => 0);
   begin
      SparkPass.Crypto.Blake2b.Hash (Input, Output);
      Put_Line ("SparkPass Blake2b-512(""""):");
      for B of Output loop
         Put_Hex (B);
      end loop;
      New_Line;
   end;

end Test_Blake2b_Simple;
