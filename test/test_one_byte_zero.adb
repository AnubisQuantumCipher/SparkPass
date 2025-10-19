pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Blake2b;

procedure Test_One_Byte_Zero is
   use SparkPass.Crypto.Blake2b;

   Message : constant Byte_Array (1 .. 1) := (1 => 0);
   Result  : Hash_Type;

   function Byte_To_Hex (B : U8) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Hi : constant Natural := Natural (Shift_Right (Unsigned_8 (B), 4));
      Lo : constant Natural := Natural (Unsigned_8 (B) and 16#0F#);
   begin
      return Hex_Chars (Hi + 1) & Hex_Chars (Lo + 1);
   end Byte_To_Hex;

begin
   Put_Line ("Hashing one-byte zero message...");

   begin
      Hash (Message, Result);
      Put_Line ("Hash completed successfully");
   exception
      when E : others =>
         Put_Line ("ERROR during hashing!");
         raise;
   end;

   Put ("Result:   ");
   for I in Result'Range loop
      Put (Byte_To_Hex (Result (I)));
   end loop;
   New_Line;

   Put_Line ("Expected: 2fa3f686df876995167e7c2e5d74c4c7b6e48f8068fe0e44208344d480f7904c36963e44115fe3eb2a3ac8694c28bcb4f5a0f3276f2e79487d8219057a506e4b");
end Test_One_Byte_Zero;
