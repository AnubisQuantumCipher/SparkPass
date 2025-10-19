pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Blake2b;

procedure Test_Quick is
   use SparkPass.Crypto.Blake2b;

   Message128 : constant Byte_Array (1 .. 128) := (others => 0);
   Result : Hash_Type;
begin
   Put_Line ("Testing 128-byte zeros...");
   Hash (Message128, Result);

   Put ("Result: ");
   for I in 1 .. 8 loop
      Put (U8'Image (Result (I)) & " ");
   end loop;
   New_Line;

   Put_Line ("Expected: 134 89 57 225 32 230 128 84");
   Put_Line ("(hex: 86 59 39 e1 20 e6 80 54)");
end Test_Quick;
