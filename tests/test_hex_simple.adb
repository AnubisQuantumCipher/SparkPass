pragma SPARK_Mode (Off);
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling;
with SparkPass.Types; use SparkPass.Types;
with Test_Hex_Utils; use Test_Hex_Utils;

procedure Test_Hex_Simple is
   Test1 : constant String := "abcd1234";
   Bytes : constant Byte_Array := Hex_String_To_Bytes (Test1);
   Back  : constant String := Bytes_To_Hex_String (Bytes);
begin
   Put_Line ("Original: " & Test1);
   Put_Line ("Converted to bytes and back: " & Back);

   if Ada.Characters.Handling.To_Lower (Test1) /= Ada.Characters.Handling.To_Lower (Back) then
      Put_Line ("FAIL: Strings don't match");
   else
      Put_Line ("PASS: Hex conversion works");
   end if;
end Test_Hex_Simple;
