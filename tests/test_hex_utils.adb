pragma SPARK_Mode (Off);
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;

package body Test_Hex_Utils is

   function Hex_Char_To_Byte (C : Character) return U8 is
   begin
      case C is
         when '0' .. '9' =>
            return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' =>
            return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' =>
            return Character'Pos (C) - Character'Pos ('A') + 10;
         when others =>
            raise Constraint_Error with "Invalid hex character: " & C;
      end case;
   end Hex_Char_To_Byte;

   function Hex_String_To_Bytes (Hex : String) return Byte_Array is
      Result : Byte_Array (1 .. Hex'Length / 2);
      Hex_Index : Integer := Hex'First;
      Result_Index : Positive := 1;
      High, Low : U8;
   begin
      while Hex_Index <= Hex'Last - 1 loop
         High := Hex_Char_To_Byte (Hex (Hex_Index));
         Low  := Hex_Char_To_Byte (Hex (Hex_Index + 1));
         Result (Result_Index) := Shift_Left (High, 4) or Low;
         Hex_Index := Hex_Index + 2;
         Result_Index := Result_Index + 1;
      end loop;
      return Result;
   end Hex_String_To_Bytes;

   function Bytes_To_Hex_String (Bytes : Byte_Array) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Result : String (1 .. Bytes'Length * 2);
      Index : Positive := 1;
   begin
      for B of Bytes loop
         Result (Index) := Hex_Chars (Integer (Shift_Right (B, 4)) + 1);
         Result (Index + 1) := Hex_Chars (Integer (B and 16#0F#) + 1);
         Index := Index + 2;
      end loop;
      return Result;
   end Bytes_To_Hex_String;

   function Parse_U32 (S : String) return U32 is
      Result : U32 := 0;
      Trimmed : constant String := Ada.Characters.Handling.To_Lower (
        Ada.Strings.Fixed.Trim (S, Ada.Strings.Both));
   begin
      for C of Trimmed loop
         if C not in '0' .. '9' then
            raise Constraint_Error with "Invalid decimal number: " & S;
         end if;
         Result := Result * 10 + U32 (Character'Pos (C) - Character'Pos ('0'));
      end loop;
      return Result;
   end Parse_U32;

end Test_Hex_Utils;
