with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

--  Test LE_Pack function to verify little-endian conversion

procedure Test_LE_Standalone is
   type Byte_Array is array (Positive range <>) of Unsigned_8;

   --  Helper: Print byte array as hex
   procedure Print_Hex (Name : String; Data : Byte_Array) is
   begin
      Put (Name & ": ");
      for I in Data'Range loop
         declare
            Hex_Chars : constant String := "0123456789abcdef";
            B : constant Unsigned_8 := Data (I);
            Hi : constant Unsigned_8 := Shift_Right(B, 4);
            Lo : constant Unsigned_8 := B and 16#0F#;
         begin
            Put (Hex_Chars(Natural(Hi) + 1));
            Put (Hex_Chars(Natural(Lo) + 1));
         end;
      end loop;
      New_Line;
   end Print_Hex;

   --  Helper: Print U64 as hex
   procedure Print_U64_Hex (Name : String; Value : Unsigned_64) is
      Hex_Chars : constant String := "0123456789abcdef";
      Temp : Unsigned_64 := Value;
   begin
      Put (Name & ": 0x");
      for I in reverse 0 .. 15 loop
         declare
            Nibble : constant Unsigned_64 := Shift_Right(Value, I * 4) and 16#0F#;
         begin
            Put (Hex_Chars(Natural(Nibble) + 1));
         end;
      end loop;
      New_Line;
   end Print_U64_Hex;

   --  Test LE_Pack implementation (from Blake2b)
   function LE_Pack (Bytes : Byte_Array) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      --  Build from MSB down to LSB (byte 7 is most significant)
      Result := Unsigned_64 (Bytes (Bytes'First + 7));
      Result := Shift_Left (Result, 8) or Unsigned_64 (Bytes (Bytes'First + 6));
      Result := Shift_Left (Result, 8) or Unsigned_64 (Bytes (Bytes'First + 5));
      Result := Shift_Left (Result, 8) or Unsigned_64 (Bytes (Bytes'First + 4));
      Result := Shift_Left (Result, 8) or Unsigned_64 (Bytes (Bytes'First + 3));
      Result := Shift_Left (Result, 8) or Unsigned_64 (Bytes (Bytes'First + 2));
      Result := Shift_Left (Result, 8) or Unsigned_64 (Bytes (Bytes'First + 1));
      Result := Shift_Left (Result, 8) or Unsigned_64 (Bytes (Bytes'First));
      return Result;
   end LE_Pack;

   --  Test data: "abc" + padding (first 8 bytes of 128-byte block)
   Test1 : constant Byte_Array (1 .. 8) := (16#61#, 16#62#, 16#63#, 0, 0, 0, 0, 0);

   --  Test data: All zeros
   Test2 : constant Byte_Array (1 .. 8) := (others => 0);

   --  Test data: Sequential bytes
   Test3 : constant Byte_Array (1 .. 8) := (1, 2, 3, 4, 5, 6, 7, 8);

   Result : Unsigned_64;

begin
   Put_Line ("LE_Pack Test");
   Put_Line ("============");
   New_Line;

   --  Test 1: "abc" + padding
   Put_Line ("Test 1: 'abc' + padding");
   Print_Hex ("Input bytes", Test1);
   Result := LE_Pack (Test1);
   Print_U64_Hex ("LE_Pack result", Result);
   Print_U64_Hex ("Expected      ", 16#0000000000636261#);
   Put_Line ("(little-endian: LSB=61, MSB=00)");
   if Result = 16#0000000000636261# then
      Put_Line ("PASS");
   else
      Put_Line ("FAIL");
   end if;
   New_Line;

   --  Test 2: All zeros
   Put_Line ("Test 2: All zeros");
   Print_Hex ("Input bytes", Test2);
   Result := LE_Pack (Test2);
   Print_U64_Hex ("LE_Pack result", Result);
   Print_U64_Hex ("Expected      ", 0);
   if Result = 0 then
      Put_Line ("PASS");
   else
      Put_Line ("FAIL");
   end if;
   New_Line;

   --  Test 3: Sequential bytes
   Put_Line ("Test 3: Sequential bytes 01 02 03 04 05 06 07 08");
   Print_Hex ("Input bytes", Test3);
   Result := LE_Pack (Test3);
   Print_U64_Hex ("LE_Pack result", Result);
   Print_U64_Hex ("Expected      ", 16#0807060504030201#);
   Put_Line ("(little-endian: LSB=01, MSB=08)");
   if Result = 16#0807060504030201# then
      Put_Line ("PASS");
   else
      Put_Line ("FAIL");
   end if;
   New_Line;

   Put_Line ("Test complete!");
end Test_LE_Standalone;
