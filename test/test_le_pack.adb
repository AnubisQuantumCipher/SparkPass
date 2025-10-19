pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;

--  Test LE_Pack function to verify little-endian conversion

procedure Test_LE_Pack is

   --  Helper: Print byte array as hex
   procedure Print_Hex (Name : String; Data : Byte_Array) is
   begin
      Put (Name & ": ");
      for I in Data'Range loop
         declare
            Hex_Chars : constant String := "0123456789abcdef";
            B : constant U8 := Data (I);
            Hi : constant U8 := Shift_Right(B, 4);
            Lo : constant U8 := B and 16#0F#;
         begin
            Put (Hex_Chars(Natural(Hi) + 1));
            Put (Hex_Chars(Natural(Lo) + 1));
         end;
      end loop;
      New_Line;
   end Print_Hex;

   --  Test LE_Pack implementation
   function LE_Pack (Bytes : Byte_Array) return U64 is
      Result : U64 := 0;
   begin
      --  Build from MSB down to LSB (byte 7 is most significant)
      Result := U64 (Bytes (Bytes'First + 7));
      Result := Shift_Left (Result, 8) or U64 (Bytes (Bytes'First + 6));
      Result := Shift_Left (Result, 8) or U64 (Bytes (Bytes'First + 5));
      Result := Shift_Left (Result, 8) or U64 (Bytes (Bytes'First + 4));
      Result := Shift_Left (Result, 8) or U64 (Bytes (Bytes'First + 3));
      Result := Shift_Left (Result, 8) or U64 (Bytes (Bytes'First + 2));
      Result := Shift_Left (Result, 8) or U64 (Bytes (Bytes'First + 1));
      Result := Shift_Left (Result, 8) or U64 (Bytes (Bytes'First));
      return Result;
   end LE_Pack;

   --  Test data: "abc" + padding (first 8 bytes of 128-byte block)
   Test1 : constant Byte_Array (1 .. 8) := (16#61#, 16#62#, 16#63#, 0, 0, 0, 0, 0);

   --  Test data: All zeros
   Test2 : constant Byte_Array (1 .. 8) := (others => 0);

   --  Test data: Sequential bytes
   Test3 : constant Byte_Array (1 .. 8) := (1, 2, 3, 4, 5, 6, 7, 8);

   Result : U64;

begin
   Put_Line ("LE_Pack Test");
   Put_Line ("============");
   New_Line;

   --  Test 1: "abc" + padding
   Print_Hex ("Test1 bytes", Test1);
   Result := LE_Pack (Test1);
   Put_Line ("LE_Pack result: " & U64'Image(Result));
   Put_Line ("Expected: 0x0000000000636261 (little-endian: 61 62 63 00 00 00 00 00)");
   if Result = 16#0000000000636261# then
      Put_Line ("PASS");
   else
      Put_Line ("FAIL");
   end if;
   New_Line;

   --  Test 2: All zeros
   Print_Hex ("Test2 bytes", Test2);
   Result := LE_Pack (Test2);
   Put_Line ("LE_Pack result: " & U64'Image(Result));
   Put_Line ("Expected: 0x0000000000000000");
   if Result = 0 then
      Put_Line ("PASS");
   else
      Put_Line ("FAIL");
   end if;
   New_Line;

   --  Test 3: Sequential bytes
   Print_Hex ("Test3 bytes", Test3);
   Result := LE_Pack (Test3);
   Put_Line ("LE_Pack result: " & U64'Image(Result));
   Put_Line ("Expected: 0x0807060504030201 (little-endian: 01 02 03 04 05 06 07 08)");
   if Result = 16#0807060504030201# then
      Put_Line ("PASS");
   else
      Put_Line ("FAIL");
   end if;
   New_Line;

   Put_Line ("Test complete!");
end Test_LE_Pack;
