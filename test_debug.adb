pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

procedure Test_Debug is
   subtype U8 is Interfaces.Unsigned_8;
   type Byte_Array is array (Positive range <>) of U8;

   Message128 : constant Byte_Array (1 .. 128) := (others => 0);
begin
   Put_Line ("Message'First = " & Integer'Image (Message128'First));
   Put_Line ("Message'Last = " & Integer'Image (Message128'Last));
   Put_Line ("Message'Length = " & Integer'Image (Message128'Length));

   declare
      Offset : Natural := Message128'First;
   begin
      Put_Line ("Initial Offset = " & Natural'Image (Offset));
      Put_Line ("Loop condition: Offset <= Message'Last = " & Boolean'Image (Offset <= Message128'Last));
      Put_Line ("Loop condition: Message'Last - Offset > 127 = " & Boolean'Image (Message128'Last - Offset > 127));
      Put_Line ("  (Message'Last - Offset = " & Integer'Image (Message128'Last - Offset) & ")");

      if Offset <= Message128'Last and then Message128'Last - Offset > 127 then
         Put_Line ("Loop WOULD execute");
      else
         Put_Line ("Loop would NOT execute");
      end if;

      --  Simulate what happens after loop
      declare
         Remaining : constant Natural := Message128'Last - Offset + 1;
      begin
         Put_Line ("Remaining = " & Natural'Image (Remaining));
      end;
   end;
end Test_Debug;
