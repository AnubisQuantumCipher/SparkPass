pragma SPARK_Mode (On);
with Ada.Text_IO; use Ada.Text_IO;

package body SparkPass.Debug is

   procedure Log (S : in String) is
   begin
      if Enable_Debug then
         Put_Line (S);
      end if;
   end Log;

   procedure Log_Hex (Label : in String; Data : in SparkPass.Types.Byte_Array; Count : in Natural := 20) is
      Hex : constant String := "0123456789ABCDEF";
      Actual_Count : constant Natural := Natural'Min(Count, Data'Length);
   begin
      if not Enable_Debug then
         return;
      end if;

      Put (Label);
      for I in 1 .. Actual_Count loop
         declare
            Byte_Val : constant Natural := Natural(Data(Data'First + I - 1));
            Hi : constant Natural := Byte_Val / 16;
            Lo : constant Natural := Byte_Val mod 16;
         begin
            Put (Hex(Hi + 1) & Hex(Lo + 1) & " ");
         end;
      end loop;
      New_Line;
   end Log_Hex;

end SparkPass.Debug;
