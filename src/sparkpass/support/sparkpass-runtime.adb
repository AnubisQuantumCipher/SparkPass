pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Ada.Directories;
with Ada.Environment_Variables;
with SparkPass.Config;

package body SparkPass.Runtime is

   Mode : Boolean := SparkPass.Config.High_Assurance_Mode;

   function Config_Path return String is
      Home : constant String := Ada.Environment_Variables.Value ("HOME");
      Dir  : constant String := (if Home'Length > 0 then Home & "/.sparkpass" else ".");
   begin
      return Dir & "/policy.cfg";
   end Config_Path;

   procedure Ensure_Config_Dir is
      Dir : constant String := Ada.Directories.Containing_Directory (Config_Path);
   begin
      if not Ada.Directories.Exists (Dir) then
         Ada.Directories.Create_Path (Dir);
      end if;
   exception
      when others => null;
   end Ensure_Config_Dir;

   procedure Load_Mode is
      F : Ada.Text_IO.File_Type;
   begin
      declare
         Path : constant String := Config_Path;
      begin
         if Ada.Directories.Exists (Path) then
            Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Path);
            declare
               Line : String := Ada.Text_IO.Get_Line (F);
            begin
               if Line'Length >= 3 and then Line (1 .. 3) = "HA=" then
                  if Line'Last >= 4 and then (Line (4) = '1' or else Line (4) = 'T' or else Line (4) = 't') then
                     Mode := True;
                  else
                     Mode := False;
                  end if;
               end if;
            exception
               when others => null;
            end;
            Ada.Text_IO.Close (F);
         end if;
      end;
   exception
      when others => null;
   end Load_Mode;

   procedure Save_Mode is
      F : Ada.Text_IO.File_Type;
   begin
      Ensure_Config_Dir;
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Config_Path);
      if Mode then
         Ada.Text_IO.Put_Line (F, "HA=1");
      else
         Ada.Text_IO.Put_Line (F, "HA=0");
      end if;
      Ada.Text_IO.Close (F);
   exception
      when others => null;
   end Save_Mode;

   function High_Assurance_Enabled return Boolean is
   begin
      -- Lazy load from persisted config
      Load_Mode;
      return Mode;
   end High_Assurance_Enabled;

   procedure Set_High_Assurance (Enabled : Boolean) is
   begin
      Mode := Enabled;
      Save_Mode;
   end Set_High_Assurance;

begin
   -- Initialize from persisted value
   Load_Mode;
end SparkPass.Runtime;

