pragma SPARK_Mode (Off);
with Ada.Text_IO;
with Ada.Environment_Variables;
with Interfaces.C;
with System;
with Bindings.Termios;
with Bindings.POSIX;

package body SparkPass.CLI.Password_Input is

   --  Check if stdin is a TTY (interactive terminal)
   function Is_TTY return Boolean is
      use type Interfaces.C.int;
      STDIN_FILENO : constant := 0;
      Result : Interfaces.C.int;
   begin
      Result := Bindings.POSIX.isatty (STDIN_FILENO);
      return Result /= 0;
   end Is_TTY;

   --  Try to read password from environment variable (SPARKPASS_PASSWORD)
   procedure Try_Read_From_Env
     (Password : out Byte_Array;
      Length   : out Natural;
      Found    : out Boolean)
   is
      use Ada.Environment_Variables;
   begin
      Found := False;
      Length := 0;

      if not Exists ("SPARKPASS_PASSWORD") then
         return;
      end if;

      declare
         Env_Pass : constant String := Value ("SPARKPASS_PASSWORD");
      begin
         if Env_Pass'Length = 0 or else Env_Pass'Length > Password'Length then
            return;
         end if;

         --  Copy from environment variable
         for I in 0 .. Env_Pass'Length - 1 loop
            Password (Password'First + I) := U8 (Character'Pos (Env_Pass (Env_Pass'First + I)));
         end loop;

         Length := Env_Pass'Length;
         Found := True;
      end;
   end Try_Read_From_Env;

   --  Read password from stdin (pipe/redirect, non-interactive)
   procedure Read_From_Stdin
     (Password : out Byte_Array;
      Length   : out Natural;
      Success  : out Boolean)
   is
      use Ada.Text_IO;
      Line : String (1 .. Password'Length);
      Last : Natural;
   begin
      Success := False;
      Length := 0;

      --  Read line from stdin
      begin
         Get_Line (Line, Last);

         if Last > 0 then
            for I in 1 .. Last loop
               Password (Password'First + I - 1) := U8 (Character'Pos (Line (I)));
            end loop;
            Length := Last;
            Success := True;
         end if;
      exception
         when others =>
            Success := False;
            Length := 0;
      end;
   end Read_From_Stdin;

   --  Read password from TTY with echo disabled (interactive)
   procedure Read_From_TTY
     (Prompt   : in String;
      Password : out Byte_Array;
      Length   : out Natural;
      Success  : out Boolean)
   is
      use Ada.Text_IO;
      use Interfaces.C;

      --  Terminal control structures (opaque buffer, platform-independent)
      Old_Term : aliased Bindings.Termios.Termios_Buffer;
      New_Term : aliased Bindings.Termios.Termios_Buffer;
      Result   : Interfaces.C.int;

      STDIN_FILENO : constant := 0;
      Index : Natural := 0;
   begin
      Success := False;
      Length := 0;

      --  Print prompt (without newline)
      Put (Prompt);
      Flush;

      --  Get current terminal settings
      Result := Bindings.Termios.tcgetattr
        (STDIN_FILENO,
         Old_Term'Address);

      if Result /= 0 then
         --  Failed to get terminal settings
         New_Line;
         Success := False;
         Length := 0;
         return;
      end if;

      --  Copy old settings to new
      New_Term := Old_Term;

      --  Disable echo using platform-independent helper (works on Linux/macOS/BSD)
      Bindings.Termios.Disable_Echo (New_Term'Address);

      --  Set new terminal settings
      Result := Bindings.Termios.tcsetattr
        (STDIN_FILENO,
         Bindings.Termios.TCSANOW,
         New_Term'Address);

      if Result /= 0 then
         --  Failed to set terminal settings
         New_Line;
         return;
      end if;

      --  Read password line
      declare
         Line : String (1 .. Password'Length);
         Last : Natural;
      begin
         Get_Line (Line, Last);

         --  Copy line to password buffer
         if Last > 0 then
            for I in 1 .. Last loop
               Password (Password'First + I - 1) := U8 (Character'Pos (Line (I)));
            end loop;
            Index := Last;
         end if;
      end;

      --  Restore old terminal settings
      Result := Bindings.Termios.tcsetattr
        (STDIN_FILENO,
         Bindings.Termios.TCSANOW,
         Old_Term'Address);

      --  Print newline (since echo was disabled, user's Enter wasn't visible)
      New_Line;

      Length := Index;
      Success := True;

   exception
      when others =>
         --  Restore terminal settings on exception
         Result := Bindings.Termios.tcsetattr
           (STDIN_FILENO,
            Bindings.Termios.TCSANOW,
            Old_Term'Address);
         Success := False;
         Length := 0;
   end Read_From_TTY;

   procedure Read_Password
     (Prompt   : in String;
      Password : out Byte_Array;
      Length   : out Natural;
      Success  : out Boolean)
   is
      Env_Found : Boolean := False;
   begin
      Success := False;
      Length := 0;

      --  Zero password buffer
      for I in Password'Range loop
         Password (I) := 0;
      end loop;

      --  Method 1: Try environment variable (SPARKPASS_PASSWORD)
      --  Security note: Environment variables can be seen in process listings.
      --  Only use for testing/automation in secure environments.
      Try_Read_From_Env (Password, Length, Env_Found);
      if Env_Found then
         Success := True;
         return;
      end if;

      --  Method 2: Check if stdin is a TTY
      if Is_TTY then
         --  Interactive mode: Read with echo disabled
         Read_From_TTY (Prompt, Password, Length, Success);
      else
         --  Non-interactive mode (pipe/redirect): Read from stdin directly
         --  This allows: echo "password" | sparkpass unlock vault.spass
         Read_From_Stdin (Password, Length, Success);
      end if;

   end Read_Password;

end SparkPass.CLI.Password_Input;
