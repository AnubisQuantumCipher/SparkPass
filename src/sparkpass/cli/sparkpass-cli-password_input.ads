pragma SPARK_Mode (Off);
with SparkPass.Types; use SparkPass.Types;

package SparkPass.CLI.Password_Input is
   --  Secure password input with echo disabled
   --
   --  Reads password from stdin without displaying characters on screen.
   --  Uses termios on Unix/macOS to disable terminal echo.

   procedure Read_Password
     (Prompt   : in String;
      Password : out Byte_Array;
      Length   : out Natural;
      Success  : out Boolean)
     with Pre => Password'Length >= 1;
   --  Prompts user for password and reads from stdin with echo disabled.
   --  Password will be null-terminated at Length.
   --  Success is False if reading fails or buffer is too small.

end SparkPass.CLI.Password_Input;
