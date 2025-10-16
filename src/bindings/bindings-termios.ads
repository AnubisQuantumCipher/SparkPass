pragma SPARK_Mode (Off);
with Interfaces.C;
with System;

package Bindings.Termios is
   pragma Preelaborate;

   --  Terminal I/O control for secure password input (Unix/POSIX)
   --  Used to disable echo when reading passwords
   --
   --  PORTABILITY NOTE: This uses an opaque buffer approach to avoid
   --  platform-specific struct layouts. Works on Linux, macOS, *BSD.

   --  Opaque termios buffer (sized to accommodate all platforms)
   --  Linux: ~60 bytes, macOS: ~72 bytes, BSD: ~44 bytes
   --  We use 256 bytes to be safe across all Unix variants
   subtype Termios_Buffer is Interfaces.C.char_array (1 .. 256);

   --  tcgetattr/tcsetattr options
   TCSANOW : constant := 0;

   --  c_lflag bits (standard POSIX values)
   ECHO    : constant := 8;
   ICANON  : constant := 2;

   --  Get current terminal attributes
   function tcgetattr
     (fd        : Interfaces.C.int;
      termios_p : System.Address) return Interfaces.C.int
     with Import, Convention => C, External_Name => "tcgetattr";

   --  Set terminal attributes
   function tcsetattr
     (fd               : Interfaces.C.int;
      optional_actions : Interfaces.C.int;
      termios_p        : System.Address) return Interfaces.C.int
     with Import, Convention => C, External_Name => "tcsetattr";

   --  Platform-independent helper: disable echo in termios buffer
   --  Manipulates c_lflag field portably across Linux/macOS/BSD
   procedure Disable_Echo
     (termios_p : System.Address)
     with Import, Convention => C, External_Name => "sparkpass_termios_disable_echo";

   --  Platform-independent helper: enable echo in termios buffer
   procedure Enable_Echo
     (termios_p : System.Address)
     with Import, Convention => C, External_Name => "sparkpass_termios_enable_echo";

end Bindings.Termios;
