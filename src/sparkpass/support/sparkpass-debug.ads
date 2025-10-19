pragma SPARK_Mode (On);
with SparkPass.Types;

package SparkPass.Debug is
   --  pragma Preelaborate;  -- Cannot be preelaborated since we use Ada.Text_IO

   --  Compile-time debug flag - set via -gnateDEnable_Debug=True
   --  Default: disabled for production builds
   Enable_Debug : constant Boolean := False;

   --  Safe logging wrapper - only outputs if Enable_Debug is True
   --  Never use this to log secrets (keys, nonces, random values)
   procedure Log (S : in String) with Inline;

   --  Log hex bytes (for non-secret data only)
   procedure Log_Hex (Label : in String; Data : in SparkPass.Types.Byte_Array; Count : in Natural := 20) with Inline;

end SparkPass.Debug;
