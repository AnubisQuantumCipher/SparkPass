pragma SPARK_Mode (On);
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.Zeroize is
   function Is_Zeroed (Buffer : Byte_Array) return Boolean
     with
       Global  => null,
       Depends => (Is_Zeroed'Result => Buffer);

   procedure Wipe (Buffer : in out Byte_Array)
     with
       Global  => null,
       Depends => (Buffer => Buffer),
       Post    => Is_Zeroed (Buffer);

   procedure Wipe_Key (Buffer : in out Key_Array)
     with
       Global  => null,
       Depends => (Buffer => Buffer),
       Post    => Is_Zeroed (Byte_Array (Buffer));

   procedure Wipe_Tag (Buffer : in out Tag_Array)
     with
       Global  => null,
       Depends => (Buffer => Buffer),
       Post    => Is_Zeroed (Byte_Array (Buffer));

   procedure Wipe_Chain (Buffer : in out Chain_Key_Array)
     with
       Global  => null,
       Depends => (Buffer => Buffer),
       Post    => Is_Zeroed (Byte_Array (Buffer));

   --  Constant-time comparison of byte arrays
   --  Returns True if arrays are equal, False otherwise
   --  Uses sodium_memcmp to prevent timing attacks
   function Equal (Left : Byte_Array; Right : Byte_Array) return Boolean
     with
       Global  => null,
       Depends => (Equal'Result => (Left, Right)),
       Pre     => Left'Length = Right'Length and then
                  Left'Length > 0 and then
                  Left'Length <= 65536;
end SparkPass.Crypto.Zeroize;
