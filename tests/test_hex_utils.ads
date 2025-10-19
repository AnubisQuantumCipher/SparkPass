pragma SPARK_Mode (Off);  -- File I/O not supported in SPARK
with SparkPass.Types; use SparkPass.Types;

package Test_Hex_Utils is
   --  Utility package for parsing hex strings in test vectors

   --  Convert a single hex character to its numeric value (0-15)
   function Hex_Char_To_Byte (C : Character) return U8
     with Pre => (C in '0' .. '9') or (C in 'a' .. 'f') or (C in 'A' .. 'F');

   --  Convert a hex string to a Byte_Array
   --  Input: "ABCD1234" -> Output: [171, 205, 18, 52]
   function Hex_String_To_Bytes (Hex : String) return Byte_Array
     with Pre => Hex'Length mod 2 = 0 and Hex'Length > 0;

   --  Convert Byte_Array to hex string for debugging
   function Bytes_To_Hex_String (Bytes : Byte_Array) return String;

   --  Parse a single unsigned 32-bit value from string
   function Parse_U32 (S : String) return U32;

end Test_Hex_Utils;
