pragma SPARK_Mode (Off);  -- Uses Ada.Streams.Stream_IO (not in SPARK subset)
with Ada.Streams.Stream_IO;

package body SparkPass.Crypto.Random is

   procedure Fill (Buffer : in out Byte_Array) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      F : File_Type;
      Last : Stream_Element_Offset;

      --  Stream_Element_Array for reading from /dev/urandom
      --  We use 1-based indexing to match Buffer
      SEA : Stream_Element_Array (1 .. Stream_Element_Offset (Buffer'Length));
   begin
      if Buffer'Length = 0 then
         return;
      end if;

      --  Read from /dev/urandom (POSIX cryptographic random source)
      --  This is non-blocking and suitable for cryptographic use
      Open (F, In_File, "/dev/urandom");

      Read (F, SEA, Last);

      if Last /= SEA'Last then
         Close (F);
         raise Program_Error with
            "/dev/urandom read incomplete: expected " &
            Stream_Element_Offset'Image (SEA'Length) &
            " bytes, got " & Stream_Element_Offset'Image (Last);
      end if;

      Close (F);

      --  Copy the random bytes to the output buffer
      --  Stream_Element and U8 are both mod 256, so this is safe
      for I in Buffer'Range loop
         Buffer (I) := U8 (SEA (Stream_Element_Offset (I - Buffer'First + 1)));
      end loop;
   end Fill;

end SparkPass.Crypto.Random;
