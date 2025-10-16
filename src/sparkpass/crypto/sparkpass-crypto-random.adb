pragma SPARK_Mode (On);
with Interfaces.C;
with System;
with Bindings.Libsodium;

package body SparkPass.Crypto.Random is

   Initialized : Boolean := False;

   function Ensure_Initialized return Boolean is
      use Interfaces.C;
   begin
      if not Initialized then
         if Bindings.Libsodium.Sodium_Init < 0 then
            return False;
         end if;
         Initialized := True;
      end if;
      return True;
   end Ensure_Initialized;

   procedure Fill (Buffer : in out Byte_Array) is
   begin
      if Buffer'Length = 0 then
         return;
      end if;

      if not Ensure_Initialized then
         raise Program_Error with "libsodium initialization failed";
      end if;

      Bindings.Libsodium.Randombytes_Buf
        (Buffer (Buffer'First)'Address,
         Interfaces.C.size_t (Buffer'Length));
   end Fill;

end SparkPass.Crypto.Random;
