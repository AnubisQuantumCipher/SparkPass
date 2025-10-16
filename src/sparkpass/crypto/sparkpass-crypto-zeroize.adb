pragma SPARK_Mode (On);
with System;
with Interfaces; use type Interfaces.Unsigned_8;
with Interfaces.C;
with Bindings.Libsodium;

package body SparkPass.Crypto.Zeroize is

   function To_Size (Length : Natural) return Interfaces.C.size_t is
      Result : constant Interfaces.C.size_t := Interfaces.C.size_t (Length);
   begin
      if Interfaces.C.size_t'Pos (Result) /= Length then
         raise Constraint_Error with "length exceeds size_t bound";
      end if;
      return Result;
   end To_Size;

   function Is_Zeroed (Buffer : Byte_Array) return Boolean is
   begin
      for Element of Buffer loop
         if Element /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zeroed;

   procedure Wipe_Buffer (Ptr : System.Address; Length : Interfaces.C.size_t) is
   begin
      Bindings.Libsodium.Sodium_Memzero (Ptr, Length);
   end Wipe_Buffer;

   procedure Wipe (Buffer : in out Byte_Array) is
   begin
      if Buffer'Length > 0 then
         Wipe_Buffer (Buffer (Buffer'First)'Address,
                      To_Size (Buffer'Length));
      end if;
   end Wipe;

   procedure Wipe_Key (Buffer : in out Key_Array) is
      Temp : Byte_Array (Buffer'Range);
      for Temp'Address use Buffer (Buffer'First)'Address;
   begin
      Wipe (Temp);
   end Wipe_Key;

   procedure Wipe_Tag (Buffer : in out Tag_Array) is
      Temp : Byte_Array (Buffer'Range);
      for Temp'Address use Buffer (Buffer'First)'Address;
   begin
      Wipe (Temp);
   end Wipe_Tag;

   procedure Wipe_Chain (Buffer : in out Chain_Key_Array) is
      Temp : Byte_Array (Buffer'Range);
      for Temp'Address use Buffer (Buffer'First)'Address;
   begin
      Wipe (Temp);
   end Wipe_Chain;

   function Equal (Left : Byte_Array; Right : Byte_Array) return Boolean is
      use Interfaces.C;
      Result : Interfaces.C.int;
   begin
      --  sodium_memcmp returns 0 if equal, -1 if different (constant-time)
      Result := Bindings.Libsodium.Sodium_Memcmp
        (Left (Left'First)'Address,
         Right (Right'First)'Address,
         To_Size (Left'Length));
      return Result = 0;
   end Equal;

end SparkPass.Crypto.Zeroize;
