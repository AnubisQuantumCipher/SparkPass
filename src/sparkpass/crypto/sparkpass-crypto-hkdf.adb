pragma SPARK_Mode (Off);  -- Uses FFI with libsodium for HMAC-SHA512
with System;
with Interfaces; use type Interfaces.Unsigned_8;
with Interfaces.C;
with Bindings.Libsodium;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.HKDF is

   Hash_Length        : constant Positive := 48; -- SHA-384 output length

   procedure Sha512 (Input : Byte_Array; Output : out Hash64_Array) is
      use Interfaces.C;
      Input_Address : System.Address := Input (Input'First)'Address;
      Input_Length  : Interfaces.C.unsigned_long_long := Interfaces.C.unsigned_long_long (Input'Length);
   begin
      if Bindings.Libsodium.Crypto_Hash_Sha512
        (Output (Output'First)'Address,
         Input_Address,
         Input_Length) /= 0
      then
         raise Program_Error with "crypto_hash_sha512 failed";
      end if;
   end Sha512;

   function HMAC (Key : Byte_Array; Data : Byte_Array) return Hash64_Array is
      use Interfaces.C;
      Result      : Hash64_Array;
      Key_Address : System.Address := Key (Key'First)'Address;
      Data_Address : System.Address := Data (Data'First)'Address;
      Key_Length  : Interfaces.C.unsigned_long_long := Interfaces.C.unsigned_long_long (Key'Length);
      Data_Length : Interfaces.C.unsigned_long_long := Interfaces.C.unsigned_long_long (Data'Length);
   begin
      --  Use libsodium's crypto_auth_hmacsha512 instead of custom implementation
      --  This provides better assurance through battle-tested, audited code
      if Bindings.Libsodium.Crypto_Auth_Hmacsha512
        (Result (Result'First)'Address,
         Data_Address,
         Data_Length,
         Key_Address) /= 0
      then
         raise Program_Error with "crypto_auth_hmacsha512 failed";
      end if;

      return Result;
   end HMAC;

   function Derive
     (IKM    : Byte_Array;
      Salt   : Byte_Array;
      Info   : Byte_Array;
      Length : Positive) return Byte_Array
   is
      subtype Hash48_Array is Byte_Array (1 .. Hash_Length);

      PRK_Full : Hash64_Array := HMAC (Salt, IKM);
      PRK      : Hash48_Array;
   begin
      for I in PRK'Range loop
         PRK (I) := PRK_Full (I);
      end loop;

      declare
         Block_Count : constant Positive := (Length + Hash_Length - 1) / Hash_Length;
         Result      : Byte_Array (1 .. Length);
         Offset      : Natural := 0;
         Previous    : Hash48_Array := (others => 0);
      begin
         for Counter in 1 .. Block_Count loop
            declare
               Prev_Length  : constant Natural := (if Counter = 1 then 0 else Hash_Length);
               Input_Length : constant Natural := Prev_Length + Info'Length + 1;
               Block_Input  : Byte_Array (1 .. Input_Length);
            begin
               if Prev_Length > 0 then
                  for I in Previous'Range loop
                     Block_Input (I) := Previous (I);
                  end loop;
               end if;

               for I in Info'Range loop
                  Block_Input (Prev_Length + I - Info'First + 1) := Info (I);
               end loop;

               Block_Input (Block_Input'Last) := Interfaces.Unsigned_8 (Counter);

               declare
                  Block_Hash : Hash64_Array := HMAC (PRK, Block_Input);
               begin
                  for I in 1 .. Hash_Length loop
                     Previous (I) := Block_Hash (I);
                  end loop;

                  declare
                     Remaining : constant Natural := Length - Offset;
                     To_Copy   : constant Natural := (if Remaining >= Hash_Length then Hash_Length else Remaining);
                  begin
                     for I in 1 .. To_Copy loop
                        Result (Offset + I) := Previous (I);
                     end loop;
                     Offset := Offset + To_Copy;
                  end;

                  SparkPass.Crypto.Zeroize.Wipe (Byte_Array (Block_Hash));
               end;

               SparkPass.Crypto.Zeroize.Wipe (Block_Input);
            end;
         end loop;

         SparkPass.Crypto.Zeroize.Wipe (PRK);
         SparkPass.Crypto.Zeroize.Wipe (Byte_Array (PRK_Full));
         SparkPass.Crypto.Zeroize.Wipe (Previous);
         return Result;
      end;
   end Derive;

end SparkPass.Crypto.HKDF;
