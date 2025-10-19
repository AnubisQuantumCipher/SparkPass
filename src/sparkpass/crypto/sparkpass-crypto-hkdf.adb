pragma SPARK_Mode (On);  -- Pure SPARK using SparkPass.Crypto.HMAC
with Interfaces; use type Interfaces.Unsigned_8;
with SparkPass.Crypto.HMAC;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.HKDF is

   Hash_Length        : constant Positive := 48; -- SHA-384 output length

   function HMAC (Key : Byte_Array; Data : Byte_Array) return Hash64_Array is
      Result      : Hash64_Array;
      Key_Norm    : Byte_Array (1 .. Key'Length) := Key;
      Data_Norm   : Byte_Array (1 .. Data'Length) := Data;
   begin
      --  Use pure SPARK HMAC-SHA512 (RFC 2104 compliant)
      --  Provides cryptographic security with SPARK-proven memory safety
      SparkPass.Crypto.HMAC.HMAC_SHA512 (Key_Norm, Data_Norm, Result);
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
