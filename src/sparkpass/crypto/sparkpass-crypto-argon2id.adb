pragma SPARK_Mode (Off);  -- Uses FFI with libsodium for Argon2id
with System;
with Interfaces; use type Interfaces.Unsigned_32; use type Interfaces.Unsigned_8;
with Interfaces.C;
with Bindings.Libsodium;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.Argon2id is

   Initialized : Boolean := False;

   function Ensure_Initialized return Boolean is
      use Interfaces.C;
      Rc : Interfaces.C.int;
   begin
      if not Initialized then
          Rc := Bindings.Libsodium.Sodium_Init;
          if Rc < 0 then
             return False;
          end if;
          Initialized := True;
      end if;
      return True;
   end Ensure_Initialized;

   procedure Derive
     (Password : Byte_Array;
      Params   : Parameters;
      Output   : out Key_Array;
      Success  : out Boolean)
   is
      use Interfaces.C;
      use type Interfaces.C.int;

      Memlimit_Bytes : constant Interfaces.C.size_t :=
        Interfaces.C.size_t (Params.Memory_Cost) * Interfaces.C.size_t (1024);
      Result_Code    : Interfaces.C.int := -1;
   begin
      Output := (others => 0);
      Success := False;

      if Password'Length < 12 then
         return;
      end if;

      if not Ensure_Initialized then
         return;
      end if;

      Result_Code := Bindings.Libsodium.Crypto_Pwhash
        (Output_Buffer    => Output'Address,
         Output_Length    => Interfaces.C.size_t (Output'Length),
         Password_Buffer  => Password'Address,
         Password_Length  => Interfaces.C.size_t (Password'Length),
         Salt_Buffer      => Params.Salt'Address,
         Opslimit         => Interfaces.C.unsigned_long_long (Params.Iterations),
         Memlimit         => Memlimit_Bytes,
         Algorithm        => Bindings.Libsodium.Crypto_Pwhash_Alg_Argon2id13);

      if Result_Code = 0 then
         Success := True;
      else
         Zeroize (Output);
      end if;
   end Derive;

   procedure Zeroize (Value : in out Key_Array) is
   begin
      SparkPass.Crypto.Zeroize.Wipe (Value);
   end Zeroize;

end SparkPass.Crypto.Argon2id;
