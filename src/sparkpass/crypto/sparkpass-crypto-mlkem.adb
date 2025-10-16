pragma SPARK_Mode (On);
with Interfaces.C; use type Interfaces.C.int;
with Interfaces.C.Strings;
with Bindings.LibOQS; use type Bindings.LibOQS.Kem_Handle;
with SparkPass.Config;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.MLKEM is

   MLKEM_Name : constant String := "ML-KEM-1024";

   function To_Natural (Value : Interfaces.C.size_t) return Natural is
   begin
      return Natural (Interfaces.C.size_t'Pos (Value));
   end To_Natural;

   function Acquire_KEM return Bindings.LibOQS.Kem_Handle is
      Name : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (MLKEM_Name);
      Kem  : Bindings.LibOQS.Kem_Handle := Bindings.LibOQS.OQS_KEM_New (Name);
   begin
      Interfaces.C.Strings.Free (Name);
      return Kem;
   end Acquire_KEM;

   procedure Release_KEM (Kem : in out Bindings.LibOQS.Kem_Handle) is
   begin
      if Kem /= null then
         Bindings.LibOQS.OQS_KEM_Free (Kem);
         Kem := null;
      end if;
   end Release_KEM;

   procedure Ensure_Lengths
     (Kem          : Bindings.LibOQS.Kem_Handle;
      Public_Len   : Natural;
      Secret_Len   : Natural;
      Cipher_Len   : Natural;
      Shared_Len   : Natural) is
   begin
      if Kem = null then
         raise Program_Error with "OQS_KEM_new returned null";
      end if;

      if To_Natural (Kem.all.Length_Public_Key) /= Public_Len then
         raise Program_Error with "ML-KEM public key length mismatch";
      end if;

      if To_Natural (Kem.all.Length_Secret_Key) /= Secret_Len then
         raise Program_Error with "ML-KEM secret key length mismatch";
      end if;

      if To_Natural (Kem.all.Length_Ciphertext) /= Cipher_Len then
         raise Program_Error with "ML-KEM ciphertext length mismatch";
      end if;

      if To_Natural (Kem.all.Length_Shared_Secret) /= Shared_Len then
         raise Program_Error with "ML-KEM shared secret length mismatch";
      end if;
   end Ensure_Lengths;

   procedure Wipe (Buffer : in out Byte_Array) is
      View : Byte_Array (Buffer'Range);
      for View'Address use Buffer (Buffer'First)'Address;
   begin
      SparkPass.Crypto.Zeroize.Wipe (View);
   end Wipe;

   procedure Keypair (Public : out Public_Key; Secret : out Secret_Key) is
      Kem    : Bindings.LibOQS.Kem_Handle := Acquire_KEM;
      Result : Interfaces.C.int := 0;
   begin
      begin
         Ensure_Lengths (Kem,
                         Public'Length,
                         Secret'Length,
                         SparkPass.Config.MLKem_Ciphertext_Length,
                         SparkPass.Config.MLKem_Shared_Key_Length);

         Result := Bindings.LibOQS.OQS_KEM_Keypair
           (Kem,
            Public (Public'First)'Address,
            Secret (Secret'First)'Address);

         if Result /= 0 then
            Wipe (Public);
            Wipe (Secret);
            raise Program_Error with "ML-KEM keypair failed";
         end if;

      exception
         when others =>
            Release_KEM (Kem);
            raise;
      end;

      Release_KEM (Kem);
   end Keypair;

   procedure Encapsulate
     (Public     : Public_Key;
      Cipher     : out Ciphertext;
      Shared     : out Shared_Key;
      Success    : out Boolean)
   is
      Kem    : Bindings.LibOQS.Kem_Handle := Acquire_KEM;
      Result : Interfaces.C.int := 0;
   begin
      begin
         Ensure_Lengths (Kem,
                         Public'Length,
                         SparkPass.Config.MLKem_Secret_Key_Length,
                         Cipher'Length,
                         Shared'Length);

         Shared := (others => 0);
         Success := False;

         Result := Bindings.LibOQS.OQS_KEM_Encaps
           (Kem,
            Cipher (Cipher'First)'Address,
            Shared (Shared'First)'Address,
            Public (Public'First)'Address);

         if Result = 0 then
            Success := True;
         else
            Wipe (Cipher);
            Wipe (Shared);
         end if;

      exception
         when others =>
            Release_KEM (Kem);
            raise;
      end;

      Release_KEM (Kem);
   end Encapsulate;

   procedure Decapsulate
     (Secret     : Secret_Key;
      Cipher     : Ciphertext;
      Shared     : out Shared_Key;
      Success    : out Boolean)
   is
      Kem    : Bindings.LibOQS.Kem_Handle := Acquire_KEM;
      Result : Interfaces.C.int := 0;
   begin
      begin
         Ensure_Lengths (Kem,
                         SparkPass.Config.MLKem_Public_Key_Length,
                         Secret'Length,
                         Cipher'Length,
                         Shared'Length);

         Shared := (others => 0);

         Result := Bindings.LibOQS.OQS_KEM_Decaps
           (Kem,
            Shared (Shared'First)'Address,
            Cipher (Cipher'First)'Address,
            Secret (Secret'First)'Address);

         if Result = 0 then
            Success := True;
         else
            Success := False;
            Wipe (Shared);
         end if;

      exception
         when others =>
            Release_KEM (Kem);
            raise;
      end;

      Release_KEM (Kem);
   end Decapsulate;

end SparkPass.Crypto.MLKEM;
