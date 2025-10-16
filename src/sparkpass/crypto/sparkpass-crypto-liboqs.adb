pragma SPARK_Mode (On);
with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Bindings.LibOQS; use type Bindings.LibOQS.Kem_Handle; use type Bindings.LibOQS.Sig_Handle;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Random;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.LibOQS is

   function To_Natural (Value : Interfaces.C.size_t) return Natural is
   begin
      return Natural (Interfaces.C.size_t'Pos (Value));
   end To_Natural;

   function Self_Test return Boolean is
      use Interfaces.C;

      Kem_Name  : constant String := "ML-KEM-1024";
      Sig_Name  : constant String := "ML-DSA-87";

      Kem_Name_C : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Kem_Name);
      Sig_Name_C : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Sig_Name);

      Kem : Bindings.LibOQS.Kem_Handle := Bindings.LibOQS.OQS_KEM_New (Kem_Name_C);
      Sig : Bindings.LibOQS.Sig_Handle := null;

      Success   : Boolean := False;
   begin
      if Kem = null then
         goto Cleanup;
      end if;

      declare
         Public_Key  : Byte_Array (1 .. To_Natural (Kem.all.Length_Public_Key));
         Secret_Key  : Byte_Array (1 .. To_Natural (Kem.all.Length_Secret_Key));
         Ciphertext  : Byte_Array (1 .. To_Natural (Kem.all.Length_Ciphertext));
         Shared_Secret_Enc : Byte_Array (1 .. To_Natural (Kem.all.Length_Shared_Secret));
         Shared_Secret_Dec : Byte_Array (Shared_Secret_Enc'Range);
         Rv : Interfaces.C.int;
      begin
         Rv := Bindings.LibOQS.OQS_KEM_Keypair
           (Kem,
            Public_Key  (Public_Key'First)'Address,
            Secret_Key  (Secret_Key'First)'Address);
         if Rv /= 0 then
            goto Cleanup_KEM;
         end if;

         Rv := Bindings.LibOQS.OQS_KEM_Encaps
           (Kem,
            Ciphertext (Ciphertext'First)'Address,
            Shared_Secret_Enc (Shared_Secret_Enc'First)'Address,
            Public_Key (Public_Key'First)'Address);
         if Rv /= 0 then
            goto Cleanup_KEM;
         end if;

         Rv := Bindings.LibOQS.OQS_KEM_Decaps
           (Kem,
            Shared_Secret_Dec (Shared_Secret_Dec'First)'Address,
            Ciphertext (Ciphertext'First)'Address,
            Secret_Key (Secret_Key'First)'Address);
         if Rv /= 0 then
            goto Cleanup_KEM;
         end if;

         if Shared_Secret_Dec /= Shared_Secret_Enc then
            goto Cleanup_KEM;
         end if;

         Sig := Bindings.LibOQS.OQS_SIG_New (Sig_Name_C);
         if Sig = null then
            goto Cleanup_KEM;
         end if;

         declare
            Sig_Public : Byte_Array (1 .. To_Natural (Sig.all.Length_Public_Key));
            Sig_Secret : Byte_Array (1 .. To_Natural (Sig.all.Length_Secret_Key));
            Signature  : Byte_Array (1 .. To_Natural (Sig.all.Length_Signature));
            Signature_Len : aliased Interfaces.C.size_t := 0;
            Message : Byte_Array (1 .. 32);
         begin
            SparkPass.Crypto.Random.Fill (Message);

            Rv := Bindings.LibOQS.OQS_SIG_Keypair
              (Sig,
               Sig_Public (Sig_Public'First)'Address,
               Sig_Secret (Sig_Secret'First)'Address);
            if Rv /= 0 then
               goto Cleanup_SIG;
            end if;

            Rv := Bindings.LibOQS.OQS_SIG_Sign
              (Sig,
               Signature (Signature'First)'Address,
               Signature_Len'Access,
               Message (Message'First)'Address,
               Interfaces.C.size_t (Message'Length),
               Sig_Secret (Sig_Secret'First)'Address);
            if Rv /= 0 then
               goto Cleanup_SIG;
            end if;

            Rv := Bindings.LibOQS.OQS_SIG_Verify
              (Sig,
               Message (Message'First)'Address,
               Interfaces.C.size_t (Message'Length),
               Signature (Signature'First)'Address,
               Signature_Len,
               Sig_Public (Sig_Public'First)'Address);
            if Rv /= 0 then
               goto Cleanup_SIG;
            end if;

            Success := True;

         <<Cleanup_SIG>>
            SparkPass.Crypto.Zeroize.Wipe (Sig_Public);
            SparkPass.Crypto.Zeroize.Wipe (Sig_Secret);
            SparkPass.Crypto.Zeroize.Wipe (Signature);
            SparkPass.Crypto.Zeroize.Wipe (Message);
         end;

      <<Cleanup_KEM>>
         SparkPass.Crypto.Zeroize.Wipe (Public_Key);
         SparkPass.Crypto.Zeroize.Wipe (Secret_Key);
         SparkPass.Crypto.Zeroize.Wipe (Ciphertext);
         SparkPass.Crypto.Zeroize.Wipe (Shared_Secret_Enc);
         SparkPass.Crypto.Zeroize.Wipe (Shared_Secret_Dec);
      end;

   <<Cleanup>>
      if Kem /= null then
         Bindings.LibOQS.OQS_KEM_Free (Kem);
      end if;
      if Sig /= null then
         Bindings.LibOQS.OQS_SIG_Free (Sig);
      end if;
      Interfaces.C.Strings.Free (Kem_Name_C);
      Interfaces.C.Strings.Free (Sig_Name_C);
      return Success;
   end Self_Test;

end SparkPass.Crypto.LibOQS;
