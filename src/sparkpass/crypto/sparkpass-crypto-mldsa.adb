pragma SPARK_Mode (On);
with System;
with Interfaces.C; use type Interfaces.C.int;
with Interfaces.C.Strings;
with Bindings.LibOQS; use type Bindings.LibOQS.Sig_Handle;
with SparkPass.Config;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.MLDSA is

   MLDSA_Name : constant String := "ML-DSA-87";

   function To_Natural (Value : Interfaces.C.size_t) return Natural is
   begin
      return Natural (Interfaces.C.size_t'Pos (Value));
   end To_Natural;

   function To_Size_T (Length : Natural) return Interfaces.C.size_t is
      Result : constant Interfaces.C.size_t := Interfaces.C.size_t (Length);
   begin
      if Interfaces.C.size_t'Pos (Result) /= Length then
         raise Constraint_Error with "message length exceeds size_t range";
      end if;
      return Result;
   end To_Size_T;

   function Acquire_Signature return Bindings.LibOQS.Sig_Handle is
      Name : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (MLDSA_Name);
      Sig  : constant Bindings.LibOQS.Sig_Handle := Bindings.LibOQS.OQS_SIG_New (Name);
   begin
      Interfaces.C.Strings.Free (Name);
      return Sig;
   end Acquire_Signature;

   procedure Release_Signature (Sig : in out Bindings.LibOQS.Sig_Handle) is
   begin
      if Sig /= null then
         Bindings.LibOQS.OQS_SIG_Free (Sig);
         Sig := null;
      end if;
   end Release_Signature;

   procedure Ensure_Lengths
     (Sig_Handle    : Bindings.LibOQS.Sig_Handle;
      Public_Len    : Natural;
      Secret_Len    : Natural;
      Signature_Len : Natural) is
   begin
      if Sig_Handle = null then
         raise Program_Error with "OQS_SIG_new returned null";
      end if;

      if To_Natural (Sig_Handle.all.Length_Public_Key) /= Public_Len then
         raise Program_Error with "ML-DSA public key length mismatch";
      end if;

      if To_Natural (Sig_Handle.all.Length_Secret_Key) /= Secret_Len then
         raise Program_Error with "ML-DSA secret key length mismatch";
      end if;

      if To_Natural (Sig_Handle.all.Length_Signature) /= Signature_Len then
         raise Program_Error with "ML-DSA signature length mismatch";
      end if;
   end Ensure_Lengths;

   procedure Wipe (Buffer : in out Byte_Array) is
      View : Byte_Array (Buffer'Range);
      for View'Address use Buffer (Buffer'First)'Address;
   begin
      SparkPass.Crypto.Zeroize.Wipe (View);
   end Wipe;

   procedure Keypair (Public : out Public_Key; Secret : out Secret_Key) is
      Sig_Handle : Bindings.LibOQS.Sig_Handle := Acquire_Signature;
      Result     : Interfaces.C.int := 0;
   begin
      begin
         Ensure_Lengths (Sig_Handle,
                         Public'Length,
                         Secret'Length,
                         SparkPass.Config.MLDsa_Signature_Length);

         Result := Bindings.LibOQS.OQS_SIG_Keypair
           (Sig_Handle,
            Public (Public'First)'Address,
            Secret (Secret'First)'Address);

         if Result /= 0 then
            Wipe (Public);
            Wipe (Secret);
            raise Program_Error with "ML-DSA keypair failed";
         end if;

      exception
         when others =>
            Release_Signature (Sig_Handle);
            raise;
      end;

      Release_Signature (Sig_Handle);
   end Keypair;

   procedure Sign
     (Secret  : Secret_Key;
      Message : Byte_Array;
      Output  : out Signature)
   is
      Sig_Handle : Bindings.LibOQS.Sig_Handle := Acquire_Signature;
      Result     : Interfaces.C.int := 0;
      Sig_Length : aliased Interfaces.C.size_t := 0;
   begin
      begin
         Ensure_Lengths (Sig_Handle,
                         SparkPass.Config.MLDsa_Public_Key_Length,
                         Secret'Length,
                         Output'Length);

         declare
            Message_Length : constant Interfaces.C.size_t := To_Size_T (Message'Length);
         begin
            Output := (others => 0);
            Result := Bindings.LibOQS.OQS_SIG_Sign
              (Sig_Handle,
               Output (Output'First)'Address,
               Sig_Length'Access,
               Message (Message'First)'Address,
               Message_Length,
               Secret (Secret'First)'Address);
         end;

         if Result /= 0 then
            Wipe (Output);
            raise Program_Error with "ML-DSA sign failed";
         end if;

         if To_Natural (Sig_Length) /= Output'Length then
            Wipe (Output);
            raise Program_Error with "ML-DSA signature length mismatch";
         end if;

      exception
         when others =>
            Release_Signature (Sig_Handle);
            raise;
      end;

      Release_Signature (Sig_Handle);
   end Sign;

   procedure Verify
     (Public  : Public_Key;
      Message : Byte_Array;
      Sig     : Signature;
      Success : out Boolean)
   is
      Sig_Handle : Bindings.LibOQS.Sig_Handle := Acquire_Signature;
      Result     : Interfaces.C.int := 0;
   begin
      begin
         Ensure_Lengths (Sig_Handle,
                         Public'Length,
                         SparkPass.Config.MLDsa_Secret_Key_Length,
                         Sig'Length);

         declare
            Message_Length : constant Interfaces.C.size_t := To_Size_T (Message'Length);
         begin
            Result := Bindings.LibOQS.OQS_SIG_Verify
              (Sig_Handle,
               Message (Message'First)'Address,
               Message_Length,
               Sig (Sig'First)'Address,
               To_Size_T (Sig'Length),
               Public (Public'First)'Address);
         end;

         Success := Result = 0;

      exception
         when others =>
            Release_Signature (Sig_Handle);
            raise;
      end;

      Release_Signature (Sig_Handle);
   end Verify;

end SparkPass.Crypto.MLDSA;
