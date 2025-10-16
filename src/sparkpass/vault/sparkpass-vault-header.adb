pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_8; use type Interfaces.Unsigned_16; use type Interfaces.Unsigned_32; use type Interfaces.Unsigned_64;
with SparkPass.Config;
with SparkPass.Crypto.Argon2id;
with SparkPass.Crypto.AES_GCM_SIV;
with SparkPass.Crypto.HKDF;
with SparkPass.Crypto.MLDSA;
with SparkPass.Crypto.MLKEM;
with SparkPass.Crypto.Random;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Vault.Header is

   procedure Fill_Nonce (Buffer : in out Nonce_Array) is
   begin
      SparkPass.Crypto.Random.Fill (Buffer);
   end Fill_Nonce;

   procedure Initialize
     (State     : out SparkPass.Types.Header;
      Password  : Byte_Array;
      Timestamp : U64;
      Master    : out Key_Array;
      Chain     : out Chain_Key_Array;
      Wrap_Key  : out Key_Array;
      Signing   : out SparkPass.Types.MLDsa_Secret_Key_Array)
   is
      Params : SparkPass.Crypto.Argon2id.Parameters;
      Derived : Key_Array := (others => 0);
      Derive_Success : Boolean := False;
      HKDF_Info    : constant Byte_Array := (1 => 0);
   begin
      State.Magic   := SparkPass.Config.Magic_Text;
      State.Version := SparkPass.Config.Version;
      State.Created_At  := Timestamp;
      State.Modified_At := Timestamp;
      State.Nonce_Counter := 0;
      State.Entry_Count   := 0;

      SparkPass.Crypto.Random.Fill (Params.Salt);
      State.Argon2_Memory     := Params.Memory_Cost;
      State.Argon2_Iterations := Params.Iterations;
      State.Argon2_Parallelism := Params.Parallelism;
      for Index in State.Argon2_Salt'Range loop
         State.Argon2_Salt (Index) := Params.Salt (Index);
      end loop;

      declare
         type Profile is record
            Memory      : Interfaces.Unsigned_32;
            Iterations  : Interfaces.Unsigned_32;
         end record;
         Profiles : constant array (Positive range <>) of Profile :=
           ((SparkPass.Config.Argon2_Memory_KiB, SparkPass.Config.Argon2_Iterations),
            (65_536, 1),
            (8_192, 1));
      begin
         for Candidate of Profiles loop
            Params.Memory_Cost := Candidate.Memory;
            Params.Iterations  := Candidate.Iterations;
            State.Argon2_Memory     := Params.Memory_Cost;
            State.Argon2_Iterations := Params.Iterations;
            SparkPass.Crypto.Argon2id.Derive (Password, Params, Derived, Derive_Success);
            exit when Derive_Success;
         end loop;

         if not Derive_Success then
            raise Program_Error with "Argon2id derive failed";
         end if;
      end;
      Wrap_Key := Derived;
      SparkPass.Crypto.Random.Fill (Master);
      SparkPass.Crypto.Random.Fill (Chain);

      Fill_Nonce (State.Wrapped_Master_Nonce);
      SparkPass.Crypto.AES_GCM_SIV.Seal
        (Key        => Wrap_Key,
         Nonce      => State.Wrapped_Master_Nonce,
         Plaintext  => Master,
         AAD        => State.Argon2_Salt,
         Ciphertext => State.Wrapped_Master_Key,
         Tag        => State.Wrapped_Master_Tag);

      Fill_Nonce (State.Chain_Key_Nonce);
      declare
         Chain_Key_Bytes : constant Byte_Array := Chain;
         Chain_Info      : Byte_Array := SparkPass.Crypto.HKDF.Derive (Wrap_Key, State.Argon2_Salt, HKDF_Info, Chain'Length);
      begin
         SparkPass.Crypto.AES_GCM_SIV.Seal
           (Key        => Wrap_Key,
            Nonce      => State.Chain_Key_Nonce,
            Plaintext  => Chain_Key_Bytes,
            AAD        => Chain_Info,
            Ciphertext => State.Chain_Key_Value,
            Tag        => State.Chain_Key_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Chain_Info);
      end;

      declare
         Public : MLDsa_Public_Key_Array;
      begin
         SparkPass.Crypto.MLDSA.Keypair (Public, Signing);
         State.MLDsa_Public_Key := Public;
         State.MLDsa_Secret_Key := Signing;
         State.Has_MLDsa_Secret := True;
         SparkPass.Crypto.Zeroize.Wipe (Public);
      end;

      Fill_Nonce (State.MLDsa_Secret_Nonce);
      SparkPass.Crypto.AES_GCM_SIV.Seal
        (Key        => Wrap_Key,
         Nonce      => State.MLDsa_Secret_Nonce,
         Plaintext  => Signing,
         AAD        => State.Argon2_Salt,
         Ciphertext => State.MLDsa_Secret_Value,
         Tag        => State.MLDsa_Secret_Tag);

      declare
         Public    : MLKem_Public_Key_Array;
         Secret_SK : MLKem_Secret_Key_Array;
      begin
         SparkPass.Crypto.MLKEM.Keypair (Public, Secret_SK);
         State.MLKem_Public_Key := Public;
         State.MLKem_Secret_Key := Secret_SK;
         State.Has_MLKem_Secret := True;
         SparkPass.Crypto.Zeroize.Wipe (Public);
      end;

      --  Wrap ML-KEM secret key (encrypted with wrap key for storage)
      Fill_Nonce (State.MLKem_Secret_Nonce);
      SparkPass.Crypto.AES_GCM_SIV.Seal
        (Key        => Wrap_Key,
         Nonce      => State.MLKem_Secret_Nonce,
         Plaintext  => State.MLKem_Secret_Key,
         AAD        => State.Argon2_Salt,
         Ciphertext => State.MLKem_Secret_Value,
         Tag        => State.MLKem_Secret_Tag);

      Refresh_Fingerprint (State);

      declare
         Signature : MLDsa_Signature_Array;
      begin
         SparkPass.Crypto.MLDSA.Sign (State.MLDsa_Secret_Key, State.Vault_Fingerprint, Signature);
         State.Header_Signature := Signature;
         SparkPass.Crypto.Zeroize.Wipe (Signature);
      end;

      SparkPass.Crypto.Argon2id.Zeroize (Derived);
      SparkPass.Crypto.Zeroize.Wipe (Params.Salt);
   end Initialize;

   procedure Bump
     (State     : in out SparkPass.Types.Header;
      Timestamp : U64) is
   begin
      State.Nonce_Counter := State.Nonce_Counter + 1;
      State.Modified_At   := Timestamp;
      Refresh_Fingerprint (State);
      Update_Signature (State);
   end Bump;

   function Compute_Fingerprint (State : SparkPass.Types.Header) return Fingerprint_Array is
      --  Fingerprint = HMAC-SHA512 over cryptographic material only
      --  Includes: wrapped secrets + public keys + Argon2 salt
      --  Excludes: timestamps, counters, flags (these change without affecting security)

      Message_Size : constant Positive :=
        State.Argon2_Salt'Length +          -- 32 bytes
        State.Wrapped_Master_Nonce'Length + -- 12 bytes
        State.Wrapped_Master_Key'Length +   -- 32 bytes
        State.Wrapped_Master_Tag'Length +   -- 16 bytes
        State.Chain_Key_Nonce'Length +      -- 12 bytes
        State.Chain_Key_Value'Length +      -- 32 bytes
        State.Chain_Key_Tag'Length +        -- 16 bytes
        State.MLDsa_Secret_Nonce'Length +   -- 12 bytes
        State.MLDsa_Secret_Value'Length +   -- 4896 bytes
        State.MLDsa_Secret_Tag'Length +     -- 16 bytes
        State.MLKem_Secret_Nonce'Length +   -- 12 bytes
        State.MLKem_Secret_Value'Length +   -- 3168 bytes
        State.MLKem_Secret_Tag'Length +     -- 16 bytes
        State.Wrapped_D_Nonce'Length +      -- 12 bytes (Touch ID Wrap D)
        State.Wrapped_D_Key'Length +        -- 32 bytes
        State.Wrapped_D_Tag'Length +        -- 16 bytes
        State.MLDsa_Public_Key'Length +     -- 2592 bytes
        State.MLKem_Public_Key'Length;      -- 1568 bytes

      Message : Byte_Array (1 .. Message_Size);
      Offset  : Positive := Message'First;
      Hash    : SparkPass.Crypto.HKDF.Hash64_Array;
      Result  : Fingerprint_Array;

   begin
      --  Include only immutable cryptographic material
      for Byte of State.Argon2_Salt loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.Wrapped_Master_Nonce loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.Wrapped_Master_Key loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.Wrapped_Master_Tag loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.Chain_Key_Nonce loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.Chain_Key_Value loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.Chain_Key_Tag loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.MLDsa_Secret_Nonce loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.MLDsa_Secret_Value loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.MLDsa_Secret_Tag loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.MLKem_Secret_Nonce loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.MLKem_Secret_Value loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.MLKem_Secret_Tag loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      --  Include Wrap D (Touch ID) fields
      for Byte of State.Wrapped_D_Nonce loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.Wrapped_D_Key loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.Wrapped_D_Tag loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.MLDsa_Public_Key loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      for Byte of State.MLKem_Public_Key loop
         Message (Offset) := Byte;
         Offset := Offset + 1;
      end loop;

      --  Compute HMAC-SHA512 using Argon2 salt as key
      Hash := SparkPass.Crypto.HKDF.HMAC (State.Argon2_Salt, Message);

      --  Truncate 64-byte hash to 32 bytes
      for Index in Result'Range loop
         Result (Index) := Hash (Hash'First + (Index - Result'First));
      end loop;

      SparkPass.Crypto.Zeroize.Wipe (Message);
      SparkPass.Crypto.Zeroize.Wipe (Byte_Array (Hash));
      return Result;
   end Compute_Fingerprint;

   procedure Refresh_Fingerprint (State : in out SparkPass.Types.Header) is
   begin
      State.Vault_Fingerprint := Compute_Fingerprint (State);
   end Refresh_Fingerprint;

   function Has_Signing_Key (State : SparkPass.Types.Header) return Boolean is
   begin
      return State.Has_MLDsa_Secret;
   end Has_Signing_Key;

   procedure Update_Signature (State : in out SparkPass.Types.Header) is
      Signature : MLDsa_Signature_Array;
   begin
      SparkPass.Crypto.MLDSA.Sign (State.MLDsa_Secret_Key, State.Vault_Fingerprint, Signature);
      State.Header_Signature := Signature;
   end Update_Signature;

   procedure Clear_Signing_Key (State : in out SparkPass.Types.Header) is
   begin
      SparkPass.Crypto.Zeroize.Wipe (State.MLDsa_Secret_Key);
      State.Has_MLDsa_Secret := False;
   end Clear_Signing_Key;

   function Verify_Password
     (State    : SparkPass.Types.Header;
      Password : Byte_Array) return Boolean
   is
      Params : SparkPass.Crypto.Argon2id.Parameters;
      Derived : Key_Array;
      Plain   : Key_Array := (others => 0);
      Success : Boolean := False;
   begin
      Params.Memory_Cost := State.Argon2_Memory;
      Params.Iterations  := State.Argon2_Iterations;
      Params.Parallelism := State.Argon2_Parallelism;
      for Index in Params.Salt'Range loop
         Params.Salt (Index) := State.Argon2_Salt (Index);
      end loop;

      SparkPass.Crypto.Argon2id.Derive (Password, Params, Derived, Success);
      if not Success then
         return False;
      end if;
      SparkPass.Crypto.AES_GCM_SIV.Open
        (Key        => Derived,
         Nonce      => State.Wrapped_Master_Nonce,
         Ciphertext => State.Wrapped_Master_Key,
         AAD        => State.Argon2_Salt,
         Tag        => State.Wrapped_Master_Tag,
         Plaintext  => Plain,
         Success    => Success);

      SparkPass.Crypto.Argon2id.Zeroize (Derived);
      SparkPass.Crypto.Argon2id.Zeroize (Plain);
      return Success;
   end Verify_Password;

end SparkPass.Vault.Header;
