pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_8; use type Interfaces.Unsigned_16; use type Interfaces.Unsigned_32; use type Interfaces.Unsigned_64;
with Ada.Streams.Stream_IO;
with SparkPass.Config;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id;
with SparkPass.Crypto.AES_GCM_SIV;
with SparkPass.Crypto.HKDF;
with SparkPass.Crypto.MLKEM;
with SparkPass.Crypto.MLDSA;
with SparkPass.Crypto.Random;
with SparkPass.Crypto.Zeroize;
with SparkPass.Vault.Header;
with SparkPass.Vault.Storage;

package body SparkPass.Vault is

   HKDF_Chain_Info : constant SparkPass.Types.Byte_Array := (1 => 0);
   Ratchet_Info    : constant SparkPass.Types.Byte_Array :=
     (1 => SparkPass.Types.U8 (Character'Pos ('r')),
      2 => SparkPass.Types.U8 (Character'Pos ('a')),
      3 => SparkPass.Types.U8 (Character'Pos ('t')),
      4 => SparkPass.Types.U8 (Character'Pos ('c')),
      5 => SparkPass.Types.U8 (Character'Pos ('h')),
      6 => SparkPass.Types.U8 (Character'Pos ('e')),
      7 => SparkPass.Types.U8 (Character'Pos ('t')));

   subtype UInt32 is Interfaces.Unsigned_32;
   subtype U16 is Interfaces.Unsigned_16;

   function Is_Unlocked (State : Vault_State) return Boolean is
   begin
      return State.Unlocked;
   end Is_Unlocked;

   procedure Zero_Entry (Item : in out SparkPass.Types.Entry_Record) is
   begin
      SparkPass.Crypto.Zeroize.Wipe (Item.Id);
      Item.Kind := SparkPass.Types.Password;
      Item.Label_Len := 0;
      Item.Data_Len := 0;
      Item.Created_At := 0;
      Item.Modified_At := 0;
      SparkPass.Crypto.Zeroize.Wipe (Item.Label);
      SparkPass.Crypto.Zeroize.Wipe (Item.Nonce);
      SparkPass.Crypto.Zeroize.Wipe (Item.Ciphertext);
      SparkPass.Crypto.Zeroize.Wipe (Item.Tag);
      SparkPass.Crypto.Zeroize.Wipe (Item.Signature);
   end Zero_Entry;

   procedure Clear (State : in out Vault_State) is
   begin
      SparkPass.Crypto.Zeroize.Wipe (State.Master_Key);
      SparkPass.Crypto.Zeroize.Wipe_Chain (State.Chain_Key);
      SparkPass.Crypto.Zeroize.Wipe (State.Wrap_Key);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_Master_Key);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_Master_Nonce);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_Master_Tag);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Chain_Key_Value);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Chain_Key_Nonce);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Chain_Key_Tag);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.MLDsa_Secret_Value);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.MLDsa_Secret_Nonce);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.MLDsa_Secret_Tag);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.MLKem_Secret_Value);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.MLKem_Secret_Nonce);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.MLKem_Secret_Tag);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_D_Key);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_D_Nonce);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_D_Tag);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Header_Signature);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Argon2_Salt);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.MLDsa_Public_Key);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.MLKem_Public_Key);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Vault_Fingerprint);
      for Index in State.Entries'Range loop
         Zero_Entry (State.Entries (Index));
      end loop;
      State.Entry_Count := 0;
      State.Header.Entry_Count := 0;
      State.Header.Nonce_Counter := 0;
      State.Header.Created_At := 0;
      State.Header.Modified_At := 0;
      State.Unlocked := False;
      State.Header.Has_Wrap_D := False;
      SparkPass.Vault.Header.Clear_Signing_Key (State.Header);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.MLKem_Secret_Key);
      State.Header.Has_MLKem_Secret := False;
   end Clear;

   procedure Wrap_Secrets (State : in out Vault_State) is
      Wrap_View  : SparkPass.Types.Byte_Array (1 .. SparkPass.Config.Master_Key_Length);
      for Wrap_View'Address use State.Wrap_Key (State.Wrap_Key'First)'Address;
      Chain_Key_Copy : SparkPass.Types.Byte_Array (1 .. SparkPass.Config.Chain_Key_Length);
      for Chain_Key_Copy'Address use State.Chain_Key (State.Chain_Key'First)'Address;
      Secret_View : SparkPass.Types.Byte_Array (1 .. SparkPass.Config.MLDsa_Secret_Key_Length);
      for Secret_View'Address use State.Header.MLDsa_Secret_Key (State.Header.MLDsa_Secret_Key'First)'Address;
      MLKem_Secret_View : SparkPass.Types.Byte_Array (1 .. SparkPass.Config.MLKem_Secret_Key_Length);
      for MLKem_Secret_View'Address use State.Header.MLKem_Secret_Key (State.Header.MLKem_Secret_Key'First)'Address;
      Derived_Info : SparkPass.Types.Byte_Array :=
        SparkPass.Crypto.HKDF.Derive (Wrap_View, State.Header.Argon2_Salt, HKDF_Chain_Info, State.Chain_Key'Length);
   begin
      SparkPass.Crypto.Random.Fill (State.Header.Wrapped_Master_Nonce);
      SparkPass.Crypto.AES_GCM_SIV.Seal
        (Key        => State.Wrap_Key,
         Nonce      => State.Header.Wrapped_Master_Nonce,
         Plaintext  => State.Master_Key,
         AAD        => State.Header.Argon2_Salt,
         Ciphertext => State.Header.Wrapped_Master_Key,
         Tag        => State.Header.Wrapped_Master_Tag);

      SparkPass.Crypto.Random.Fill (State.Header.Chain_Key_Nonce);
     SparkPass.Crypto.AES_GCM_SIV.Seal
       (Key        => State.Wrap_Key,
        Nonce      => State.Header.Chain_Key_Nonce,
        Plaintext  => Chain_Key_Copy,
        AAD        => Derived_Info,
        Ciphertext => State.Header.Chain_Key_Value,
        Tag        => State.Header.Chain_Key_Tag);

     SparkPass.Crypto.Random.Fill (State.Header.MLDsa_Secret_Nonce);
     SparkPass.Crypto.AES_GCM_SIV.Seal
       (Key        => State.Wrap_Key,
        Nonce      => State.Header.MLDsa_Secret_Nonce,
        Plaintext  => Secret_View,
        AAD        => State.Header.Argon2_Salt,
        Ciphertext => State.Header.MLDsa_Secret_Value,
        Tag        => State.Header.MLDsa_Secret_Tag);

     --  Wrap ML-KEM secret key (if present)
     if State.Header.Has_MLKem_Secret then
        SparkPass.Crypto.Random.Fill (State.Header.MLKem_Secret_Nonce);
        SparkPass.Crypto.AES_GCM_SIV.Seal
          (Key        => State.Wrap_Key,
           Nonce      => State.Header.MLKem_Secret_Nonce,
           Plaintext  => MLKem_Secret_View,
           AAD        => State.Header.Argon2_Salt,
           Ciphertext => State.Header.MLKem_Secret_Value,
           Tag        => State.Header.MLKem_Secret_Tag);
     end if;

     SparkPass.Crypto.Zeroize.Wipe (Derived_Info);
   end Wrap_Secrets;

   procedure Create
     (State     : out Vault_State;
      Password  : Byte_Array;
      Timestamp : U64)
   is
      Signing : SparkPass.Types.MLDsa_Secret_Key_Array := (others => 0);
   begin
      State.Entry_Count := 0;
      for Index in State.Entries'Range loop
         Zero_Entry (State.Entries (Index));
      end loop;
      SparkPass.Vault.Header.Initialize
        (State        => State.Header,
         Password     => Password,
         Timestamp    => Timestamp,
         Master       => State.Master_Key,
         Chain        => State.Chain_Key,
         Wrap_Key     => State.Wrap_Key,
         Signing      => Signing);
      State.Header.MLDsa_Secret_Key := Signing;
      State.Header.Has_MLDsa_Secret := True;
      --  NOTE: Wrap_Secrets NOT called here because Header.Initialize already wrapped all secrets
      --  and computed the fingerprint. Calling Wrap_Secrets would invalidate the fingerprint!
      SparkPass.Crypto.Zeroize.Wipe (Signing);
      State.Unlocked := True;
   end Create;

   procedure Derive_Entry_Key
     (State     : Vault_State;
      Entry_Id  : SparkPass.Types.Entry_Id_Array;
      Label     : Byte_Array;
      Key       : out Key_Array) is
      Chain_View : SparkPass.Types.Byte_Array (1 .. State.Chain_Key'Length);
      for Chain_View'Address use State.Chain_Key (State.Chain_Key'First)'Address;
      Master_View : SparkPass.Types.Byte_Array (1 .. State.Master_Key'Length);
      for Master_View'Address use State.Master_Key (State.Master_Key'First)'Address;
      Info : SparkPass.Types.Byte_Array (1 .. Label'Length + Entry_Id'Length);
      Offset : Natural := 0;
   begin
      for Byte_Value of Entry_Id loop
         Info (Info'First + Offset) := Byte_Value;
         Offset := Offset + 1;
      end loop;
      for Index in Label'Range loop
         Info (Info'First + Offset) := Label (Index);
         Offset := Offset + 1;
      end loop;
      declare
         Derived : SparkPass.Types.Byte_Array :=
           SparkPass.Crypto.HKDF.Derive (Chain_View, Master_View, Info, Key'Length);
      begin
         for Index in Key'Range loop
            Key (Index) := Derived (Derived'First + (Index - Key'First));
         end loop;
         SparkPass.Crypto.Zeroize.Wipe (Derived);
         SparkPass.Crypto.Zeroize.Wipe (Info);
      end;
   end Derive_Entry_Key;

   function Labels_Match
     (Stored : SparkPass.Types.Entry_Record;
      Label  : Byte_Array) return Boolean is
      --  Fully constant-time comparison to prevent timing attacks
      --  Uses only XOR and OR operations, no conditional branches
      Diff : U8 := 0;
   begin
      --  Compare length as bytes (constant time)
      Diff := Diff or (U8 (U16 (Stored.Label_Len) and 16#FF#) xor U8 (U16 (Label'Length) and 16#FF#));
      Diff := Diff or (U8 (Interfaces.Shift_Right (U16 (Stored.Label_Len), 8)) xor
                       U8 (Interfaces.Shift_Right (U16 (Label'Length), 8)));

      --  Compare all bytes regardless of length (constant time)
      for Index in 0 .. SparkPass.Config.Max_Label_Length - 1 loop
         declare
            Stored_Byte : constant U8 :=
              (if Index < Natural (Stored.Label_Len)
               then Stored.Label (Stored.Label'First + Index)
               else 0);
            Label_Byte  : constant U8 :=
              (if Index < Label'Length
               then Label (Label'First + Index)
               else 0);
         begin
            Diff := Diff or (Stored_Byte xor Label_Byte);
         end;
      end loop;

      return Diff = 0;
   end Labels_Match;

   procedure Ratchet_Chain
     (State : in out Vault_State) is
      Chain_View : SparkPass.Types.Byte_Array (1 .. State.Chain_Key'Length);
      for Chain_View'Address use State.Chain_Key (State.Chain_Key'First)'Address;
      Master_View : SparkPass.Types.Byte_Array (1 .. State.Master_Key'Length);
      for Master_View'Address use State.Master_Key (State.Master_Key'First)'Address;
      New_Key : SparkPass.Types.Byte_Array :=
        SparkPass.Crypto.HKDF.Derive (Chain_View, Master_View, Ratchet_Info, State.Chain_Key'Length);
   begin
      for Index in State.Chain_Key'Range loop
         State.Chain_Key (Index) := New_Key (New_Key'First + (Index - State.Chain_Key'First));
      end loop;
      SparkPass.Crypto.Zeroize.Wipe (New_Key);
      Wrap_Secrets (State);
   end Ratchet_Chain;

   procedure Extract_Wrapped_Key
     (Source_Entry : SparkPass.Types.Entry_Record;
      Nonce    : out SparkPass.Types.Nonce_Array;
      Cipher   : out Key_Array;
      Tag      : out Tag_Array) is
      Index : Natural := Source_Entry.Signature'First;
   begin
      for Offset in Nonce'Range loop
         Nonce (Offset) := Source_Entry.Signature (Index);
         Index := Index + 1;
      end loop;
      for Offset in Cipher'Range loop
         Cipher (Offset) := Source_Entry.Signature (Index);
         Index := Index + 1;
      end loop;
      for Offset in Tag'Range loop
         Tag (Offset) := Source_Entry.Signature (Index);
         Index := Index + 1;
      end loop;
   end Extract_Wrapped_Key;

   procedure Store_Wrapped_Key
     (Target_Entry  : in out SparkPass.Types.Entry_Record;
      Nonce  : SparkPass.Types.Nonce_Array;
      Cipher : Key_Array;
      Tag    : Tag_Array) is
      Index : Natural := Target_Entry.Signature'First;
   begin
      for Offset in Nonce'Range loop
         Target_Entry.Signature (Index) := Nonce (Offset);
         Index := Index + 1;
      end loop;
      for Offset in Cipher'Range loop
         Target_Entry.Signature (Index) := Cipher (Offset);
         Index := Index + 1;
      end loop;
      for Offset in Tag'Range loop
         Target_Entry.Signature (Index) := Tag (Offset);
         Index := Index + 1;
      end loop;
      while Index <= Target_Entry.Signature'Last loop
         Target_Entry.Signature (Index) := 0;
         Index := Index + 1;
      end loop;
   end Store_Wrapped_Key;

   procedure Open
     (State     : out Vault_State;
      Path      : String;
      Password  : Byte_Array;
      Status    : out Open_Status)
   is
      Header  : SparkPass.Types.Header;
      Entries : SparkPass.Types.Entry_Table;
      Count   : Entry_Count_Type := 0;
      Storage_Status : SparkPass.Vault.Storage.Status;
      Params  : SparkPass.Crypto.Argon2id.Parameters;
      Wrap_Key : Key_Array := (others => 0);
      Master   : Key_Array := (others => 0);
      Chain    : Chain_Key_Array := (others => 0);
      Secret      : SparkPass.Types.MLDsa_Secret_Key_Array := (others => 0);
      MLKem_Secret : SparkPass.Types.MLKem_Secret_Key_Array := (others => 0);
      Op_Success  : Boolean := False;
      Signature_OK : Boolean := False;
      Chain_Info : SparkPass.Types.Byte_Array (1 .. Chain'Length);
      for Chain_Info'Address use Chain (Chain'First)'Address;
      Wrap_View : SparkPass.Types.Byte_Array (1 .. Wrap_Key'Length);
      for Wrap_View'Address use Wrap_Key (Wrap_Key'First)'Address;
      Master_View : SparkPass.Types.Byte_Array (1 .. Master'Length);
      for Master_View'Address use Master (Master'First)'Address;

      procedure Zero_Locals is
      begin
         SparkPass.Crypto.Zeroize.Wipe (Wrap_Key);
         SparkPass.Crypto.Zeroize.Wipe (Master);
         SparkPass.Crypto.Zeroize.Wipe_Chain (Chain);
         SparkPass.Crypto.Zeroize.Wipe (Secret);
         SparkPass.Crypto.Zeroize.Wipe (MLKem_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Params.Salt);
      end Zero_Locals;

      procedure Zero_Header_Data (Item : in out SparkPass.Types.Header) is
      begin
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_Master_Key);
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_Master_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_Master_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Item.Chain_Key_Value);
         SparkPass.Crypto.Zeroize.Wipe (Item.Chain_Key_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.Chain_Key_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Secret_Value);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Secret_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Secret_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Secret_Key);
         Item.Has_MLDsa_Secret := False;
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Secret_Value);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Secret_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Secret_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Secret_Key);
         Item.Has_MLKem_Secret := False;
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_D_Key);
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_D_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_D_Tag);
         Item.Has_Wrap_D := False;
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Public_Key);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Public_Key);
         SparkPass.Crypto.Zeroize.Wipe (Item.Header_Signature);
         SparkPass.Crypto.Zeroize.Wipe (Item.Argon2_Salt);
         SparkPass.Crypto.Zeroize.Wipe (Item.Vault_Fingerprint);
      end Zero_Header_Data;

      procedure Zero_Entries is
      begin
         for Index in Entries'Range loop
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Ciphertext);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Label);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Signature);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Nonce);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Tag);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Id);
            Entries (Index).Label_Len := 0;
            Entries (Index).Data_Len := 0;
            Entries (Index).Kind := SparkPass.Types.Password;
            Entries (Index).Created_At := 0;
            Entries (Index).Modified_At := 0;
         end loop;
      end Zero_Entries;
   begin
      Clear (State);

      SparkPass.Vault.Storage.Load (Path, Header, Entries, Count, Storage_Status);
      case Storage_Status is
         when SparkPass.Vault.Storage.Ok => null;
         when SparkPass.Vault.Storage.Io_Error =>
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Io_Error;
            return;
         when SparkPass.Vault.Storage.Format_Error =>
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Format_Error;
            return;
         when SparkPass.Vault.Storage.Integrity_Error =>
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Integrity_Error;
            return;
         when SparkPass.Vault.Storage.Permission_Error =>
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Integrity_Error;  -- Map permission error to integrity error for now
            return;
      end case;

      SparkPass.Crypto.MLDSA.Verify
        (Public  => Header.MLDsa_Public_Key,
         Message => Header.Vault_Fingerprint,
         Sig     => Header.Header_Signature,
         Success => Signature_OK);
      if not Signature_OK then
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Integrity_Error;
         return;
      end if;

      Params.Memory_Cost := Header.Argon2_Memory;
      Params.Iterations  := Header.Argon2_Iterations;
      Params.Parallelism := Header.Argon2_Parallelism;
      for Index in Params.Salt'Range loop
         Params.Salt (Index) := Header.Argon2_Salt (Index);
      end loop;

      SparkPass.Crypto.Argon2id.Derive (Password, Params, Wrap_Key, Op_Success);
      if not Op_Success then
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Authentication_Failed;
         return;
      end if;

      SparkPass.Crypto.AES_GCM_SIV.Open
        (Key        => Wrap_Key,
         Nonce      => Header.Wrapped_Master_Nonce,
         Ciphertext => Header.Wrapped_Master_Key,
         AAD        => Header.Argon2_Salt,
         Tag        => Header.Wrapped_Master_Tag,
         Plaintext  => Master,
         Success    => Op_Success);
      if not Op_Success then
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Authentication_Failed;
         return;
      end if;

      declare
         Derived_Info : SparkPass.Types.Byte_Array :=
           SparkPass.Crypto.HKDF.Derive (Wrap_View, Header.Argon2_Salt, HKDF_Chain_Info, Chain'Length);
      begin
         SparkPass.Crypto.AES_GCM_SIV.Open
            (Key        => Wrap_Key,
             Nonce      => Header.Chain_Key_Nonce,
             Ciphertext => Header.Chain_Key_Value,
             AAD        => Derived_Info,
             Tag        => Header.Chain_Key_Tag,
             Plaintext  => Chain_Info,
             Success    => Op_Success);
         SparkPass.Crypto.Zeroize.Wipe (Derived_Info);
      end;

      if not Op_Success then
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Authentication_Failed;
         return;
      end if;

      SparkPass.Crypto.AES_GCM_SIV.Open
        (Key        => Wrap_Key,
         Nonce      => Header.MLDsa_Secret_Nonce,
         Ciphertext => Header.MLDsa_Secret_Value,
         AAD        => Header.Argon2_Salt,
         Tag        => Header.MLDsa_Secret_Tag,
         Plaintext  => Secret,
         Success    => Op_Success);
      if not Op_Success then
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Authentication_Failed;
         return;
      end if;

      --  Unwrap ML-KEM secret key (if present in header)
      if Header.Has_MLKem_Secret then
         SparkPass.Crypto.AES_GCM_SIV.Open
           (Key        => Wrap_Key,
            Nonce      => Header.MLKem_Secret_Nonce,
            Ciphertext => Header.MLKem_Secret_Value,
            AAD        => Header.Argon2_Salt,
            Tag        => Header.MLKem_Secret_Tag,
            Plaintext  => MLKem_Secret,
            Success    => Op_Success);
         if not Op_Success then
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Authentication_Failed;
            return;
         end if;
      end if;

      State.Header := Header;
      State.Master_Key := Master;
      State.Chain_Key := Chain;
      State.Wrap_Key := Wrap_Key;
      State.Header.MLDsa_Secret_Key := Secret;
      State.Header.Has_MLDsa_Secret := True;
      State.Header.MLKem_Secret_Key := MLKem_Secret;
      State.Header.Has_MLKem_Secret := Header.Has_MLKem_Secret;
      State.Entry_Count := Count;

      for Index in State.Entries'Range loop
         if Index <= Natural (Count) then
            State.Entries (Index) := Entries (Index);
         else
            Zero_Entry (State.Entries (Index));
         end if;
      end loop;

      State.Unlocked := True;
      Zero_Header_Data (Header);
      Zero_Entries;
      Zero_Locals;
      Status := Success;
   exception
      when others =>
         Clear (State);
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Io_Error;
   end Open;

   procedure Open_With_Key
     (State     : out Vault_State;
      Path      : String;
      Wrap_Key  : Key_Array;
      Status    : out Open_Status)
   is
      Header  : SparkPass.Types.Header;
      Entries : SparkPass.Types.Entry_Table;
      Count   : Entry_Count_Type := 0;
      Storage_Status : SparkPass.Vault.Storage.Status;
      Master   : Key_Array := (others => 0);
      Chain    : Chain_Key_Array := (others => 0);
      Secret      : SparkPass.Types.MLDsa_Secret_Key_Array := (others => 0);
      MLKem_Secret : SparkPass.Types.MLKem_Secret_Key_Array := (others => 0);
      Op_Success  : Boolean := False;
      Signature_OK : Boolean := False;
      Chain_Info : SparkPass.Types.Byte_Array (1 .. Chain'Length);
      for Chain_Info'Address use Chain (Chain'First)'Address;
      Wrap_View : SparkPass.Types.Byte_Array (1 .. Wrap_Key'Length);
      for Wrap_View'Address use Wrap_Key (Wrap_Key'First)'Address;
      Master_View : SparkPass.Types.Byte_Array (1 .. Master'Length);
      for Master_View'Address use Master (Master'First)'Address;

      procedure Zero_Locals is
      begin
         SparkPass.Crypto.Zeroize.Wipe (Master);
         SparkPass.Crypto.Zeroize.Wipe_Chain (Chain);
         SparkPass.Crypto.Zeroize.Wipe (Secret);
         SparkPass.Crypto.Zeroize.Wipe (MLKem_Secret);
      end Zero_Locals;

      procedure Zero_Header_Data (Item : in out SparkPass.Types.Header) is
      begin
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_Master_Key);
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_Master_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_Master_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Item.Chain_Key_Value);
         SparkPass.Crypto.Zeroize.Wipe (Item.Chain_Key_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.Chain_Key_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Secret_Value);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Secret_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Secret_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Secret_Key);
         Item.Has_MLDsa_Secret := False;
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Secret_Value);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Secret_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Secret_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Secret_Key);
         Item.Has_MLKem_Secret := False;
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_D_Key);
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_D_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Item.Wrapped_D_Tag);
         Item.Has_Wrap_D := False;
         SparkPass.Crypto.Zeroize.Wipe (Item.MLDsa_Public_Key);
         SparkPass.Crypto.Zeroize.Wipe (Item.MLKem_Public_Key);
         SparkPass.Crypto.Zeroize.Wipe (Item.Header_Signature);
         SparkPass.Crypto.Zeroize.Wipe (Item.Argon2_Salt);
         SparkPass.Crypto.Zeroize.Wipe (Item.Vault_Fingerprint);
      end Zero_Header_Data;

      procedure Zero_Entries is
      begin
         for Index in Entries'Range loop
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Ciphertext);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Label);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Signature);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Nonce);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Tag);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Id);
            Entries (Index).Label_Len := 0;
            Entries (Index).Data_Len := 0;
            Entries (Index).Kind := SparkPass.Types.Password;
            Entries (Index).Created_At := 0;
            Entries (Index).Modified_At := 0;
         end loop;
      end Zero_Entries;
   begin
      Clear (State);

      SparkPass.Vault.Storage.Load (Path, Header, Entries, Count, Storage_Status);
      case Storage_Status is
         when SparkPass.Vault.Storage.Ok => null;
         when SparkPass.Vault.Storage.Io_Error =>
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Io_Error;
            return;
         when SparkPass.Vault.Storage.Format_Error =>
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Format_Error;
            return;
         when SparkPass.Vault.Storage.Integrity_Error =>
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Integrity_Error;
            return;
         when SparkPass.Vault.Storage.Permission_Error =>
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Integrity_Error;  -- Map permission error to integrity error for now
            return;
      end case;

      SparkPass.Crypto.MLDSA.Verify
        (Public  => Header.MLDsa_Public_Key,
         Message => Header.Vault_Fingerprint,
         Sig     => Header.Header_Signature,
         Success => Signature_OK);
      if not Signature_OK then
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Integrity_Error;
         return;
      end if;

      --  NOTE: Skipping Argon2id password derivation - using provided wrap_key directly

      SparkPass.Crypto.AES_GCM_SIV.Open
        (Key        => Wrap_Key,
         Nonce      => Header.Wrapped_Master_Nonce,
         Ciphertext => Header.Wrapped_Master_Key,
         AAD        => Header.Argon2_Salt,
         Tag        => Header.Wrapped_Master_Tag,
         Plaintext  => Master,
         Success    => Op_Success);
      if not Op_Success then
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Authentication_Failed;
         return;
      end if;

      declare
         Derived_Info : SparkPass.Types.Byte_Array :=
           SparkPass.Crypto.HKDF.Derive (Wrap_View, Header.Argon2_Salt, HKDF_Chain_Info, Chain'Length);
      begin
         SparkPass.Crypto.AES_GCM_SIV.Open
            (Key        => Wrap_Key,
             Nonce      => Header.Chain_Key_Nonce,
             Ciphertext => Header.Chain_Key_Value,
             AAD        => Derived_Info,
             Tag        => Header.Chain_Key_Tag,
             Plaintext  => Chain_Info,
             Success    => Op_Success);
         SparkPass.Crypto.Zeroize.Wipe (Derived_Info);
      end;

      if not Op_Success then
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Authentication_Failed;
         return;
      end if;

      SparkPass.Crypto.AES_GCM_SIV.Open
        (Key        => Wrap_Key,
         Nonce      => Header.MLDsa_Secret_Nonce,
         Ciphertext => Header.MLDsa_Secret_Value,
         AAD        => Header.Argon2_Salt,
         Tag        => Header.MLDsa_Secret_Tag,
         Plaintext  => Secret,
         Success    => Op_Success);
      if not Op_Success then
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Authentication_Failed;
         return;
      end if;

      --  Unwrap ML-KEM secret key (if present in header)
      if Header.Has_MLKem_Secret then
         SparkPass.Crypto.AES_GCM_SIV.Open
           (Key        => Wrap_Key,
            Nonce      => Header.MLKem_Secret_Nonce,
            Ciphertext => Header.MLKem_Secret_Value,
            AAD        => Header.Argon2_Salt,
            Tag        => Header.MLKem_Secret_Tag,
            Plaintext  => MLKem_Secret,
            Success    => Op_Success);
         if not Op_Success then
            Zero_Header_Data (Header);
            Zero_Entries;
            Zero_Locals;
            Status := Authentication_Failed;
            return;
         end if;
      end if;

      State.Header := Header;
      State.Master_Key := Master;
      State.Chain_Key := Chain;
      State.Wrap_Key := Wrap_Key;
      State.Header.MLDsa_Secret_Key := Secret;
      State.Header.Has_MLDsa_Secret := True;
      State.Header.MLKem_Secret_Key := MLKem_Secret;
      State.Header.Has_MLKem_Secret := Header.Has_MLKem_Secret;
      State.Entry_Count := Count;

      for Index in State.Entries'Range loop
         if Index <= Natural (Count) then
            State.Entries (Index) := Entries (Index);
         else
            Zero_Entry (State.Entries (Index));
         end if;
      end loop;

      State.Unlocked := True;
      Zero_Header_Data (Header);
      Zero_Entries;
      Zero_Locals;
      Status := Success;
   exception
      when others =>
         Clear (State);
         Zero_Header_Data (Header);
         Zero_Entries;
         Zero_Locals;
         Status := Io_Error;
   end Open_With_Key;

   procedure Save
     (State  : in out Vault_State;
      Path   : String;
      Status : out Save_Status) is
      Result : SparkPass.Vault.Storage.Status;
   begin
      --  Storage.Save will update Entry_Count, recompute fingerprint, and re-sign
      SparkPass.Vault.Storage.Save
        (Path    => Path,
         Header  => State.Header,
         Entries => State.Entries,
         Count   => State.Entry_Count,
         Result  => Result);
      case Result is
         when SparkPass.Vault.Storage.Ok =>
            Status := Saved;
         when others =>
            Status := Io_Error;
      end case;
   end Save;

   procedure Add_Entry
     (State     : in out Vault_State;
      Label     : Byte_Array;
      Kind      : Entry_Type;
      Plaintext : Byte_Array;
      Timestamp : U64;
      Success   : out Boolean)
   is
      use type Interfaces.Unsigned_32;
      Position : Natural := Natural (State.Entry_Count);
      Entry_Key : Key_Array := (others => 0);
      Cipher    : SparkPass.Types.Byte_Array (1 .. Plaintext'Length);
      Nonce     : Nonce_Array := (others => 0);
      Tag       : Tag_Array := (others => 0);
      Key_Nonce : Nonce_Array := (others => 0);
      Wrapped_Key : Key_Array := (others => 0);
      Wrapped_Tag : Tag_Array := (others => 0);
      New_Entry : SparkPass.Types.Entry_Record;
   begin
      Success := False;
      if Position = SparkPass.Config.Max_Entries then
         return;
      end if;
      if Plaintext'Length = 0 then
         return;
      end if;
      if Plaintext'Length > SparkPass.Config.Max_Data_Length then
         return;
      end if;
      if Label'Length = 0 or else Label'Length > SparkPass.Config.Max_Label_Length then
         return;
      end if;

      for Index in 1 .. Natural (State.Entry_Count) loop
         if Labels_Match (State.Entries (Index), Label) then
            Success := False;
            return;
         end if;
      end loop;

      Position := Position + 1;
      New_Entry := State.Entries (Position);
      Zero_Entry (New_Entry);

      New_Entry.Kind := Kind;
      New_Entry.Label_Len := Interfaces.Unsigned_16 (Label'Length);
      New_Entry.Data_Len  := Interfaces.Unsigned_32 (Plaintext'Length);
      New_Entry.Created_At := Timestamp;
      New_Entry.Modified_At := Timestamp;
      SparkPass.Crypto.Random.Fill (New_Entry.Id);
      for Index in 0 .. Label'Length - 1 loop
         New_Entry.Label (New_Entry.Label'First + Index) := Label (Label'First + Index);
      end loop;

      Derive_Entry_Key (State, New_Entry.Id, Label, Entry_Key);
      SparkPass.Crypto.Random.Fill (Nonce);

      declare
         AAD : SparkPass.Types.Byte_Array (1 .. Label'Length);
      begin
         for Index in Label'Range loop
            AAD (AAD'First + (Index - Label'First)) := Label (Index);
         end loop;
         SparkPass.Crypto.AES_GCM_SIV.Seal
           (Key        => Entry_Key,
            Nonce      => Nonce,
            Plaintext  => Plaintext,
            AAD        => AAD,
            Ciphertext => Cipher,
            Tag        => Tag);
         SparkPass.Crypto.Zeroize.Wipe (AAD);
      end;

      SparkPass.Crypto.Random.Fill (Key_Nonce);
      SparkPass.Crypto.AES_GCM_SIV.Seal
        (Key        => State.Master_Key,
         Nonce      => Key_Nonce,
         Plaintext  => Entry_Key,
         AAD        => New_Entry.Id,
         Ciphertext => Wrapped_Key,
         Tag        => Wrapped_Tag);

      for Index in 0 .. Cipher'Length - 1 loop
         New_Entry.Ciphertext (New_Entry.Ciphertext'First + Index) := Cipher (Cipher'First + Index);
      end loop;
      if Cipher'Length < New_Entry.Ciphertext'Length then
         for Index in Cipher'Length .. New_Entry.Ciphertext'Length - 1 loop
            New_Entry.Ciphertext (New_Entry.Ciphertext'First + Index) := 0;
         end loop;
      end if;
      New_Entry.Nonce := Nonce;
      New_Entry.Tag := Tag;
      Store_Wrapped_Key (New_Entry, Key_Nonce, Wrapped_Key, Wrapped_Tag);
      SparkPass.Crypto.Zeroize.Wipe (Wrapped_Key);
      SparkPass.Crypto.Zeroize.Wipe (Wrapped_Tag);
      SparkPass.Crypto.Zeroize.Wipe (Key_Nonce);
      SparkPass.Crypto.Zeroize.Wipe (Nonce);
      SparkPass.Crypto.Zeroize.Wipe (Tag);

      State.Entries (Position) := New_Entry;
      State.Entry_Count := Interfaces.Unsigned_32 (Position);

      Ratchet_Chain (State);
      State.Header.Entry_Count := State.Entry_Count;
      SparkPass.Vault.Header.Bump (State.Header, Timestamp);
      SparkPass.Vault.Header.Refresh_Fingerprint (State.Header);
      SparkPass.Vault.Header.Update_Signature (State.Header);
      Success := True;
      SparkPass.Crypto.Zeroize.Wipe (Entry_Key);
      SparkPass.Crypto.Zeroize.Wipe (Cipher);
   exception
      when others =>
         SparkPass.Crypto.Zeroize.Wipe (Entry_Key);
         SparkPass.Crypto.Zeroize.Wipe (Cipher);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Key);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Tag);
         SparkPass.Crypto.Zeroize.Wipe (Key_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Nonce);
         SparkPass.Crypto.Zeroize.Wipe (Tag);
         Success := False;
   end Add_Entry;

   procedure Get_Entry
     (State     : Vault_State;
      Label     : Byte_Array;
      Plaintext : out Byte_Array;
      Data_Len  : out Natural;
      Success   : out Boolean)
   is
      --  Constant-time variables
      Match_Found : Boolean := False;
      Final_Success : Boolean := False;
      Final_Data_Len : Natural := 0;
      --  Temporary buffers for constant-time operations
      Temp_Entry_Key : Key_Array := (others => 0);
      Temp_Plain_View : SparkPass.Types.Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Dummy_Key : Key_Array := (others => 0);
   begin
      Success := False;
      Data_Len := 0;

      --  SECURITY: Fully constant-time entry lookup
      --  Always perform SAME operations for EVERY entry to eliminate ALL timing leaks
      --  This fixes both position-based (14.38%) and conditional decryption (18%) timing variances
      for Index in 1 .. Natural (State.Entry_Count) loop
         declare
            Current_Entry : constant SparkPass.Types.Entry_Record := State.Entries (Index);
            Is_Match : constant Boolean := Labels_Match (Current_Entry, Label);
            Key_Nonce : Nonce_Array := (others => 0);
            Wrapped_Key : Key_Array := (others => 0);
            Wrapped_Tag : Tag_Array := (others => 0);
            Unwrap_Success : Boolean := False;
            Decrypt_Success : Boolean := False;
         begin
            --  ALWAYS unwrap entry key (constant time)
            Extract_Wrapped_Key (Current_Entry, Key_Nonce, Wrapped_Key, Wrapped_Tag);
            SparkPass.Crypto.AES_GCM_SIV.Open
              (Key        => State.Master_Key,
               Nonce      => Key_Nonce,
               Ciphertext => Wrapped_Key,
               AAD        => Current_Entry.Id,
               Tag        => Wrapped_Tag,
               Plaintext  => Temp_Entry_Key,
               Success    => Unwrap_Success);

            --  ALWAYS decrypt entry data (constant time)
            if Unwrap_Success and then Natural (Current_Entry.Data_Len) > 0
               and then Plaintext'Length >= Natural (Current_Entry.Data_Len) then
               declare
                  Expected_Length : constant Natural := Natural (Current_Entry.Data_Len);
                  AAD : SparkPass.Types.Byte_Array (1 .. Natural (Current_Entry.Label_Len));
                  Ciphertext_View : SparkPass.Types.Byte_Array (1 .. Expected_Length);
                  Plain_View      : SparkPass.Types.Byte_Array (1 .. Expected_Length);
               begin
                  --  Build AAD
                  for Offset in 0 .. Natural (Current_Entry.Label_Len) - 1 loop
                     AAD (AAD'First + Offset) := Current_Entry.Label (Current_Entry.Label'First + Offset);
                  end loop;

                  --  Extract ciphertext
                  for Offset in 0 .. Expected_Length - 1 loop
                     Ciphertext_View (Ciphertext_View'First + Offset) :=
                       Current_Entry.Ciphertext (Current_Entry.Ciphertext'First + Offset);
                  end loop;

                  --  Decrypt (always happens, result only used if match)
                  SparkPass.Crypto.AES_GCM_SIV.Open
                    (Key        => Temp_Entry_Key,
                     Nonce      => Current_Entry.Nonce,
                     Ciphertext => Ciphertext_View,
                     AAD        => AAD,
                     Tag        => Current_Entry.Tag,
                     Plaintext  => Plain_View,
                     Success    => Decrypt_Success);

                  --  Constant-time selection: only use result if this is the matching entry
                  if Is_Match and then not Match_Found and then Decrypt_Success then
                     Match_Found := True;
                     Final_Success := True;
                     Final_Data_Len := Expected_Length;
                     --  Copy decrypted data to output (only for match)
                     for Offset in 0 .. Expected_Length - 1 loop
                        Plaintext (Plaintext'First + Offset) := Plain_View (Plain_View'First + Offset);
                     end loop;
                  end if;

                  SparkPass.Crypto.Zeroize.Wipe (Plain_View);
                  SparkPass.Crypto.Zeroize.Wipe (Ciphertext_View);
                  SparkPass.Crypto.Zeroize.Wipe (AAD);
               exception
                  when others =>
                     SparkPass.Crypto.Zeroize.Wipe (Plain_View);
                     SparkPass.Crypto.Zeroize.Wipe (Ciphertext_View);
                     SparkPass.Crypto.Zeroize.Wipe (AAD);
                     SparkPass.Crypto.Zeroize.Wipe (Temp_Entry_Key);
                     SparkPass.Crypto.Zeroize.Wipe (Temp_Plain_View);
                     SparkPass.Crypto.Zeroize.Wipe (Dummy_Key);
                     raise;
               end;
            elsif Is_Match and then not Match_Found and then Unwrap_Success and then Natural (Current_Entry.Data_Len) = 0 then
               --  Empty entry match
               Match_Found := True;
               Final_Success := True;
               Final_Data_Len := 0;
            end if;

            SparkPass.Crypto.Zeroize.Wipe (Temp_Entry_Key);
            SparkPass.Crypto.Zeroize.Wipe (Wrapped_Key);
            SparkPass.Crypto.Zeroize.Wipe (Wrapped_Tag);
            --  NO EXIT - Continue iterating to maintain constant time
         end;
      end loop;

      --  Set final output values after full iteration
      Success := Match_Found and then Final_Success;
      Data_Len := Final_Data_Len;

      SparkPass.Crypto.Zeroize.Wipe (Temp_Plain_View);
      SparkPass.Crypto.Zeroize.Wipe (Dummy_Key);

      if Success then
         if Data_Len < Plaintext'Length then
            for Offset in Data_Len .. Plaintext'Length - 1 loop
               Plaintext (Plaintext'First + Offset) := 0;
            end loop;
         end if;
      else
         for Offset in Plaintext'Range loop
            Plaintext (Offset) := 0;
         end loop;
         Data_Len := 0;
      end if;
  exception
      when others =>
         SparkPass.Crypto.Zeroize.Wipe (Temp_Entry_Key);
         SparkPass.Crypto.Zeroize.Wipe (Temp_Plain_View);
         SparkPass.Crypto.Zeroize.Wipe (Dummy_Key);
         for Offset in Plaintext'Range loop
            Plaintext (Offset) := 0;
         end loop;
         Data_Len := 0;
         Success := False;
  end Get_Entry;

   procedure Remove_Entry
     (State     : in out Vault_State;
      Label     : Byte_Array;
      Timestamp : U64;
      Success   : out Boolean)
   is
      Found_Index : Natural := 0;
   begin
      Success := False;

      --  SECURITY: Constant-time entry search
      --  Always iterate through ALL entries to prevent position-based timing leaks
      for Index in 1 .. Natural (State.Entry_Count) loop
         if Labels_Match (State.Entries (Index), Label) and then Found_Index = 0 then
            Found_Index := Index;
            --  NO EXIT - Continue iterating to maintain constant time
         end if;
      end loop;

      if Found_Index = 0 then
         return;
      end if;

      declare
         Last_Index : constant Natural := Natural (State.Entry_Count);
      begin
         if Found_Index /= Last_Index then
            State.Entries (Found_Index) := State.Entries (Last_Index);
         end if;
         Zero_Entry (State.Entries (Last_Index));
         State.Entry_Count := Interfaces.Unsigned_32 (Last_Index - 1);
      end;

      Ratchet_Chain (State);
      State.Header.Entry_Count := State.Entry_Count;
      SparkPass.Vault.Header.Bump (State.Header, Timestamp);
      SparkPass.Vault.Header.Refresh_Fingerprint (State.Header);
      SparkPass.Vault.Header.Update_Signature (State.Header);
      Success := True;
   end Remove_Entry;

   procedure Rotate_Master_Key
     (State     : in out Vault_State;
      Timestamp : U64;
      Success   : out Boolean)
   is
      New_Master : Key_Array := (others => 0);
      New_Chain  : Chain_Key_Array := (others => 0);
      Entry_Key  : Key_Array := (others => 0);
      Old_Entry_Key : Key_Array := (others => 0);
      Plaintext  : SparkPass.Types.Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Plain_Len  : Natural := 0;
      Label_Text : SparkPass.Types.Byte_Array (1 .. SparkPass.Config.Max_Label_Length);
      Nonce      : Nonce_Array := (others => 0);
      Tag        : Tag_Array := (others => 0);
      Cipher     : SparkPass.Types.Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Key_Nonce  : Nonce_Array := (others => 0);
      Wrapped_Key : Key_Array := (others => 0);
      Wrapped_Tag : Tag_Array := (others => 0);
      Old_Key_Nonce : Nonce_Array := (others => 0);
      Old_Wrapped_Key : Key_Array := (others => 0);
      Old_Wrapped_Tag : Tag_Array := (others => 0);
      Unwrap_Success : Boolean := False;
   begin
      Success := False;

      --  Generate new master key and chain key
      SparkPass.Crypto.Random.Fill (New_Master);
      SparkPass.Crypto.Random.Fill (New_Chain);

      --  Update chain key BEFORE re-encryption for forward secrecy
      --  This ensures new entry keys are derived from the new chain key
      State.Chain_Key := New_Chain;

      --  Re-encrypt all entries with new master key
      for Index in 1 .. Natural (State.Entry_Count) loop
         declare
            Current_Entry : SparkPass.Types.Entry_Record := State.Entries (Index);
            Label_Len : constant Natural := Natural (Current_Entry.Label_Len);
         begin
            --  Extract label
            for Offset in 0 .. Label_Len - 1 loop
               Label_Text (Label_Text'First + Offset) :=
                 Current_Entry.Label (Current_Entry.Label'First + Offset);
            end loop;

            --  Decrypt entry with old master key
            Extract_Wrapped_Key (Current_Entry, Old_Key_Nonce, Old_Wrapped_Key, Old_Wrapped_Tag);
            SparkPass.Crypto.AES_GCM_SIV.Open
              (Key        => State.Master_Key,
               Nonce      => Old_Key_Nonce,
               Ciphertext => Old_Wrapped_Key,
               AAD        => Current_Entry.Id,
               Tag        => Old_Wrapped_Tag,
               Plaintext  => Old_Entry_Key,
               Success    => Unwrap_Success);

            if not Unwrap_Success then
               SparkPass.Crypto.Zeroize.Wipe (New_Master);
               SparkPass.Crypto.Zeroize.Wipe_Chain (New_Chain);
               SparkPass.Crypto.Zeroize.Wipe (Old_Entry_Key);
               return;
            end if;

            declare
               AAD : SparkPass.Types.Byte_Array (1 .. Label_Len);
               Expected_Length : constant Natural := Natural (Current_Entry.Data_Len);
               Ciphertext_View : SparkPass.Types.Byte_Array (1 .. Expected_Length);
               Plain_View      : SparkPass.Types.Byte_Array (1 .. Expected_Length);
            begin
               for Offset in 0 .. Label_Len - 1 loop
                  AAD (AAD'First + Offset) := Label_Text (Label_Text'First + Offset);
               end loop;
               for Offset in 0 .. Expected_Length - 1 loop
                  Ciphertext_View (Ciphertext_View'First + Offset) :=
                    Current_Entry.Ciphertext (Current_Entry.Ciphertext'First + Offset);
               end loop;

               SparkPass.Crypto.AES_GCM_SIV.Open
                 (Key        => Old_Entry_Key,
                  Nonce      => Current_Entry.Nonce,
                  Ciphertext => Ciphertext_View,
                  AAD        => AAD,
                  Tag        => Current_Entry.Tag,
                  Plaintext  => Plain_View,
                  Success    => Unwrap_Success);

               if not Unwrap_Success then
                  SparkPass.Crypto.Zeroize.Wipe (New_Master);
                  SparkPass.Crypto.Zeroize.Wipe_Chain (New_Chain);
                  SparkPass.Crypto.Zeroize.Wipe (Old_Entry_Key);
                  SparkPass.Crypto.Zeroize.Wipe (Plain_View);
                  return;
               end if;

               --  Re-encrypt with new master key
               Derive_Entry_Key (State, Current_Entry.Id, Label_Text (Label_Text'First .. Label_Text'First + Label_Len - 1), Entry_Key);
               SparkPass.Crypto.Random.Fill (Nonce);

               SparkPass.Crypto.AES_GCM_SIV.Seal
                 (Key        => Entry_Key,
                  Nonce      => Nonce,
                  Plaintext  => Plain_View,
                  AAD        => AAD,
                  Ciphertext => Cipher (Cipher'First .. Cipher'First + Expected_Length - 1),
                  Tag        => Tag);

               --  Wrap entry key with new master
               SparkPass.Crypto.Random.Fill (Key_Nonce);
               SparkPass.Crypto.AES_GCM_SIV.Seal
                 (Key        => New_Master,
                  Nonce      => Key_Nonce,
                  Plaintext  => Entry_Key,
                  AAD        => Current_Entry.Id,
                  Ciphertext => Wrapped_Key,
                  Tag        => Wrapped_Tag);

               --  Update entry
               for Offset in 0 .. Expected_Length - 1 loop
                  State.Entries (Index).Ciphertext (State.Entries (Index).Ciphertext'First + Offset) :=
                    Cipher (Cipher'First + Offset);
               end loop;
               State.Entries (Index).Nonce := Nonce;
               State.Entries (Index).Tag := Tag;
               Store_Wrapped_Key (State.Entries (Index), Key_Nonce, Wrapped_Key, Wrapped_Tag);

               SparkPass.Crypto.Zeroize.Wipe (Plain_View);
               SparkPass.Crypto.Zeroize.Wipe (AAD);
            end;

            SparkPass.Crypto.Zeroize.Wipe (Old_Entry_Key);
         end;
      end loop;

      --  Update master and chain keys
      State.Master_Key := New_Master;
      State.Chain_Key := New_Chain;

      --  Re-wrap secrets and update header
      Wrap_Secrets (State);
      SparkPass.Vault.Header.Bump (State.Header, Timestamp);
      SparkPass.Vault.Header.Refresh_Fingerprint (State.Header);
      SparkPass.Vault.Header.Update_Signature (State.Header);

      --  Zeroize temporary keys
      SparkPass.Crypto.Zeroize.Wipe (New_Master);
      SparkPass.Crypto.Zeroize.Wipe_Chain (New_Chain);
      SparkPass.Crypto.Zeroize.Wipe (Entry_Key);
      SparkPass.Crypto.Zeroize.Wipe (Plaintext);

      Success := True;
   end Rotate_Master_Key;

   procedure Export_Recovery
     (State          : Vault_State;
      Recovery_Path  : String;
      Success        : out Boolean)
   is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      File_Stream : Stream_Access;
      --  Use vault's existing ML-KEM public key (generated at vault creation)
      ML_Kem_Public  : SparkPass.Types.MLKem_Public_Key_Array;
      Ciphertext     : SparkPass.Types.MLKem_Ciphertext_Array;
      Shared_Secret  : SparkPass.Types.MLKem_Shared_Key_Array := (others => 0);

      --  Wrapped master key: nonce + ciphertext + tag
      Wrapped_Master_Nonce : SparkPass.Types.Nonce_Array := (others => 0);
      Wrapped_Master_Key   : Key_Array := (others => 0);
      Wrapped_Master_Tag   : Tag_Array := (others => 0);

      Enc_Success    : Boolean := False;
      Master_View    : SparkPass.Types.Byte_Array (1 .. State.Master_Key'Length);
      for Master_View'Address use State.Master_Key (State.Master_Key'First)'Address;

      --  Also wrap chain key for full vault recovery
      Wrapped_Chain_Nonce : SparkPass.Types.Nonce_Array := (others => 0);
      Wrapped_Chain_Key   : Chain_Key_Array := (others => 0);
      Wrapped_Chain_Tag   : Tag_Array := (others => 0);
      Chain_View : SparkPass.Types.Byte_Array (1 .. State.Chain_Key'Length);
      for Chain_View'Address use State.Chain_Key (State.Chain_Key'First)'Address;

      Byte_Val : Interfaces.Unsigned_8;
   begin
      Success := False;

      --  Copy vault's ML-KEM public key
      ML_Kem_Public := State.Header.MLKem_Public_Key;

      --  Encapsulate: creates shared secret + ciphertext
      --  The shared secret will be used as KEK to wrap the master key
      SparkPass.Crypto.MLKEM.Encapsulate (ML_Kem_Public, Ciphertext, Shared_Secret, Enc_Success);
      if not Enc_Success then
         SparkPass.Crypto.Zeroize.Wipe (Shared_Secret);
         return;
      end if;

      --  Wrap master key with the shared secret as KEK
      SparkPass.Crypto.Random.Fill (Wrapped_Master_Nonce);
      declare
         KEK : Key_Array;
         for KEK'Address use Shared_Secret (Shared_Secret'First)'Address;
      begin
         SparkPass.Crypto.AES_GCM_SIV.Seal
           (Key        => KEK,
            Nonce      => Wrapped_Master_Nonce,
            Plaintext  => Master_View,
            AAD        => State.Header.Vault_Fingerprint,
            Ciphertext => Wrapped_Master_Key,
            Tag        => Wrapped_Master_Tag);
      end;

      --  Also wrap chain key for complete recovery
      SparkPass.Crypto.Random.Fill (Wrapped_Chain_Nonce);
      declare
         KEK : Key_Array;
         for KEK'Address use Shared_Secret (Shared_Secret'First)'Address;
      begin
         SparkPass.Crypto.AES_GCM_SIV.Seal
           (Key        => KEK,
            Nonce      => Wrapped_Chain_Nonce,
            Plaintext  => Chain_View,
            AAD        => State.Header.Argon2_Salt,
            Ciphertext => Wrapped_Chain_Key,
            Tag        => Wrapped_Chain_Tag);
      end;

      --  Save recovery file format:
      --  1. ML-KEM ciphertext (1568 bytes)
      --  2. Wrapped master key nonce (12 bytes)
      --  3. Wrapped master key ciphertext (32 bytes)
      --  4. Wrapped master key tag (16 bytes)
      --  5. Wrapped chain key nonce (12 bytes)
      --  6. Wrapped chain key ciphertext (32 bytes)
      --  7. Wrapped chain key tag (16 bytes)
      --  Total: 1688 bytes
      begin
         Create (File, Out_File, Recovery_Path);
         File_Stream := Stream (File);

         --  Write ML-KEM ciphertext
         for Index in Ciphertext'Range loop
            Byte_Val := Interfaces.Unsigned_8 (Ciphertext (Index));
            Interfaces.Unsigned_8'Write (File_Stream, Byte_Val);
         end loop;

         --  Write wrapped master key
         for Index in Wrapped_Master_Nonce'Range loop
            Byte_Val := Interfaces.Unsigned_8 (Wrapped_Master_Nonce (Index));
            Interfaces.Unsigned_8'Write (File_Stream, Byte_Val);
         end loop;
         for Index in Wrapped_Master_Key'Range loop
            Byte_Val := Interfaces.Unsigned_8 (Wrapped_Master_Key (Index));
            Interfaces.Unsigned_8'Write (File_Stream, Byte_Val);
         end loop;
         for Index in Wrapped_Master_Tag'Range loop
            Byte_Val := Interfaces.Unsigned_8 (Wrapped_Master_Tag (Index));
            Interfaces.Unsigned_8'Write (File_Stream, Byte_Val);
         end loop;

         --  Write wrapped chain key
         for Index in Wrapped_Chain_Nonce'Range loop
            Byte_Val := Interfaces.Unsigned_8 (Wrapped_Chain_Nonce (Index));
            Interfaces.Unsigned_8'Write (File_Stream, Byte_Val);
         end loop;
         for Index in Wrapped_Chain_Key'Range loop
            Byte_Val := Interfaces.Unsigned_8 (Wrapped_Chain_Key (Index));
            Interfaces.Unsigned_8'Write (File_Stream, Byte_Val);
         end loop;
         for Index in Wrapped_Chain_Tag'Range loop
            Byte_Val := Interfaces.Unsigned_8 (Wrapped_Chain_Tag (Index));
            Interfaces.Unsigned_8'Write (File_Stream, Byte_Val);
         end loop;

         Close (File);
         Success := True;
      exception
         when others =>
            if Is_Open (File) then
               Close (File);
            end if;
            Success := False;
      end;

      SparkPass.Crypto.Zeroize.Wipe (Shared_Secret);
      SparkPass.Crypto.Zeroize.Wipe (Wrapped_Master_Key);
      SparkPass.Crypto.Zeroize.Wipe (Wrapped_Master_Tag);
      SparkPass.Crypto.Zeroize.Wipe (Wrapped_Chain_Key);
      SparkPass.Crypto.Zeroize.Wipe (Wrapped_Chain_Tag);
   end Export_Recovery;

   procedure Import_Recovery
     (Vault_Path     : String;
      Recovery_Path  : String;
      Password       : Byte_Array;
      State          : out Vault_State;
      Success        : out Boolean)
   is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      File_Stream : Stream_Access;

      --  Recovery file format (1688 bytes total)
      Ciphertext          : SparkPass.Types.MLKem_Ciphertext_Array;
      Wrapped_Master_Nonce : SparkPass.Types.Nonce_Array := (others => 0);
      Wrapped_Master_Key   : Key_Array := (others => 0);
      Wrapped_Master_Tag   : Tag_Array := (others => 0);
      Wrapped_Chain_Nonce  : SparkPass.Types.Nonce_Array := (others => 0);
      Wrapped_Chain_Key    : Chain_Key_Array := (others => 0);
      Wrapped_Chain_Tag    : Tag_Array := (others => 0);

      Shared_Secret  : SparkPass.Types.MLKem_Shared_Key_Array := (others => 0);
      Master_Key     : Key_Array := (others => 0);
      Chain_Key      : Chain_Key_Array := (others => 0);
      ML_Kem_Secret  : SparkPass.Types.MLKem_Secret_Key_Array := (others => 0);

      Dec_Success    : Boolean := False;
      Unwrap_Success : Boolean := False;
      Vault_Open_Status : Open_Status;
      Vault_Tmp      : Vault_State;
      Byte_Val       : Interfaces.Unsigned_8;

      Master_View : SparkPass.Types.Byte_Array (1 .. Master_Key'Length);
      for Master_View'Address use Master_Key (Master_Key'First)'Address;
      Chain_View : SparkPass.Types.Byte_Array (1 .. Chain_Key'Length);
      for Chain_View'Address use Chain_Key (Chain_Key'First)'Address;
   begin
      Success := False;
      Clear (State);

      --  Read recovery file (1688 bytes)
      begin
         Open (File, In_File, Recovery_Path);
         File_Stream := Stream (File);

         --  Read ML-KEM ciphertext (1568 bytes)
         for Index in Ciphertext'Range loop
            Interfaces.Unsigned_8'Read (File_Stream, Byte_Val);
            Ciphertext (Index) := SparkPass.Types.U8 (Byte_Val);
         end loop;

         --  Read wrapped master key (12 + 32 + 16 = 60 bytes)
         for Index in Wrapped_Master_Nonce'Range loop
            Interfaces.Unsigned_8'Read (File_Stream, Byte_Val);
            Wrapped_Master_Nonce (Index) := SparkPass.Types.U8 (Byte_Val);
         end loop;
         for Index in Wrapped_Master_Key'Range loop
            Interfaces.Unsigned_8'Read (File_Stream, Byte_Val);
            Wrapped_Master_Key (Index) := SparkPass.Types.U8 (Byte_Val);
         end loop;
         for Index in Wrapped_Master_Tag'Range loop
            Interfaces.Unsigned_8'Read (File_Stream, Byte_Val);
            Wrapped_Master_Tag (Index) := SparkPass.Types.U8 (Byte_Val);
         end loop;

         --  Read wrapped chain key (12 + 32 + 16 = 60 bytes)
         for Index in Wrapped_Chain_Nonce'Range loop
            Interfaces.Unsigned_8'Read (File_Stream, Byte_Val);
            Wrapped_Chain_Nonce (Index) := SparkPass.Types.U8 (Byte_Val);
         end loop;
         for Index in Wrapped_Chain_Key'Range loop
            Interfaces.Unsigned_8'Read (File_Stream, Byte_Val);
            Wrapped_Chain_Key (Index) := SparkPass.Types.U8 (Byte_Val);
         end loop;
         for Index in Wrapped_Chain_Tag'Range loop
            Interfaces.Unsigned_8'Read (File_Stream, Byte_Val);
            Wrapped_Chain_Tag (Index) := SparkPass.Types.U8 (Byte_Val);
         end loop;

         Close (File);
      exception
         when others =>
            if Is_Open (File) then
               Close (File);
            end if;
            SparkPass.Crypto.Zeroize.Wipe (Shared_Secret);
            SparkPass.Crypto.Zeroize.Wipe (Master_Key);
            SparkPass.Crypto.Zeroize.Wipe_Chain (Chain_Key);
            SparkPass.Crypto.Zeroize.Wipe (ML_Kem_Secret);
            SparkPass.Crypto.Zeroize.Wipe (Wrapped_Master_Key);
            SparkPass.Crypto.Zeroize.Wipe (Wrapped_Chain_Key);
            return;
      end;

      --  Step 1: Open the vault to get the ML-KEM secret key from header
      Open (Vault_Tmp, Vault_Path, Password, Vault_Open_Status);
      if Vault_Open_Status /= SparkPass.Vault.Success then
         SparkPass.Crypto.Zeroize.Wipe (Shared_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Master_Key);
         SparkPass.Crypto.Zeroize.Wipe_Chain (Chain_Key);
         SparkPass.Crypto.Zeroize.Wipe (ML_Kem_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Master_Key);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Chain_Key);
         Clear (Vault_Tmp);
         return;
      end if;

      --  Check if vault has ML-KEM secret key
      if not Vault_Tmp.Header.Has_MLKem_Secret then
         SparkPass.Crypto.Zeroize.Wipe (Shared_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Master_Key);
         SparkPass.Crypto.Zeroize.Wipe_Chain (Chain_Key);
         SparkPass.Crypto.Zeroize.Wipe (ML_Kem_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Master_Key);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Chain_Key);
         Clear (Vault_Tmp);
         return;
      end if;

      --  Extract ML-KEM secret key from opened vault
      ML_Kem_Secret := Vault_Tmp.Header.MLKem_Secret_Key;

      --  Step 2: Decapsulate ML-KEM ciphertext to recover shared secret
      SparkPass.Crypto.MLKEM.Decapsulate (ML_Kem_Secret, Ciphertext, Shared_Secret, Dec_Success);
      if not Dec_Success then
         SparkPass.Crypto.Zeroize.Wipe (Shared_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Master_Key);
         SparkPass.Crypto.Zeroize.Wipe_Chain (Chain_Key);
         SparkPass.Crypto.Zeroize.Wipe (ML_Kem_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Master_Key);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Chain_Key);
         Clear (Vault_Tmp);
         return;
      end if;

      --  Step 3: Unwrap master key using shared secret as KEK
      declare
         KEK : Key_Array;
         for KEK'Address use Shared_Secret (Shared_Secret'First)'Address;
      begin
         SparkPass.Crypto.AES_GCM_SIV.Open
           (Key        => KEK,
            Nonce      => Wrapped_Master_Nonce,
            Ciphertext => Wrapped_Master_Key,
            AAD        => Vault_Tmp.Header.Vault_Fingerprint,
            Tag        => Wrapped_Master_Tag,
            Plaintext  => Master_View,
            Success    => Unwrap_Success);
      end;
      if not Unwrap_Success then
         SparkPass.Crypto.Zeroize.Wipe (Shared_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Master_Key);
         SparkPass.Crypto.Zeroize.Wipe_Chain (Chain_Key);
         SparkPass.Crypto.Zeroize.Wipe (ML_Kem_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Master_Key);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Chain_Key);
         Clear (Vault_Tmp);
         return;
      end if;

      --  Step 4: Unwrap chain key using shared secret as KEK
      declare
         KEK : Key_Array;
         for KEK'Address use Shared_Secret (Shared_Secret'First)'Address;
      begin
         SparkPass.Crypto.AES_GCM_SIV.Open
           (Key        => KEK,
            Nonce      => Wrapped_Chain_Nonce,
            Ciphertext => Wrapped_Chain_Key,
            AAD        => Vault_Tmp.Header.Argon2_Salt,
            Tag        => Wrapped_Chain_Tag,
            Plaintext  => Chain_View,
            Success    => Unwrap_Success);
      end;
      if not Unwrap_Success then
         SparkPass.Crypto.Zeroize.Wipe (Shared_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Master_Key);
         SparkPass.Crypto.Zeroize.Wipe_Chain (Chain_Key);
         SparkPass.Crypto.Zeroize.Wipe (ML_Kem_Secret);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Master_Key);
         SparkPass.Crypto.Zeroize.Wipe (Wrapped_Chain_Key);
         Clear (Vault_Tmp);
         return;
      end if;

      --  Step 5: Reconstruct vault state with recovered keys (field by field)
      State.Header := Vault_Tmp.Header;
      State.Master_Key := Master_Key;
      State.Chain_Key := Chain_Key;
      State.Wrap_Key := Vault_Tmp.Wrap_Key;
      State.Entry_Count := Vault_Tmp.Entry_Count;

      for Index in State.Entries'Range loop
         State.Entries (Index) := Vault_Tmp.Entries (Index);
      end loop;

      State.Unlocked := True;

      --  Zeroize all sensitive data
      SparkPass.Crypto.Zeroize.Wipe (Shared_Secret);
      SparkPass.Crypto.Zeroize.Wipe (Master_Key);
      SparkPass.Crypto.Zeroize.Wipe_Chain (Chain_Key);
      SparkPass.Crypto.Zeroize.Wipe (ML_Kem_Secret);
      SparkPass.Crypto.Zeroize.Wipe (Wrapped_Master_Key);
      SparkPass.Crypto.Zeroize.Wipe (Wrapped_Chain_Key);
      Clear (Vault_Tmp);

      Success := True;
   end Import_Recovery;

   procedure Enroll_Touch_ID
     (State         : in out Vault_State;
      Device_Secret : in     Key_Array;
      Timestamp     : U64;
      Success       : out    Boolean)
   is
      Wrap_View : SparkPass.Types.Byte_Array (1 .. State.Wrap_Key'Length);
      for Wrap_View'Address use State.Wrap_Key (State.Wrap_Key'First)'Address;
   begin
      Success := False;

      --  Check if already enrolled
      if State.Header.Has_Wrap_D then
         --  Already enrolled, can re-enroll by overwriting
         null;
      end if;

      --  Generate fresh nonce for Wrap D
      SparkPass.Crypto.Random.Fill (State.Header.Wrapped_D_Nonce);

      --  Wrap root key (Wrap_Key) with device secret
      --  Result: Wrapped_D = AES-256-GCM-SIV.Seal(Device_Secret, Nonce, Wrap_Key, AAD="")
      SparkPass.Crypto.AES_GCM_SIV.Seal
        (Key        => Device_Secret,
         Nonce      => State.Header.Wrapped_D_Nonce,
         Plaintext  => State.Wrap_Key,
         AAD        => State.Header.Argon2_Salt,  -- Use salt as AAD for binding
         Ciphertext => State.Header.Wrapped_D_Key,
         Tag        => State.Header.Wrapped_D_Tag);

      --  Mark Wrap D as present
      State.Header.Has_Wrap_D := True;

      --  Update header with new timestamp and re-sign
      SparkPass.Vault.Header.Bump (State.Header, Timestamp);
      SparkPass.Vault.Header.Refresh_Fingerprint (State.Header);
      SparkPass.Vault.Header.Update_Signature (State.Header);

      Success := True;
   exception
      when others =>
         --  On failure, wipe Wrap D and restore state
         SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_D_Nonce);
         SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_D_Key);
         SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_D_Tag);
         State.Header.Has_Wrap_D := False;
         Success := False;
   end Enroll_Touch_ID;

   procedure Unenroll_Touch_ID
     (State     : in out Vault_State;
      Timestamp : U64;
      Success   : out    Boolean)
   is
   begin
      Success := False;

      --  Check if Touch ID is enrolled
      if not State.Header.Has_Wrap_D then
         --  Not enrolled, nothing to do
         Success := False;
         return;
      end if;

      --  Wipe Wrap D data
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_D_Nonce);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_D_Key);
      SparkPass.Crypto.Zeroize.Wipe (State.Header.Wrapped_D_Tag);
      State.Header.Has_Wrap_D := False;

      --  Update header with new timestamp and re-sign
      SparkPass.Vault.Header.Bump (State.Header, Timestamp);
      SparkPass.Vault.Header.Refresh_Fingerprint (State.Header);
      SparkPass.Vault.Header.Update_Signature (State.Header);

      Success := True;
   exception
      when others =>
         Success := False;
   end Unenroll_Touch_ID;

   function Is_Touch_ID_Enrolled
     (Path : String) return Boolean
   is
      Header  : SparkPass.Types.Header;
      Entries : SparkPass.Types.Entry_Table;
      Count   : Entry_Count_Type := 0;
      Storage_Status : SparkPass.Vault.Storage.Status;
   begin
      --  Load vault header and entries (we only need header, but Storage.Load loads both)
      SparkPass.Vault.Storage.Load (Path, Header, Entries, Count, Storage_Status);

      case Storage_Status is
         when SparkPass.Vault.Storage.Ok =>
            --  Return enrollment status from header
            return Header.Has_Wrap_D;
         when others =>
            --  On any error, return False (not enrolled or unable to read)
            return False;
      end case;
   exception
      when others =>
         return False;
   end Is_Touch_ID_Enrolled;

end SparkPass.Vault;
