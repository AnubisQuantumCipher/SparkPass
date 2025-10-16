pragma SPARK_Mode (On);
with Interfaces;
with SparkPass.Config;

package SparkPass.Types is
   pragma Preelaborate;

   subtype U8  is Interfaces.Unsigned_8;
   subtype U16 is Interfaces.Unsigned_16;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;

   type Byte_Array is array (Positive range <>) of U8
     with Pack;

   Fingerprint_Size : constant Positive := 32;
   subtype Fingerprint_Array is Byte_Array (1 .. Fingerprint_Size);

   subtype Salt_Array is Byte_Array (1 .. SparkPass.Config.Argon2_Salt_Length);
   subtype Key_Array is Byte_Array (1 .. SparkPass.Config.Master_Key_Length);
   subtype Nonce_Array is Byte_Array (1 .. SparkPass.Config.Nonce_Length);
   subtype Tag_Array is Byte_Array (1 .. SparkPass.Config.Tag_Length);
   subtype Chain_Key_Array is Byte_Array (1 .. SparkPass.Config.Chain_Key_Length);
   subtype MLDsa_Public_Key_Array is Byte_Array (1 .. SparkPass.Config.MLDsa_Public_Key_Length);
   subtype MLDsa_Secret_Key_Array is Byte_Array (1 .. SparkPass.Config.MLDsa_Secret_Key_Length);
   subtype MLDsa_Signature_Array is Byte_Array (1 .. SparkPass.Config.MLDsa_Signature_Length);
   subtype MLKem_Public_Key_Array is Byte_Array (1 .. SparkPass.Config.MLKem_Public_Key_Length);
   subtype MLKem_Secret_Key_Array is Byte_Array (1 .. SparkPass.Config.MLKem_Secret_Key_Length);
   subtype MLKem_Ciphertext_Array is Byte_Array (1 .. SparkPass.Config.MLKem_Ciphertext_Length);
   subtype MLKem_Shared_Key_Array is Byte_Array (1 .. SparkPass.Config.MLKem_Shared_Key_Length);

   Entry_Id_Size : constant Positive := 16;
   subtype Entry_Id_Array is Byte_Array (1 .. Entry_Id_Size);

   type Entry_Type is (Password, TOTP, Secure_Note, SSH_Key);

   for Entry_Type use
     (Password     => 1,
      TOTP         => 2,
      Secure_Note  => 3,
      SSH_Key      => 4);

   for Entry_Type'Size use 8;

   subtype Label_Length_Type is U16;
   subtype Data_Length_Type  is U32;

   type Entry_Record is record
      Id          : Entry_Id_Array := (others => 0);
      Kind        : Entry_Type     := Password;
      Label_Len   : Label_Length_Type := 0;
   Data_Len    : Data_Length_Type  := 0;
   Created_At  : U64 := 0;
   Modified_At : U64 := 0;
   Label       : Byte_Array (1 .. SparkPass.Config.Max_Label_Length) := (others => 0);
   Nonce       : Nonce_Array := (others => 0);
      Ciphertext  : Byte_Array (1 .. SparkPass.Config.Max_Data_Length) := (others => 0);
      Tag         : Tag_Array := (others => 0);
      Signature   : Byte_Array (1 .. 64) := (others => 0);
   end record;

   subtype Entry_Count_Type is U32;

   type Header is record
      Magic            : String (1 .. SparkPass.Config.Magic_Length) := SparkPass.Config.Magic_Text;
      Version          : U8 := SparkPass.Config.Version;
      Created_At       : U64 := 0;
      Modified_At      : U64 := 0;
      Nonce_Counter    : U64 := 0;
      Entry_Count      : Entry_Count_Type := 0;
      Vault_Fingerprint : Fingerprint_Array := (others => 0);
      Argon2_Memory    : U32 := SparkPass.Config.Argon2_Memory_KiB;
      Argon2_Iterations : U32 := SparkPass.Config.Argon2_Iterations;
      Argon2_Parallelism : U32 := SparkPass.Config.Argon2_Parallelism;
      Argon2_Salt      : Salt_Array := (others => 0);
      --  Wrap A (Passphrase): Root key wrapped with Argon2id-derived KEK
      Wrapped_Master_Nonce : Nonce_Array := (others => 0);
      Wrapped_Master_Key   : Key_Array := (others => 0);
      Wrapped_Master_Tag   : Tag_Array := (others => 0);
      --  Chain key wrapping
      Chain_Key_Nonce      : Nonce_Array := (others => 0);
      Chain_Key_Value      : Chain_Key_Array := (others => 0);
      Chain_Key_Tag        : Tag_Array := (others => 0);
      --  ML-DSA signing key (encrypted in header)
      MLDsa_Secret_Nonce   : Nonce_Array := (others => 0);
      MLDsa_Secret_Value   : MLDsa_Secret_Key_Array := (others => 0);
      MLDsa_Secret_Tag     : Tag_Array := (others => 0);
      Has_MLDsa_Secret     : Boolean := False;
      MLDsa_Secret_Key     : MLDsa_Secret_Key_Array := (others => 0);
      --  ML-KEM secret key (encrypted in header)
      MLKem_Secret_Nonce   : Nonce_Array := (others => 0);
      MLKem_Secret_Value   : MLKem_Secret_Key_Array := (others => 0);
      MLKem_Secret_Tag     : Tag_Array := (others => 0);
      Has_MLKem_Secret     : Boolean := False;
      MLKem_Secret_Key     : MLKem_Secret_Key_Array := (others => 0);
      --  Wrap D (Touch ID): Root key wrapped with device secret from Keychain
      Wrapped_D_Nonce      : Nonce_Array := (others => 0);
      Wrapped_D_Key        : Key_Array := (others => 0);
      Wrapped_D_Tag        : Tag_Array := (others => 0);
      Has_Wrap_D           : Boolean := False;
      --  Public keys
      MLDsa_Public_Key     : MLDsa_Public_Key_Array := (others => 0);
      MLKem_Public_Key     : MLKem_Public_Key_Array := (others => 0);
      --  Header signature
      Header_Signature     : MLDsa_Signature_Array := (others => 0);
   end record;

type Entry_Array is array (Positive range <>) of Entry_Record;
subtype Entry_Table is Entry_Array (1 .. SparkPass.Config.Max_Entries);

end SparkPass.Types;
