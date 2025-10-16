pragma SPARK_Mode (Off);
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Interfaces; use type Interfaces.Unsigned_8; use type Interfaces.Unsigned_16; use type Interfaces.Unsigned_32; use type Interfaces.Unsigned_64;
with Interfaces.C;
with Interfaces.C.Strings;
with SparkPass.Config;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Vault.Header;
with SparkPass.Crypto.Zeroize;
with Bindings.POSIX;

package body SparkPass.Vault.Storage is

   procedure Save
     (Path    : String;
      Header  : in out SparkPass.Types.Header;
      Entries : SparkPass.Types.Entry_Table;
      Count   : Entry_Count_Type;
      Result  : out Status)
   is
      use Ada.Streams.Stream_IO;
      use Interfaces.C;
      use Interfaces.C.Strings;

      File      : File_Type;
      IO_Stream : Stream_Access;
      Temp_Path : constant String := Path & ".tmp";

      procedure Write_String (S : String) is
      begin
         for Ch of S loop
            Interfaces.Unsigned_8'Write (IO_Stream, Interfaces.Unsigned_8 (Character'Pos (Ch)));
         end loop;
      end Write_String;

      procedure Write_Bytes (Buffer : SparkPass.Types.Byte_Array; Length : Natural) is
      begin
         if Length = 0 then
            return;
         end if;
         for Index in 0 .. Length - 1 loop
            Interfaces.Unsigned_8'Write
              (IO_Stream,
               Interfaces.Unsigned_8 (Buffer (Buffer'First + Index)));
         end loop;
      end Write_Bytes;

      procedure Write_Key (Buffer : SparkPass.Types.Byte_Array) is
      begin
         for Byte of Buffer loop
            Interfaces.Unsigned_8'Write (IO_Stream, Interfaces.Unsigned_8 (Byte));
         end loop;
      end Write_Key;

      procedure Fsync_File (File_Path : String) is
         FD : Interfaces.C.int;
         Path_C : chars_ptr;
         Sync_Result : Interfaces.C.int;
         Close_Result : Interfaces.C.int;
      begin
         Path_C := New_String (File_Path);
         FD := Bindings.POSIX.open (Path_C, Bindings.POSIX.O_RDONLY);
         Free (Path_C);

         if FD < 0 then
            raise Program_Error with "Failed to open file for fsync: " & File_Path;
         end if;

         Sync_Result := Bindings.POSIX.fsync (FD);
         if Sync_Result /= 0 then
            Close_Result := Bindings.POSIX.close (FD);
            raise Program_Error with "fsync failed for file: " & File_Path;
         end if;

         Close_Result := Bindings.POSIX.close (FD);
         if Close_Result /= 0 then
            raise Program_Error with "close failed after fsync for file: " & File_Path;
         end if;
      end Fsync_File;

      procedure Atomic_Rename (Old_Path, New_Path : String) is
         Old_C : chars_ptr;
         New_C : chars_ptr;
         Rename_Result : Interfaces.C.int;
      begin
         Old_C := New_String (Old_Path);
         New_C := New_String (New_Path);
         Rename_Result := Bindings.POSIX.rename (Old_C, New_C);
         Free (Old_C);
         Free (New_C);
         if Rename_Result /= 0 then
            raise Program_Error with "Atomic rename failed";
         end if;
      end Atomic_Rename;

   begin
      if not SparkPass.Vault.Header.Has_Signing_Key (Header) then
         Result := Integrity_Error;
         return;
      end if;

      Header.Entry_Count := Count;
      SparkPass.Vault.Header.Refresh_Fingerprint (Header);
      SparkPass.Vault.Header.Update_Signature (Header);

      --  Write to temporary file first (atomic write pattern)
      Create (File, Out_File, Temp_Path);
      IO_Stream := Stream (File);

      Write_String (Header.Magic);
      Interfaces.Unsigned_8'Write (IO_Stream, Header.Version);
      Interfaces.Unsigned_64'Write (IO_Stream, Header.Created_At);
      Interfaces.Unsigned_64'Write (IO_Stream, Header.Modified_At);
      Interfaces.Unsigned_64'Write (IO_Stream, Header.Nonce_Counter);
      Interfaces.Unsigned_32'Write (IO_Stream, Header.Entry_Count);
      Write_Key (Header.Vault_Fingerprint);

      Interfaces.Unsigned_32'Write (IO_Stream, Header.Argon2_Memory);
      Interfaces.Unsigned_32'Write (IO_Stream, Header.Argon2_Iterations);
      Interfaces.Unsigned_32'Write (IO_Stream, Header.Argon2_Parallelism);
      Write_Key (Header.Argon2_Salt);

      Write_Key (Header.Wrapped_Master_Nonce);
      Write_Key (Header.Wrapped_Master_Key);
      Write_Key (Header.Wrapped_Master_Tag);

      Write_Key (Header.Chain_Key_Nonce);
      Write_Key (Header.Chain_Key_Value);
      Write_Key (Header.Chain_Key_Tag);

      Write_Key (Header.MLDsa_Secret_Nonce);
      Write_Key (Header.MLDsa_Secret_Value);
      Write_Key (Header.MLDsa_Secret_Tag);

      Write_Key (Header.MLKem_Secret_Nonce);
      Write_Key (Header.MLKem_Secret_Value);
      Write_Key (Header.MLKem_Secret_Tag);
      Interfaces.Unsigned_8'Write (IO_Stream, (if Header.Has_MLKem_Secret then 1 else 0));

      --  Wrap D (Touch ID) fields
      Write_Key (Header.Wrapped_D_Nonce);
      Write_Key (Header.Wrapped_D_Key);
      Write_Key (Header.Wrapped_D_Tag);
      Interfaces.Unsigned_8'Write (IO_Stream, (if Header.Has_Wrap_D then 1 else 0));

      Write_Key (Header.MLDsa_Public_Key);
      Write_Key (Header.MLKem_Public_Key);
      Write_Key (Header.Header_Signature);

      declare
         Limit : constant Natural := Natural (Count);
      begin
         for Index in 1 .. Limit loop
            declare
               Vault_Entry   : constant SparkPass.Types.Entry_Record := Entries (Index);
               Label_Length : constant Natural := Natural (Vault_Entry.Label_Len);
               Data_Length  : constant Natural := Natural (Vault_Entry.Data_Len);
            begin
               Interfaces.Unsigned_16'Write (IO_Stream, Vault_Entry.Label_Len);
               Interfaces.Unsigned_32'Write (IO_Stream, Vault_Entry.Data_Len);
               Interfaces.Unsigned_64'Write (IO_Stream, Vault_Entry.Created_At);
               Interfaces.Unsigned_64'Write (IO_Stream, Vault_Entry.Modified_At);
               Interfaces.Unsigned_8'Write
                 (IO_Stream,
                  Interfaces.Unsigned_8 (SparkPass.Types.Entry_Type'Pos (Vault_Entry.Kind)));

               Write_Key (Vault_Entry.Id);
               Write_Bytes (Vault_Entry.Label, Label_Length);
               Write_Key (Vault_Entry.Nonce);
               Write_Bytes (Vault_Entry.Ciphertext, Data_Length);
               Write_Key (Vault_Entry.Tag);
               Write_Key (Vault_Entry.Signature);
            end;
         end loop;
      end;

      Write_String (SparkPass.Config.Finalization_Marker);

      --  Close temporary file to ensure all data is written
      Close (File);

      --  Set restrictive permissions (0600 = owner read/write only)
      --  CRITICAL: Must happen before atomic rename to prevent race condition
      declare
         Temp_C : chars_ptr := New_String (Temp_Path);
         Chmod_Result : Interfaces.C.int;
      begin
         Chmod_Result := Bindings.POSIX.chmod (Temp_C, 8#600#);
         Free (Temp_C);
         if Chmod_Result /= 0 then
            --  Fail closed: delete temp file and abort
            if Ada.Directories.Exists (Temp_Path) then
               Ada.Directories.Delete_File (Temp_Path);
            end if;
            Result := Io_Error;
            return;
         end if;
      end;

      --  Fsync the temporary file to ensure data is persisted to disk
      Fsync_File (Temp_Path);

      --  Atomically rename temporary file to final path
      --  This ensures either old vault exists or new vault exists, never partial
      Atomic_Rename (Temp_Path, Path);

      --  Optionally fsync parent directory to ensure directory entry is persisted
      declare
         Parent_Dir : constant String := Ada.Directories.Containing_Directory (Path);
      begin
         if Parent_Dir'Length > 0 then
            Fsync_File (Parent_Dir);
         end if;
      exception
         when others =>
            null;  -- fsync on directory is optional, ignore failures
      end;

      Result := Ok;
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Close (File);
         end if;
         --  Clean up temporary file if it exists
         if Ada.Directories.Exists (Temp_Path) then
            Ada.Directories.Delete_File (Temp_Path);
         end if;
         Result := Io_Error;
   end Save;

   procedure Load
     (Path    : String;
      Header  : out SparkPass.Types.Header;
      Entries : out SparkPass.Types.Entry_Table;
      Count   : out Entry_Count_Type;
      Result  : out Status)
   is
      use Ada.Streams.Stream_IO;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use type Interfaces.C.int;
      use type Interfaces.C.unsigned;

      File      : File_Type;
      IO_Stream : Stream_Access;
      Magic_Buffer : String (1 .. SparkPass.Config.Magic_Length);
      Marker : String (1 .. SparkPass.Config.Finalization_Marker'Length);

      procedure Read_Bytes (Buffer : in out SparkPass.Types.Byte_Array) is
      begin
         for Index in Buffer'Range loop
            declare
               Byte : Interfaces.Unsigned_8;
            begin
               Interfaces.Unsigned_8'Read (IO_Stream, Byte);
               Buffer (Index) := SparkPass.Types.U8 (Byte);
            end;
         end loop;
      end Read_Bytes;

      procedure Read_Slice
        (Target : in out SparkPass.Types.Byte_Array;
         Length : Natural) is
      begin
         if Length > 0 then
            for Offset in 0 .. Length - 1 loop
               declare
                  Byte : Interfaces.Unsigned_8;
               begin
                  Interfaces.Unsigned_8'Read (IO_Stream, Byte);
                  Target (Target'First + Offset) := SparkPass.Types.U8 (Byte);
               end;
            end loop;
         end if;
         if Length < Target'Length then
            for Offset in Length .. Target'Length - 1 loop
               Target (Target'First + Offset) := 0;
            end loop;
         end if;
      end Read_Slice;

   begin
      --  Validate file ownership and permissions before opening
      declare
         Path_C : chars_ptr := New_String (Path);
         File_Stat : aliased Bindings.POSIX.stat_t;
         Stat_Result : Interfaces.C.int;
         Current_UID : Interfaces.C.unsigned;
         File_Mode : Interfaces.C.unsigned;
      begin
         --  Use lstat instead of stat to prevent symlink TOCTOU attacks
         --  lstat returns info about the symlink itself, not the target
         Stat_Result := Bindings.POSIX.lstat (Path_C, File_Stat'Access);
         Free (Path_C);

         if Stat_Result /= 0 then
            Result := Io_Error;
            Count := 0;
            return;
         end if;

         --  Verify file is owned by current user
         Current_UID := Bindings.POSIX.getuid;
         if File_Stat.st_uid /= Current_UID then
            Result := Integrity_Error;
            Count := 0;
            return;
         end if;

         --  Verify file is a regular file (not a symlink, device, etc.)
         --  With lstat, this check will reject symlinks (S_IFLNK)
         if (Interfaces.C.unsigned (File_Stat.st_mode) and Bindings.POSIX.S_IFMT) /= Bindings.POSIX.S_IFREG then
            Result := Format_Error;
            Count := 0;
            return;
         end if;

         --  Verify file is not readable by group or others (must be owner-only)
         File_Mode := Interfaces.C.unsigned (File_Stat.st_mode) and (Bindings.POSIX.S_IRWXG or Bindings.POSIX.S_IRWXO);
         if File_Mode /= 0 then
            --  Return specific error for permission issues
            Result := Permission_Error;
            Count := 0;
            return;
         end if;
      end;

      Open (File, In_File, Path);
      IO_Stream := Stream (File);

      for Index in Magic_Buffer'Range loop
         declare
            Byte : Interfaces.Unsigned_8;
         begin
            Interfaces.Unsigned_8'Read (IO_Stream, Byte);
            Magic_Buffer (Index) := Character'Val (Integer (Byte));
         end;
      end loop;

      Header.Magic := Magic_Buffer;
      Interfaces.Unsigned_8'Read (IO_Stream, Header.Version);
      Interfaces.Unsigned_64'Read (IO_Stream, Header.Created_At);
      Interfaces.Unsigned_64'Read (IO_Stream, Header.Modified_At);
      Interfaces.Unsigned_64'Read (IO_Stream, Header.Nonce_Counter);
      Interfaces.Unsigned_32'Read (IO_Stream, Header.Entry_Count);
      Read_Bytes (Header.Vault_Fingerprint);

      Interfaces.Unsigned_32'Read (IO_Stream, Header.Argon2_Memory);
      Interfaces.Unsigned_32'Read (IO_Stream, Header.Argon2_Iterations);
      Interfaces.Unsigned_32'Read (IO_Stream, Header.Argon2_Parallelism);
      Read_Bytes (Header.Argon2_Salt);

      Read_Bytes (Header.Wrapped_Master_Nonce);
      Read_Bytes (Header.Wrapped_Master_Key);
      Read_Bytes (Header.Wrapped_Master_Tag);

      Read_Bytes (Header.Chain_Key_Nonce);
      Read_Bytes (Header.Chain_Key_Value);
      Read_Bytes (Header.Chain_Key_Tag);

      Read_Bytes (Header.MLDsa_Secret_Nonce);
      Read_Bytes (Header.MLDsa_Secret_Value);
      Read_Bytes (Header.MLDsa_Secret_Tag);

      Read_Bytes (Header.MLKem_Secret_Nonce);
      Read_Bytes (Header.MLKem_Secret_Value);
      Read_Bytes (Header.MLKem_Secret_Tag);
      declare
         Has_MLKem_Byte : Interfaces.Unsigned_8;
      begin
         Interfaces.Unsigned_8'Read (IO_Stream, Has_MLKem_Byte);
         Header.Has_MLKem_Secret := (Has_MLKem_Byte /= 0);
      end;

      --  Wrap D (Touch ID) fields
      Read_Bytes (Header.Wrapped_D_Nonce);
      Read_Bytes (Header.Wrapped_D_Key);
      Read_Bytes (Header.Wrapped_D_Tag);
      declare
         Has_Wrap_D_Byte : Interfaces.Unsigned_8;
      begin
         Interfaces.Unsigned_8'Read (IO_Stream, Has_Wrap_D_Byte);
         Header.Has_Wrap_D := (Has_Wrap_D_Byte /= 0);
      end;

      Read_Bytes (Header.MLDsa_Public_Key);
      Read_Bytes (Header.MLKem_Public_Key);
      Read_Bytes (Header.Header_Signature);

      Header.Has_MLDsa_Secret := False;
      SparkPass.Crypto.Zeroize.Wipe (Header.MLDsa_Secret_Key);
      if not Header.Has_MLKem_Secret then
         SparkPass.Crypto.Zeroize.Wipe (Header.MLKem_Secret_Key);
      end if;

      declare
         Raw_Count : constant Interfaces.Unsigned_32 := Header.Entry_Count;
         Expected_Count : Natural;
      begin
         if Raw_Count > Interfaces.Unsigned_32 (SparkPass.Config.Max_Entries) then
            Result := Format_Error;
            Close (File);
            Count := 0;
            return;
         end if;

         Expected_Count := Natural (Raw_Count);
         Count := Header.Entry_Count;

         for Index in 1 .. Expected_Count loop
            declare
               Loaded_Entry : SparkPass.Types.Entry_Record;
               Kind_Byte : Interfaces.Unsigned_8;
               Label_Length : Interfaces.Unsigned_16;
               Data_Length  : Interfaces.Unsigned_32;
            begin
               Interfaces.Unsigned_16'Read (IO_Stream, Label_Length);
               Interfaces.Unsigned_32'Read (IO_Stream, Data_Length);
               Interfaces.Unsigned_64'Read (IO_Stream, Loaded_Entry.Created_At);
               Interfaces.Unsigned_64'Read (IO_Stream, Loaded_Entry.Modified_At);
              Interfaces.Unsigned_8'Read (IO_Stream, Kind_Byte);

              Loaded_Entry.Label_Len := Label_Length;
              Loaded_Entry.Data_Len  := Data_Length;

              if Natural (Loaded_Entry.Label_Len) > SparkPass.Config.Max_Label_Length
                or else Natural (Loaded_Entry.Data_Len) > SparkPass.Config.Max_Data_Length
               then
                  Result := Format_Error;
                  Close (File);
                  Count := 0;
                  return;
               end if;
               declare
                  Kind_Pos : constant Integer := Integer (Kind_Byte);
               begin
                  if Kind_Pos < SparkPass.Types.Entry_Type'Pos (SparkPass.Types.Entry_Type'First)
                    or else Kind_Pos > SparkPass.Types.Entry_Type'Pos (SparkPass.Types.Entry_Type'Last)
                  then
                     Result := Format_Error;
                     Close (File);
                     Count := 0;
                     return;
                  end if;
                  Loaded_Entry.Kind := SparkPass.Types.Entry_Type'Val (Kind_Pos);
               end;
               Read_Bytes (Loaded_Entry.Id);
               Read_Slice (Loaded_Entry.Label, Natural (Loaded_Entry.Label_Len));
               Read_Bytes (Loaded_Entry.Nonce);
               Read_Slice (Loaded_Entry.Ciphertext, Natural (Loaded_Entry.Data_Len));
               Read_Bytes (Loaded_Entry.Tag);
               Read_Bytes (Loaded_Entry.Signature);

               Entries (Index) := Loaded_Entry;
            end;
         end loop;

         for Index in Expected_Count + 1 .. SparkPass.Config.Max_Entries loop
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Id);
            Entries (Index).Label_Len := 0;
            Entries (Index).Data_Len  := 0;
            Entries (Index).Created_At := 0;
            Entries (Index).Modified_At := 0;
            Entries (Index).Kind := SparkPass.Types.Password;
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Label);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Nonce);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Ciphertext);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Tag);
            SparkPass.Crypto.Zeroize.Wipe (Entries (Index).Signature);
         end loop;
      end;

      for Index in Marker'Range loop
         declare
            Byte : Interfaces.Unsigned_8;
         begin
            Interfaces.Unsigned_8'Read (IO_Stream, Byte);
            Marker (Index) := Character'Val (Integer (Byte));
         end;
      end loop;

      --  Verify finalization marker
      if Marker /= SparkPass.Config.Finalization_Marker then
         Result := Format_Error;
         Close (File);
         Count := 0;
         return;
      end if;

      Close (File);

      declare
         Expected : constant Fingerprint_Array := SparkPass.Vault.Header.Compute_Fingerprint (Header);
      begin
         if Expected /= Header.Vault_Fingerprint then
            Result := Integrity_Error;
            Count := 0;
            return;
         end if;
      end;

      Result := Ok;
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Close (File);
         end if;
         Count := 0;
         Result := Io_Error;
   end Load;

end SparkPass.Vault.Storage;
