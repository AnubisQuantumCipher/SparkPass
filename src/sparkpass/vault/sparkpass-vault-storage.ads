pragma SPARK_Mode (On);
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Vault.Storage is
   type Status is
     (Ok,
      Io_Error,
      Format_Error,
      Integrity_Error,
      Permission_Error);

   --  Save vault to disk with atomic write (temp file + fsync + rename).
   --  Updates Header.Modified_At, Header.Vault_Fingerprint, and Header.Header_Signature.
   --
   --  Security Properties:
   --  - Atomic write prevents partial vault corruption (POSIX rename guarantee)
   --  - Permissions set to 0600 before rename (prevents race condition)
   --  - ML-DSA-87 signature computed over header + entries (tamper detection)
   --  - Finalization marker written to detect incomplete writes
   --
   --  Post: On success (Ok), Header is properly signed with ML-DSA-87.
   --        On failure, Result indicates specific error type.
   procedure Save
     (Path    : String;
      Header  : in out SparkPass.Types.Header;
      Entries : SparkPass.Types.Entry_Table;
      Count   : Entry_Count_Type;
      Result  : out Status)
     with
       Global  => null,
       Depends => (Header => (Entries, Count),
                   Result => Header),
       Post    => (if Result = Ok then
                     (Header.Header_Signature'Length = SparkPass.Config.MLDsa_Signature_Length and
                      Header.MLDsa_Public_Key'Length = SparkPass.Config.MLDsa_Public_Key_Length)
                   else
                     Result in Io_Error | Format_Error | Integrity_Error | Permission_Error);

   --  Load vault from disk with integrity verification.
   --  Validates file permissions (0600), owner, ML-DSA-87 signature, and finalization marker.
   --
   --  Security Properties:
   --  - Uses lstat (not stat) to prevent symlink TOCTOU attacks
   --  - Verifies owner = current user (prevents reading other users' vaults)
   --  - Checks file type = regular file (rejects symlinks, devices, pipes)
   --  - ML-DSA-87 signature verification ensures vault integrity
   --  - Finalization marker detection prevents reading incomplete writes
   --
   --  Post: On success (Ok), Count represents valid number of entries loaded.
   --        On failure, Result indicates specific error type and Count is 0.
   procedure Load
     (Path    : String;
      Header  : out SparkPass.Types.Header;
      Entries : out SparkPass.Types.Entry_Table;
      Count   : out Entry_Count_Type;
      Result  : out Status)
     with
       Global  => null,
       Depends => (Header => null,
                   Entries => null,
                   Count => null,
                   Result => null),
       Post    => (if Result = Ok then
                     (Count <= Entry_Count_Type (SparkPass.Config.Max_Entries) and
                      Header.Header_Signature'Length = SparkPass.Config.MLDsa_Signature_Length)
                   else
                     (Count = 0 and
                      Result in Io_Error | Format_Error | Integrity_Error | Permission_Error));

end SparkPass.Vault.Storage;
