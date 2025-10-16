pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_32; use type Interfaces.Unsigned_64;
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Vault.Header is
   procedure Initialize
     (State     : out SparkPass.Types.Header;
      Password  : Byte_Array;
      Timestamp : U64;
      Master    : out Key_Array;
      Chain     : out Chain_Key_Array;
      Wrap_Key  : out Key_Array;
      Signing   : out SparkPass.Types.MLDsa_Secret_Key_Array)
     with
       Global  => null,
       Depends => (State   => (Password, Timestamp),
                   Master  => null,
                   Chain   => null,
                   Wrap_Key => Password,
                   Signing => null),
       Post    => Has_Signing_Key (State);

   function Has_Signing_Key (State : SparkPass.Types.Header) return Boolean
     with
       Global  => null,
       Depends => (Has_Signing_Key'Result => State);

   procedure Clear_Signing_Key (State : in out SparkPass.Types.Header)
     with
       Global  => null,
       Depends => (State => State),
       Post    => not Has_Signing_Key (State);

   procedure Bump
     (State     : in out SparkPass.Types.Header;
      Timestamp : U64)
     with
       Global  => null,
       Pre     => Has_Signing_Key (State),
       Depends => (State => (State, Timestamp)),
       Post    => State.Modified_At = Timestamp
         and then State.Nonce_Counter = State.Nonce_Counter'Old + 1;

   function Compute_Fingerprint (State : SparkPass.Types.Header) return Fingerprint_Array
     with
       Global  => null,
       Depends => (Compute_Fingerprint'Result => State);

   procedure Refresh_Fingerprint (State : in out SparkPass.Types.Header)
     with
       Global  => null,
       Depends => (State => State),
       Post    => State.Vault_Fingerprint = Compute_Fingerprint (State);

   procedure Update_Signature (State : in out SparkPass.Types.Header)
     with
       Global  => null,
       Pre     => Has_Signing_Key (State),
       Depends => (State => State);

   function Verify_Password
     (State    : SparkPass.Types.Header;
      Password : Byte_Array) return Boolean
     with
       Global  => null,
       Depends => (Verify_Password'Result => (State, Password));

end SparkPass.Vault.Header;
