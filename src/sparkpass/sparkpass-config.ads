pragma SPARK_Mode (On);
with Interfaces;

package SparkPass.Config is
   pragma Preelaborate;

   Magic_Text : constant String := "SPKv1";
   Magic_Length : constant := Magic_Text'Length;

   Version : constant Interfaces.Unsigned_8 := 1;

   --  Header size increased from 6500 to 9697 for ML-KEM secret storage
   --  Added: MLKem_Secret_Nonce (12) + MLKem_Secret_Value (3168) +
   --         MLKem_Secret_Tag (16) + Has_MLKem_Secret (1) = 3197 bytes
   Header_Size         : constant := 9697;
   Footer_Size         : constant := 4643;
   Finalization_Marker : constant String := "SPKv1:FINAL";

   Argon2_Memory_KiB : constant Interfaces.Unsigned_32 := 1_048_576;  -- 1 GiB
   Argon2_Iterations : constant Interfaces.Unsigned_32 := 4;
   Argon2_Parallelism : constant Interfaces.Unsigned_32 := 1;
   Argon2_Salt_Length : constant Positive := 32;

   type Argon2_Verification_Preset is
     (Test_Small,
      Test_Medium,
      Production);

   --  Default proof preset stays on the medium model so GNATprove completes
   --  quickly.  Builds can override this via gnatprove switches when running
   --  production-scale proofs.
   Argon2_Verification_Mode : constant Argon2_Verification_Preset :=
     Test_Medium;

   Master_Key_Length : constant Positive := 32;
   Nonce_Length      : constant Positive := 12;
   Tag_Length        : constant Positive := 16;

   Chain_Key_Length  : constant Positive := 32;

   -- Post-Quantum Cryptography (ML-DSA-87, NIST FIPS 204)
   -- Note: Secret key sizes updated to match liboqs 0.14.0 actual implementation
   -- Blueprint specified 4,864 bytes but liboqs 0.14.0 uses 4,896 bytes
   MLDsa_Public_Key_Length  : constant Positive := 2592;
   MLDsa_Secret_Key_Length  : constant Positive := 4896;
   MLDsa_Signature_Length   : constant Positive := 4627;

   --  Feature flag: Use pure SPARK ML-DSA-87 implementation (default: True)
   --  When True:  Uses SparkPass.Crypto.MLDSA87 (pure SPARK, zero FFI)
   --  When False: Uses Bindings.LibOQS (FFI wrapper for testing/comparison)
   Use_Pure_SPARK_MLDSA : constant Boolean := True;

   -- Post-Quantum Cryptography (ML-KEM-1024, NIST FIPS 203)
   -- Note: Secret key sizes updated to match liboqs 0.14.0 actual implementation
   -- Blueprint specified 2,528 bytes but liboqs 0.14.0 uses 3,168 bytes
   MLKem_Public_Key_Length    : constant Positive := 1568;
   MLKem_Secret_Key_Length    : constant Positive := 3168;
   MLKem_Ciphertext_Length    : constant Positive := 1568;
   MLKem_Shared_Key_Length    : constant Positive := 32;

   Max_Label_Length : constant Positive := 256;
   Max_Data_Length  : constant Positive := 4_096;
   --  Max_Entries: 2000 entries (heap-allocated ~9 MB Vault_State in sparkpass_main.adb)
   --  Each entry is ~4.5 KB, more than sufficient for most password manager use cases
   Max_Entries      : constant Positive := 2000;

   --  CLI password handling
   Password_Buffer_Size : constant Positive := 256;
   Min_Password_Length  : constant Positive := 12;

end SparkPass.Config;
