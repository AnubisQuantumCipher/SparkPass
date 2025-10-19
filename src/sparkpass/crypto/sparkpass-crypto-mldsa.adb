--  ============================================================================
--  SparkPass ML-DSA Wrapper - 100% Pure SPARK Implementation
--  ============================================================================
--
--  This package provides ML-DSA-87 (FIPS 204) post-quantum digital signatures
--  using a pure SPARK implementation with zero FFI dependencies.
--
--  Implementation: SparkPass.Crypto.MLDSA87 (formally verifiable)
--  Security Level: NIST Level 5 (beyond AES-256)
--
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLDSA87;

package body SparkPass.Crypto.MLDSA is

   --  =========================================================================
   --  Pure SPARK ML-DSA-87 Implementation (Zero FFI)
   --  =========================================================================

   procedure Keypair (Public : out Public_Key; Secret : out Secret_Key) is
   begin
      --  Call pure SPARK ML-DSA-87 KeyGen
      SparkPass.Crypto.MLDSA87.KeyGen (
         Public_Key => Public,
         Secret_Key => Secret
      );
   end Keypair;

   procedure Sign
     (Secret  : Secret_Key;
      Message : Byte_Array;
      Output  : out Signature)
   is
   begin
      --  Call pure SPARK ML-DSA-87 Sign_Deterministic
      SparkPass.Crypto.MLDSA87.Sign_Deterministic (
         Secret_Key => Secret,
         Message    => Message,
         Signature  => Output
      );
   end Sign;

   procedure Verify
     (Public  : Public_Key;
      Message : Byte_Array;
      Sig     : Signature;
      Success : out Boolean)
   is
   begin
      --  Call pure SPARK ML-DSA-87 Verify
      Success := SparkPass.Crypto.MLDSA87.Verify (
         Public_Key => Public,
         Message    => Message,
         Signature  => Sig
      );
   end Verify;

end SparkPass.Crypto.MLDSA;
