pragma SPARK_Mode (On);
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.MLDSA is
   subtype Public_Key is MLDsa_Public_Key_Array;
   subtype Secret_Key is MLDsa_Secret_Key_Array;
   subtype Signature  is MLDsa_Signature_Array;

   procedure Keypair (Public : out Public_Key; Secret : out Secret_Key)
     with
       Global  => null,
       Depends => (Public => null, Secret => null);

   procedure Sign
     (Secret  : Secret_Key;
      Message : Byte_Array;
      Output  : out Signature)
     with
       Global  => null,
       Pre     => Message'Length > 0 and then
                  Message'Length <= 65536,  -- 64KB reasonable maximum
       Depends => (Output => (Secret, Message));

   procedure Verify
     (Public  : Public_Key;
      Message : Byte_Array;
      Sig     : Signature;
      Success : out Boolean)
     with
       Global  => null,
       Pre     => Message'Length > 0 and then
                  Message'Length <= 65536,  -- 64KB reasonable maximum
       Depends => (Success => (Public, Message, Sig));

end SparkPass.Crypto.MLDSA;
