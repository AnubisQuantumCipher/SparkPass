--  Minimal ML-DSA-87 Sign/Verify test
--  Tests: KeyGen → Sign → Verify cycle

with Ada.Text_IO; use Ada.Text_IO;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLDSA;

procedure Test_MLDSA_Simple_Verify is
   PK : MLDsa_Public_Key_Array := (others => 0);
   SK : MLDsa_Secret_Key_Array := (others => 0);
   Sig : MLDsa_Signature_Array := (others => 0);

   Message : constant Byte_Array (1 .. 32) := (others => 42);  -- Test message

   Verify_OK : Boolean := False;
begin
   Put_Line ("=== ML-DSA-87 Sign/Verify Test ===");
   Put_Line ("");

   Put_Line ("[1/3] Generating keypair...");
   SparkPass.Crypto.MLDSA.Keypair (PK, SK);
   Put_Line ("  ✓ KeyGen completed");

   Put_Line ("[2/3] Signing message...");
   SparkPass.Crypto.MLDSA.Sign (SK, Message, Sig);
   Put_Line ("  ✓ Sign completed");

   Put_Line ("[3/3] Verifying signature...");
   SparkPass.Crypto.MLDSA.Verify (PK, Message, Sig, Verify_OK);

   if Verify_OK then
      Put_Line ("  ✓ Verification PASSED");
      Put_Line ("");
      Put_Line ("SUCCESS: ML-DSA-87 sign/verify cycle works!");
   else
      Put_Line ("  ✗ Verification FAILED");
      Put_Line ("");
      Put_Line ("FAILURE: ML-DSA-87 Verify has a bug!");
   end if;
end Test_MLDSA_Simple_Verify;
