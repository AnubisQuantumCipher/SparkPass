pragma SPARK_Mode (Off);
--  Minimal ML-DSA-87 test to identify crash

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLDSA; use SparkPass.Crypto.MLDSA;

procedure Test_MLDSA87_Minimal is
   Public1 : Public_Key;
   Secret1 : Secret_Key;
begin
   Put_Line ("Starting ML-DSA-87 minimal test...");
   Put_Line ("Calling Keypair...");

   Keypair (Public => Public1, Secret => Secret1);

   Put_Line ("Keypair generated successfully!");
   Put_Line ("Test PASSED");

exception
   when E : others =>
      Put_Line ("ERROR: " & Ada.Exceptions.Exception_Information (E));
      Put_Line ("Test FAILED");
end Test_MLDSA87_Minimal;
