--  ============================================================================
--  ML-DSA-87 Basic Test Harness
--  ============================================================================
--
--  **Purpose**: Test pure SPARK ML-DSA-87 implementation
--
--  **Tests**:
--    1. KeyGen: Generate key pair
--    2. Sign: Create signature on message
--    3. Verify: Verify valid signature
--    4. Verify: Reject modified message
--    5. Verify: Reject modified signature
--
--  ============================================================================

with SparkPass.Crypto.MLDSA87; use SparkPass.Crypto.MLDSA87;
with SparkPass.Types; use SparkPass.Types;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Interfaces; use type Interfaces.Unsigned_8;

procedure Test_MLDSA87_Basic is
   PK  : Public_Key_Array;
   SK  : Secret_Key_Array;
   Sig : Signature_Array;

   --  Test message
   Msg : constant Byte_Array := (
      72, 101, 108, 108, 111, 44, 32,   -- "Hello, "
      77, 76, 45, 68, 83, 65, 45, 56, 55  -- "ML-DSA-87"
   );

   Modified_Msg : Byte_Array := Msg;
   Modified_Sig : Signature_Array;

   Valid : Boolean;
begin
   Put_Line ("==============================================");
   Put_Line ("ML-DSA-87 Pure SPARK Test Harness");
   Put_Line ("==============================================");
   New_Line;

   --  Test 1: KeyGen
   Put_Line ("[1/5] Testing KeyGen...");
   KeyGen (PK, SK);
   Put_Line ("      ✓ KeyGen completed");
   Put_Line ("      - Public key:  " & Natural'Image (PK'Length) & " bytes");
   Put_Line ("      - Secret key:  " & Natural'Image (SK'Length) & " bytes");
   New_Line;

   --  Test 2: Sign
   Put_Line ("[2/5] Testing Sign...");
   Sign_Deterministic (SK, Msg, Sig);
   Put_Line ("      ✓ Sign completed");
   Put_Line ("      - Message:     " & Natural'Image (Msg'Length) & " bytes");
   Put_Line ("      - Signature:   " & Natural'Image (Sig'Length) & " bytes");
   New_Line;

   --  Test 3: Verify (valid signature)
   Put_Line ("[3/5] Testing Verify (valid signature)...");
   Valid := Verify (PK, Msg, Sig);
   if Valid then
      Put_Line ("      ✓ Valid signature verified successfully");
   else
      Put_Line ("      ✗ FAIL: Valid signature rejected!");
      return;
   end if;
   New_Line;

   --  Test 4: Verify (modified message)
   Put_Line ("[4/5] Testing Verify (modified message)...");
   Modified_Msg := Msg;
   Modified_Msg (1) := Msg (1) xor 16#FF#;  -- Flip bits
   Valid := Verify (PK, Modified_Msg, Sig);
   if not Valid then
      Put_Line ("      ✓ Modified message correctly rejected");
   else
      Put_Line ("      ✗ FAIL: Modified message incorrectly accepted!");
      return;
   end if;
   New_Line;

   --  Test 5: Verify (modified signature)
   Put_Line ("[5/5] Testing Verify (modified signature)...");
   Modified_Sig := Sig;
   Modified_Sig (1) := Sig (1) xor 16#FF#;  -- Flip bits
   Valid := Verify (PK, Msg, Modified_Sig);
   if not Valid then
      Put_Line ("      ✓ Modified signature correctly rejected");
   else
      Put_Line ("      ✗ FAIL: Modified signature incorrectly accepted!");
      return;
   end if;
   New_Line;

   Put_Line ("==============================================");
   Put_Line ("✓ All ML-DSA-87 tests passed!");
   Put_Line ("==============================================");

exception
   when E : others =>
      Put_Line ("✗ EXCEPTION: " & Ada.Exceptions.Exception_Information (E));
end Test_MLDSA87_Basic;
