pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Hash; use SparkPass.Crypto.MLKEM.Hash;

--  ========================================================================
--  ML-KEM Hash Functions Test
--  ========================================================================
--
--  **Purpose**: Verify SHA3-512 and SHA3-256 implementations work correctly
--
--  **Test Categories**:
--    1. G_Expand (SHA3-512) basic functionality
--    2. H_Hash (SHA3-256) basic functionality
--    3. Deterministic behavior
--    4. Output length verification
--
--  ========================================================================

procedure Test_MLKEM_Hash is

   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Tests_Run := Tests_Run + 1;
      if Condition then
         Tests_Passed := Tests_Passed + 1;
         Put_Line("[PASS] " & Test_Name);
      else
         Tests_Failed := Tests_Failed + 1;
         Put_Line("[FAIL] " & Test_Name);
      end if;
   end Assert;

   procedure Print_Hex (Label : String; Data : Byte_Array) is
   begin
      Put(Label & ": ");
      for I in Data'Range loop
         Put(Unsigned_8'Image(Data(I)));
         if I < Data'Last then
            Put(" ");
         end if;
      end loop;
      New_Line;
   end Print_Hex;

   --  ========================================================================
   --  Test G_Expand (SHA3-512)
   --  ========================================================================

   procedure Test_G_Expand is
      Seed   : Byte_Array(1 .. 32) := (others => 0);
      K_Val  : U8 := 4;  -- ML-KEM-1024 parameter
      Output : SHA3_512_Digest;
   begin
      Put_Line("========================================");
      Put_Line("Test G_Expand (SHA3-512)");
      Put_Line("========================================");

      --  Test 1: Basic call with zero seed
      G_Expand(Seed, K_Val, Output);
      Assert(Output'Length = 64, "G_Expand produces 64-byte output");

      --  Test 2: Output is not all zeros (hash actually ran)
      declare
         All_Zero : Boolean := True;
      begin
         for I in Output'Range loop
            if Output(I) /= 0 then
               All_Zero := False;
               exit;
            end if;
         end loop;
         Assert(not All_Zero, "G_Expand output is not all zeros");
      end;

      --  Test 3: Different seeds produce different outputs
      declare
         Seed2   : Byte_Array(1 .. 32) := (others => 255);
         Output2 : SHA3_512_Digest;
         Different : Boolean := False;
      begin
         G_Expand(Seed2, K_Val, Output2);
         for I in Output'Range loop
            if Output(I) /= Output2(I) then
               Different := True;
               exit;
            end if;
         end loop;
         Assert(Different, "Different seeds produce different outputs");
      end;

      --  Test 4: Same seed produces same output (deterministic)
      declare
         Output2 : SHA3_512_Digest;
         Match : Boolean := True;
      begin
         G_Expand(Seed, K_Val, Output2);
         for I in Output'Range loop
            if Output(I) /= Output2(I) then
               Match := False;
               exit;
            end if;
         end loop;
         Assert(Match, "Same seed produces identical output (deterministic)");
      end;

      --  Print first few bytes for visual inspection
      Put_Line("Sample output (first 16 bytes):");
      Print_Hex("  ", Output(1 .. 16));
      New_Line;
   end Test_G_Expand;

   --  ========================================================================
   --  Test H_Hash (SHA3-256)
   --  ========================================================================

   procedure Test_H_Hash is
      Input  : Byte_Array(1 .. 100) := (others => 42);
      Output : SHA3_256_Digest;
   begin
      Put_Line("========================================");
      Put_Line("Test H_Hash (SHA3-256)");
      Put_Line("========================================");

      --  Test 1: Basic call
      H_Hash(Input, Output);
      Assert(Output'Length = 32, "H_Hash produces 32-byte output");

      --  Test 2: Output is not all zeros
      declare
         All_Zero : Boolean := True;
      begin
         for I in Output'Range loop
            if Output(I) /= 0 then
               All_Zero := False;
               exit;
            end if;
         end loop;
         Assert(not All_Zero, "H_Hash output is not all zeros");
      end;

      --  Test 3: Different inputs produce different outputs
      declare
         Input2  : Byte_Array(1 .. 100) := (others => 0);
         Output2 : SHA3_256_Digest;
         Different : Boolean := False;
      begin
         H_Hash(Input2, Output2);
         for I in Output'Range loop
            if Output(I) /= Output2(I) then
               Different := True;
               exit;
            end if;
         end loop;
         Assert(Different, "Different inputs produce different outputs");
      end;

      --  Test 4: Same input produces same output (deterministic)
      declare
         Output2 : SHA3_256_Digest;
         Match : Boolean := True;
      begin
         H_Hash(Input, Output2);
         for I in Output'Range loop
            if Output(I) /= Output2(I) then
               Match := False;
               exit;
            end if;
         end loop;
         Assert(Match, "Same input produces identical output (deterministic)");
      end;

      --  Print first few bytes for visual inspection
      Put_Line("Sample output (first 16 bytes):");
      Print_Hex("  ", Output(1 .. 16));
      New_Line;
   end Test_H_Hash;

   --  ========================================================================
   --  Test Known Vectors (if available)
   --  ========================================================================

   procedure Test_Known_Vectors is
   begin
      Put_Line("========================================");
      Put_Line("Test Known Vectors");
      Put_Line("========================================");

      --  TODO: Add NIST FIPS 202 test vectors for SHA3-512 and SHA3-256
      --  For now, just document that we need to add these

      Put_Line("NIST FIPS 202 test vectors:");
      Put_Line("  TODO: Implement SHA3-512 known answer tests");
      Put_Line("  TODO: Implement SHA3-256 known answer tests");
      Put_Line("  See: https://csrc.nist.gov/Projects/cryptographic-algorithm-validation-program");
      New_Line;
   end Test_Known_Vectors;

begin
   Put_Line("========================================");
   Put_Line("ML-KEM Hash Functions Test Suite");
   Put_Line("========================================");
   Put_Line("Testing SHA3-512 (G) and SHA3-256 (H)");
   New_Line;

   Test_G_Expand;
   Test_H_Hash;
   Test_Known_Vectors;

   --  Print summary
   Put_Line("========================================");
   Put_Line("Test Summary");
   Put_Line("========================================");
   Put_Line("Total tests:  " & Natural'Image(Tests_Run));
   Put_Line("Passed:       " & Natural'Image(Tests_Passed));
   Put_Line("Failed:       " & Natural'Image(Tests_Failed));

   if Tests_Failed = 0 then
      Put_Line("Result: ALL TESTS PASSED ✓");
   else
      Put_Line("Result: SOME TESTS FAILED ✗");
   end if;

   Put_Line("========================================");

end Test_MLKEM_Hash;
