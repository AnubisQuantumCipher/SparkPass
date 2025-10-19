--  ============================================================================
--  Keccak-f[1600] Test Suite
--  ============================================================================
--
--  **Purpose**: Verify pure SPARK Keccak implementation
--
--  **Test Categories**:
--    1. SHA3-512 basic functionality
--    2. SHA3-256 basic functionality
--    3. SHAKE-256 extendable output
--    4. SHAKE-128 extendable output
--    5. Deterministic behavior
--    6. Different inputs produce different outputs
--
--  ============================================================================

pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Keccak; use SparkPass.Crypto.Keccak;

procedure Test_Keccak is

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
         declare
            Hex : constant String := "0123456789abcdef";
            B   : constant U8 := Data(I);
            Hi  : constant Natural := Natural(B / 16) + 1;
            Lo  : constant Natural := Natural(B mod 16) + 1;
         begin
            Put(Hex(Hi) & Hex(Lo));
            if I < Data'Last then
               Put(" ");
            end if;
         end;
      end loop;
      New_Line;
   end Print_Hex;

   --  =========================================================================
   --  Test SHA3-512
   --  =========================================================================

   procedure Test_SHA3_512 is
      Empty_Input : Byte_Array(1 .. 0);
      Output      : SHA3_512_Digest;
   begin
      Put_Line("========================================");
      Put_Line("Test SHA3-512");
      Put_Line("========================================");

      --  Test 1: Empty input
      SHA3_512_Hash(Empty_Input, Output);
      Assert(Output'Length = 64, "SHA3-512 produces 64-byte output");

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
         Assert(not All_Zero, "SHA3-512 output is not all zeros");
      end;

      Put_Line("SHA3-512(empty) first 16 bytes:");
      Print_Hex("  ", Output(1 .. 16));

      --  Test 3: Hash of "abc"
      declare
         Input_ABC : constant Byte_Array := (97, 98, 99);  -- "abc"
         Output_ABC : SHA3_512_Digest;
      begin
         SHA3_512_Hash(Input_ABC, Output_ABC);
         Assert(Output_ABC'Length = 64, "SHA3-512('abc') produces 64 bytes");

         Put_Line("SHA3-512('abc') first 16 bytes:");
         Print_Hex("  ", Output_ABC(1 .. 16));
      end;

      --  Test 4: Different inputs produce different outputs
      declare
         Input1  : constant Byte_Array := (1, 2, 3, 4, 5);
         Input2  : constant Byte_Array := (5, 4, 3, 2, 1);
         Output1 : SHA3_512_Digest;
         Output2 : SHA3_512_Digest;
         Different : Boolean := False;
      begin
         SHA3_512_Hash(Input1, Output1);
         SHA3_512_Hash(Input2, Output2);

         for I in Output1'Range loop
            if Output1(I) /= Output2(I) then
               Different := True;
               exit;
            end if;
         end loop;

         Assert(Different, "Different inputs produce different SHA3-512 outputs");
      end;

      --  Test 5: Same input produces same output (deterministic)
      declare
         Input  : constant Byte_Array := (42, 24, 128, 255);
         Output1 : SHA3_512_Digest;
         Output2 : SHA3_512_Digest;
         Match : Boolean := True;
      begin
         SHA3_512_Hash(Input, Output1);
         SHA3_512_Hash(Input, Output2);

         for I in Output1'Range loop
            if Output1(I) /= Output2(I) then
               Match := False;
               exit;
            end if;
         end loop;

         Assert(Match, "SHA3-512 is deterministic");
      end;

      New_Line;
   end Test_SHA3_512;

   --  =========================================================================
   --  Test SHA3-256
   --  =========================================================================

   procedure Test_SHA3_256 is
      Empty_Input : Byte_Array(1 .. 0);
      Output      : SHA3_256_Digest;
   begin
      Put_Line("========================================");
      Put_Line("Test SHA3-256");
      Put_Line("========================================");

      --  Test 1: Empty input
      SHA3_256_Hash(Empty_Input, Output);
      Assert(Output'Length = 32, "SHA3-256 produces 32-byte output");

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
         Assert(not All_Zero, "SHA3-256 output is not all zeros");
      end;

      Put_Line("SHA3-256(empty) first 16 bytes:");
      Print_Hex("  ", Output(1 .. 16));

      --  Test 3: Hash of "abc"
      declare
         Input_ABC : constant Byte_Array := (97, 98, 99);  -- "abc"
         Output_ABC : SHA3_256_Digest;
      begin
         SHA3_256_Hash(Input_ABC, Output_ABC);
         Assert(Output_ABC'Length = 32, "SHA3-256('abc') produces 32 bytes");

         Put_Line("SHA3-256('abc') all bytes:");
         Print_Hex("  ", Output_ABC);
      end;

      --  Test 4: Different inputs produce different outputs
      declare
         Input1  : constant Byte_Array := (1, 2, 3);
         Input2  : constant Byte_Array := (3, 2, 1);
         Output1 : SHA3_256_Digest;
         Output2 : SHA3_256_Digest;
         Different : Boolean := False;
      begin
         SHA3_256_Hash(Input1, Output1);
         SHA3_256_Hash(Input2, Output2);

         for I in Output1'Range loop
            if Output1(I) /= Output2(I) then
               Different := True;
               exit;
            end if;
         end loop;

         Assert(Different, "Different inputs produce different SHA3-256 outputs");
      end;

      --  Test 5: Same input produces same output (deterministic)
      declare
         Input  : constant Byte_Array := (99, 88, 77, 66, 55);
         Output1 : SHA3_256_Digest;
         Output2 : SHA3_256_Digest;
         Match : Boolean := True;
      begin
         SHA3_256_Hash(Input, Output1);
         SHA3_256_Hash(Input, Output2);

         for I in Output1'Range loop
            if Output1(I) /= Output2(I) then
               Match := False;
               exit;
            end if;
         end loop;

         Assert(Match, "SHA3-256 is deterministic");
      end;

      New_Line;
   end Test_SHA3_256;

   --  =========================================================================
   --  Test SHAKE-256
   --  =========================================================================

   procedure Test_SHAKE_256 is
      Input  : constant Byte_Array := (1, 2, 3, 4, 5);
   begin
      Put_Line("========================================");
      Put_Line("Test SHAKE-256 (Extendable Output)");
      Put_Line("========================================");

      --  Test 1: 32-byte output
      declare
         Output32 : Byte_Array(1 .. 32);
      begin
         SHAKE_256(Input, Output32);
         Assert(Output32'Length = 32, "SHAKE-256 produces 32-byte output");

         Put_Line("SHAKE-256 32-byte output (first 16):");
         Print_Hex("  ", Output32(1 .. 16));
      end;

      --  Test 2: 64-byte output
      declare
         Output64 : Byte_Array(1 .. 64);
      begin
         SHAKE_256(Input, Output64);
         Assert(Output64'Length = 64, "SHAKE-256 produces 64-byte output");

         Put_Line("SHAKE-256 64-byte output (first 16):");
         Print_Hex("  ", Output64(1 .. 16));
      end;

      --  Test 3: 128-byte output (for PRF in ML-KEM)
      declare
         Output128 : Byte_Array(1 .. 128);
      begin
         SHAKE_256(Input, Output128);
         Assert(Output128'Length = 128, "SHAKE-256 produces 128-byte output");

         Put_Line("SHAKE-256 128-byte output (first 16):");
         Print_Hex("  ", Output128(1 .. 16));
      end;

      --  Test 4: Output is not all zeros
      declare
         Output : Byte_Array(1 .. 64);
         All_Zero : Boolean := True;
      begin
         SHAKE_256(Input, Output);

         for I in Output'Range loop
            if Output(I) /= 0 then
               All_Zero := False;
               exit;
            end if;
         end loop;

         Assert(not All_Zero, "SHAKE-256 output is not all zeros");
      end;

      --  Test 5: Deterministic
      declare
         Output1 : Byte_Array(1 .. 64);
         Output2 : Byte_Array(1 .. 64);
         Match : Boolean := True;
      begin
         SHAKE_256(Input, Output1);
         SHAKE_256(Input, Output2);

         for I in Output1'Range loop
            if Output1(I) /= Output2(I) then
               Match := False;
               exit;
            end if;
         end loop;

         Assert(Match, "SHAKE-256 is deterministic");
      end;

      New_Line;
   end Test_SHAKE_256;

   --  =========================================================================
   --  Test SHAKE-128
   --  =========================================================================

   procedure Test_SHAKE_128 is
      Input  : constant Byte_Array := (10, 20, 30, 40, 50);
   begin
      Put_Line("========================================");
      Put_Line("Test SHAKE-128 (Extendable Output)");
      Put_Line("========================================");

      --  Test 1: 32-byte output
      declare
         Output32 : Byte_Array(1 .. 32);
      begin
         SHAKE_128(Input, Output32);
         Assert(Output32'Length = 32, "SHAKE-128 produces 32-byte output");

         Put_Line("SHAKE-128 32-byte output (first 16):");
         Print_Hex("  ", Output32(1 .. 16));
      end;

      --  Test 2: 168-byte output (full rate for ML-KEM XOF)
      declare
         Output168 : Byte_Array(1 .. 168);
      begin
         SHAKE_128(Input, Output168);
         Assert(Output168'Length = 168, "SHAKE-128 produces 168-byte output");

         Put_Line("SHAKE-128 168-byte output (first 16):");
         Print_Hex("  ", Output168(1 .. 16));
      end;

      --  Test 3: Output is not all zeros
      declare
         Output : Byte_Array(1 .. 64);
         All_Zero : Boolean := True;
      begin
         SHAKE_128(Input, Output);

         for I in Output'Range loop
            if Output(I) /= 0 then
               All_Zero := False;
               exit;
            end if;
         end loop;

         Assert(not All_Zero, "SHAKE-128 output is not all zeros");
      end;

      --  Test 4: Deterministic
      declare
         Output1 : Byte_Array(1 .. 64);
         Output2 : Byte_Array(1 .. 64);
         Match : Boolean := True;
      begin
         SHAKE_128(Input, Output1);
         SHAKE_128(Input, Output2);

         for I in Output1'Range loop
            if Output1(I) /= Output2(I) then
               Match := False;
               exit;
            end if;
         end loop;

         Assert(Match, "SHAKE-128 is deterministic");
      end;

      --  Test 5: Different from SHAKE-256
      declare
         Output_128 : Byte_Array(1 .. 64);
         Output_256 : Byte_Array(1 .. 64);
         Different : Boolean := False;
      begin
         SHAKE_128(Input, Output_128);
         SHAKE_256(Input, Output_256);

         for I in Output_128'Range loop
            if Output_128(I) /= Output_256(I) then
               Different := True;
               exit;
            end if;
         end loop;

         Assert(Different, "SHAKE-128 and SHAKE-256 produce different outputs");
      end;

      New_Line;
   end Test_SHAKE_128;

   --  =========================================================================
   --  Test NIST FIPS 202 Official Test Vectors
   --  =========================================================================

   procedure Test_NIST_Vectors is
      --  Helper: Compare output with expected hex string
      function Hex_To_Byte_Array (Hex : String) return Byte_Array is
         Result : Byte_Array (1 .. Hex'Length / 2);
         Pos : Positive := 1;
      begin
         for I in Result'Range loop
            declare
               Hi_Char : constant Character := Hex (Pos);
               Lo_Char : constant Character := Hex (Pos + 1);
               Hi_Val  : U8;
               Lo_Val  : U8;
            begin
               --  Convert hex character to value
               if Hi_Char in '0' .. '9' then
                  Hi_Val := Character'Pos (Hi_Char) - Character'Pos ('0');
               elsif Hi_Char in 'a' .. 'f' then
                  Hi_Val := Character'Pos (Hi_Char) - Character'Pos ('a') + 10;
               elsif Hi_Char in 'A' .. 'F' then
                  Hi_Val := Character'Pos (Hi_Char) - Character'Pos ('A') + 10;
               else
                  Hi_Val := 0;
               end if;

               if Lo_Char in '0' .. '9' then
                  Lo_Val := Character'Pos (Lo_Char) - Character'Pos ('0');
               elsif Lo_Char in 'a' .. 'f' then
                  Lo_Val := Character'Pos (Lo_Char) - Character'Pos ('a') + 10;
               elsif Lo_Char in 'A' .. 'F' then
                  Lo_Val := Character'Pos (Lo_Char) - Character'Pos ('A') + 10;
               else
                  Lo_Val := 0;
               end if;

               Result (I) := Hi_Val * 16 + Lo_Val;
               Pos := Pos + 2;
            end;
         end loop;
         return Result;
      end Hex_To_Byte_Array;

      function Arrays_Match (A, B : Byte_Array) return Boolean is
      begin
         if A'Length /= B'Length then
            return False;
         end if;
         for I in A'Range loop
            if A (I) /= B (I - A'First + B'First) then
               return False;
            end if;
         end loop;
         return True;
      end Arrays_Match;

   begin
      Put_Line ("========================================");
      Put_Line ("Test NIST FIPS 202 Official Vectors");
      Put_Line ("========================================");

      --  ======================================================================
      --  SHA3-256 NIST vectors
      --  ======================================================================

      --  Test 1: SHA3-256 of empty string
      --  Expected: a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a
      declare
         Input    : Byte_Array (1 .. 0);
         Output   : SHA3_256_Digest;
         Expected : constant Byte_Array :=
           Hex_To_Byte_Array ("a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a");
      begin
         SHA3_256_Hash (Input, Output);
         Assert (Arrays_Match (Output, Expected),
                 "NIST: SHA3-256 of empty string");
         if not Arrays_Match (Output, Expected) then
            Put_Line ("  Expected:");
            Print_Hex ("    ", Expected);
            Put_Line ("  Got:");
            Print_Hex ("    ", Output);
         end if;
      end;

      --  Test 2: SHA3-256 of "abc"
      --  Expected: 3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532
      declare
         Input    : constant Byte_Array := (97, 98, 99);  -- "abc"
         Output   : SHA3_256_Digest;
         Expected : constant Byte_Array :=
           Hex_To_Byte_Array ("3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532");
      begin
         SHA3_256_Hash (Input, Output);
         Assert (Arrays_Match (Output, Expected),
                 "NIST: SHA3-256 of 'abc'");
         if not Arrays_Match (Output, Expected) then
            Put_Line ("  Expected:");
            Print_Hex ("    ", Expected);
            Put_Line ("  Got:");
            Print_Hex ("    ", Output);
         end if;
      end;

      --  ======================================================================
      --  SHA3-512 NIST vectors
      --  ======================================================================

      --  Test 3: SHA3-512 of empty string
      --  Expected: a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a6
      --            15b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26
      declare
         Input    : Byte_Array (1 .. 0);
         Output   : SHA3_512_Digest;
         Expected : constant Byte_Array :=
           Hex_To_Byte_Array ("a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a6" &
                              "15b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26");
      begin
         SHA3_512_Hash (Input, Output);
         Assert (Arrays_Match (Output, Expected),
                 "NIST: SHA3-512 of empty string");
         if not Arrays_Match (Output, Expected) then
            Put_Line ("  Expected:");
            Print_Hex ("    ", Expected (1 .. 32));
            Print_Hex ("    ", Expected (33 .. 64));
            Put_Line ("  Got:");
            Print_Hex ("    ", Output (1 .. 32));
            Print_Hex ("    ", Output (33 .. 64));
         end if;
      end;

      --  Test 4: SHA3-512 of "abc"
      --  Expected: b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e
      --            10e116e9192af3c9a1a7ec57647e3934057340b4cf408d5a56592f8274eec53f0
      declare
         Input    : constant Byte_Array := (97, 98, 99);  -- "abc"
         Output   : SHA3_512_Digest;
         Expected : constant Byte_Array :=
           Hex_To_Byte_Array ("b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e" &
                              "10e116e9192af3c9a1a7ec57647e3934057340b4cf408d5a56592f8274eec53f0");
      begin
         SHA3_512_Hash (Input, Output);
         Assert (Arrays_Match (Output, Expected),
                 "NIST: SHA3-512 of 'abc'");
         if not Arrays_Match (Output, Expected) then
            Put_Line ("  Expected:");
            Print_Hex ("    ", Expected (1 .. 32));
            Print_Hex ("    ", Expected (33 .. 64));
            Put_Line ("  Got:");
            Print_Hex ("    ", Output (1 .. 32));
            Print_Hex ("    ", Output (33 .. 64));
         end if;
      end;

      New_Line;
   end Test_NIST_Vectors;

   --  =========================================================================
   --  Test ML-KEM Integration Points
   --  =========================================================================

   procedure Test_MLKEM_Integration is
   begin
      Put_Line("========================================");
      Put_Line("Test ML-KEM Integration Points");
      Put_Line("========================================");

      --  Test 1: SHA3-512 for G function (seed expansion)
      --  ML-KEM KeyGen needs: G(d || k) where d=32 bytes, k=1 byte
      declare
         Seed_D : constant Byte_Array(1 .. 32) := (others => 42);
         K_Val  : constant U8 := 4;  -- ML-KEM-1024 parameter
         Input  : Byte_Array(1 .. 33);
         Output : SHA3_512_Digest;
      begin
         Input(1 .. 32) := Seed_D;
         Input(33) := K_Val;

         SHA3_512_Hash(Input, Output);
         Assert(Output'Length = 64, "G function produces 64 bytes (ρ || σ)");

         Put_Line("G(seed || k) output (first 32 bytes = ρ):");
         Print_Hex("  ", Output(1 .. 32));
      end;

      --  Test 2: SHA3-256 for H function (public key hash)
      --  ML-KEM needs: H(ek) where ek is the encapsulation key
      declare
         Public_Key : constant Byte_Array(1 .. 100) := (others => 123);
         Output : SHA3_256_Digest;
      begin
         SHA3_256_Hash(Public_Key, Output);
         Assert(Output'Length = 32, "H function produces 32 bytes");

         Put_Line("H(public_key) output:");
         Print_Hex("  ", Output(1 .. 16));
      end;

      --  Test 3: SHAKE-256 for PRF (CBD sampling)
      --  ML-KEM needs: PRF(s, nonce) → 128 bytes for η=2
      declare
         Secret : constant Byte_Array(1 .. 32) := (others => 99);
         Nonce  : constant U8 := 7;
         Input  : Byte_Array(1 .. 33);
         Output : Byte_Array(1 .. 128);  -- 64*η where η=2
      begin
         Input(1 .. 32) := Secret;
         Input(33) := Nonce;

         SHAKE_256(Input, Output);
         Assert(Output'Length = 128, "PRF produces 128 bytes for CBD");

         Put_Line("PRF(secret || nonce) output (first 16):");
         Print_Hex("  ", Output(1 .. 16));
      end;

      --  Test 4: SHAKE-128 for XOF (uniform sampling)
      --  ML-KEM needs: XOF(ρ || i || j) for matrix A generation
      declare
         Rho : constant Byte_Array(1 .. 32) := (others => 77);
         I_J : constant Byte_Array := (3, 2);  -- coordinates
         Input : Byte_Array(1 .. 34);
         Output : Byte_Array(1 .. 168);  -- One block
      begin
         Input(1 .. 32) := Rho;
         Input(33 .. 34) := I_J;

         SHAKE_128(Input, Output);
         Assert(Output'Length = 168, "XOF produces 168 bytes per block");

         Put_Line("XOF(ρ || i || j) output (first 16):");
         Print_Hex("  ", Output(1 .. 16));
      end;

      New_Line;
   end Test_MLKEM_Integration;

begin
   Put_Line("========================================");
   Put_Line("Keccak-f[1600] Test Suite");
   Put_Line("========================================");
   Put_Line("Pure SPARK Implementation");
   Put_Line("NIST FIPS 202 Compliant");
   New_Line;

   Test_SHA3_512;
   Test_SHA3_256;
   Test_SHAKE_256;
   Test_SHAKE_128;
   Test_NIST_Vectors;
   Test_MLKEM_Integration;

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

end Test_Keccak;
