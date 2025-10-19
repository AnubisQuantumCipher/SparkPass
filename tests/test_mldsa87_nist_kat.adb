pragma SPARK_Mode (Off);  -- File I/O not supported in SPARK
--  NIST ML-DSA-87 Known Answer Test (KAT) Validator
--
--  This test harness validates the SparkPass pure SPARK ML-DSA-87
--  implementation against official NIST test vectors.
--
--  Test Vector Source:
--    https://github.com/itzmeanjan/d14afc3866b82119221682f0f3c9822d
--    (ML-DSA-87 deterministic KATs)
--
--  Usage:
--    ./test_mldsa87_nist_kat tests/vectors/ml_dsa_87_kat.txt

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLDSA; use SparkPass.Crypto.MLDSA;
with Test_Hex_Utils; use Test_Hex_Utils;

procedure Test_Mldsa87_Nist_KAT is

   --  Test statistics
   Tests_Run : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   --  Test vector record
   type KAT_Vector is record
      Seed      : Byte_Array (1 .. 32);              -- Random seed
      PKey      : MLDsa_Public_Key_Array;             -- Expected public key
      SKey      : MLDsa_Secret_Key_Array;             -- Expected secret key
      Msg_Len   : Natural;                            -- Message length
      Msg       : Byte_Array (1 .. 65536);            -- Message (variable length)
      Sig       : MLDsa_Signature_Array;              -- Expected signature
      Valid     : Boolean := False;                   -- Vector loaded successfully
   end record;

   --  Parse a single KAT test vector from file
   procedure Parse_KAT_Vector
     (File   : in out File_Type;
      Vector : out KAT_Vector;
      EOF    : out Boolean)
   is
      Line_Str : String (1 .. 100000);  -- Large buffer for long hex strings
      Line_Last : Natural;
   begin
      EOF := False;
      Vector.Valid := False;
      Vector.Msg_Len := 0;

      --  Read until we find "seed = " or EOF
      while not End_Of_File (File) loop
         Get_Line (File, Line_Str, Line_Last);

         --  Skip empty lines and comments
         if Line_Last = 0 or else Line_Str (1) = '#' then
            null;

         --  Parse seed
         elsif Line_Last >= 7 and then Line_Str (1 .. 7) = "seed = " then
            declare
               Hex : constant String := Line_Str (8 .. Line_Last);
            begin
               if Hex'Length = 64 then  -- 32 bytes * 2 hex chars
                  Vector.Seed := Hex_String_To_Bytes (Hex);
                  exit;  -- Found start of new vector
               end if;
            end;
         end if;
      end loop;

      if End_Of_File (File) then
         EOF := True;
         return;
      end if;

      --  Read remaining fields for this vector
      while not End_Of_File (File) loop
         Get_Line (File, Line_Str, Line_Last);

         if Line_Last = 0 then
            null;  -- Skip empty lines

         --  Public key
         elsif Line_Last >= 7 and then Line_Str (1 .. 7) = "pkey = " then
            declare
               Hex : constant String := Line_Str (8 .. Line_Last);
            begin
               if Hex'Length = 5184 then  -- 2592 bytes * 2
                  Vector.PKey := Hex_String_To_Bytes (Hex);
               end if;
            end;

         --  Secret key
         elsif Line_Last >= 7 and then Line_Str (1 .. 7) = "skey = " then
            declare
               Hex : constant String := Line_Str (8 .. Line_Last);
            begin
               if Hex'Length = 9792 then  -- 4896 bytes * 2
                  Vector.SKey := Hex_String_To_Bytes (Hex);
               end if;
            end;

         --  Message length
         elsif Line_Last >= 7 and then Line_Str (1 .. 7) = "mlen = " then
            Vector.Msg_Len := Natural (Parse_U32 (Line_Str (8 .. Line_Last)));

         --  Message
         elsif Line_Last >= 6 and then Line_Str (1 .. 6) = "msg = " then
            declare
               Hex : constant String := Line_Str (7 .. Line_Last);
               Msg_Bytes : constant Byte_Array := Hex_String_To_Bytes (Hex);
            begin
               if Msg_Bytes'Length <= Vector.Msg'Length then
                  Vector.Msg (1 .. Msg_Bytes'Length) := Msg_Bytes;
                  Vector.Msg_Len := Msg_Bytes'Length;
               end if;
            end;

         --  Signature
         elsif Line_Last >= 6 and then Line_Str (1 .. 6) = "sig = " then
            declare
               Hex : constant String := Line_Str (7 .. Line_Last);
            begin
               if Hex'Length = 9254 then  -- 4627 bytes * 2
                  Vector.Sig := Hex_String_To_Bytes (Hex);
                  Vector.Valid := True;
                  return;  -- Complete vector parsed
               end if;
            end;

         --  Check if we've hit the next vector's seed (shouldn't happen)
         elsif Line_Last >= 7 and then Line_Str (1 .. 7) = "seed = " then
            return;  -- Start of next vector, stop here
         end if;
      end loop;

   end Parse_KAT_Vector;

   --  Test a single KAT vector
   procedure Test_Vector (Vector : KAT_Vector; Test_Num : Positive) is
      --  Generated keys and signature
      Gen_Public : Public_Key;
      Gen_Secret : Secret_Key;
      Gen_Sig    : Signature;
      Verify_OK  : Boolean;

      Test_Failed : Boolean := False;
   begin
      Tests_Run := Tests_Run + 1;

      Put_Line ("----------------------------------------");
      Put_Line ("Test Vector #" & Positive'Image (Test_Num));
      Put_Line ("----------------------------------------");

      --  Note: Our implementation uses deterministic key generation from random source
      --  The NIST KAT uses a specific seed, but we can't inject that seed directly
      --  into our current API. We'll focus on Sign/Verify testing using the provided keys.

      Put_Line ("  Message Length: " & Natural'Image (Vector.Msg_Len) & " bytes");

      --  Test 1: Verify the signature using the KAT public key
      Put ("  [TEST 1] Signature Verification: ");
      Verify
        (Public  => Vector.PKey,
         Message => Vector.Msg (1 .. Vector.Msg_Len),
         Sig     => Vector.Sig,
         Success => Verify_OK);

      if Verify_OK then
         Put_Line ("PASS");
      else
         Put_Line ("FAIL - Signature verification failed");
         Test_Failed := True;
      end if;

      --  Test 2: Generate a new keypair and test sign/verify roundtrip
      Put ("  [TEST 2] Sign/Verify Roundtrip: ");
      Keypair (Public => Gen_Public, Secret => Gen_Secret);

      --  Sign the message
      Sign
        (Secret  => Gen_Secret,
         Message => Vector.Msg (1 .. Vector.Msg_Len),
         Output  => Gen_Sig);

      --  Verify our own signature
      Verify
        (Public  => Gen_Public,
         Message => Vector.Msg (1 .. Vector.Msg_Len),
         Sig     => Gen_Sig,
         Success => Verify_OK);

      if Verify_OK then
         Put_Line ("PASS");
      else
         Put_Line ("FAIL - Self-generated signature verification failed");
         Test_Failed := True;
      end if;

      --  Test 3: Cross-verification (should fail - different keys)
      Put ("  [TEST 3] Cross-Key Rejection: ");
      Verify
        (Public  => Gen_Public,
         Message => Vector.Msg (1 .. Vector.Msg_Len),
         Sig     => Vector.Sig,
         Success => Verify_OK);

      if not Verify_OK then
         Put_Line ("PASS (correctly rejected)");
      else
         Put_Line ("FAIL - Incorrectly accepted signature from different key");
         Test_Failed := True;
      end if;

      --  Update statistics
      if Test_Failed then
         Tests_Failed := Tests_Failed + 1;
      else
         Tests_Passed := Tests_Passed + 1;
      end if;

   end Test_Vector;

   --  Main program
   File : File_Type;
   Vector : KAT_Vector;
   EOF : Boolean;
   KAT_File_Path : constant String :=
     (if Ada.Command_Line.Argument_Count > 0 then
         Ada.Command_Line.Argument (1)
      else
         "tests/vectors/ml_dsa_87_kat.txt");

begin
   Put_Line ("========================================");
   Put_Line ("  ML-DSA-87 NIST KAT Validation");
   Put_Line ("  SparkPass Pure SPARK Implementation");
   Put_Line ("========================================");
   New_Line;
   Put_Line ("Test Vector File: " & KAT_File_Path);
   New_Line;

   --  Open KAT file
   begin
      Open (File, In_File, KAT_File_Path);
   exception
      when others =>
         Put_Line ("ERROR: Cannot open KAT file: " & KAT_File_Path);
         Put_Line ("Usage: test_mldsa87_nist_kat [path/to/kat_file.txt]");
         return;
   end;

   --  Parse and test each vector
   loop
      Parse_KAT_Vector (File, Vector, EOF);
      exit when EOF;

      if Vector.Valid then
         Test_Vector (Vector, Tests_Run + 1);
         New_Line;
      end if;

      --  Limit to first 10 vectors for initial testing
      exit when Tests_Run >= 10;
   end loop;

   Close (File);

   --  Print summary
   Put_Line ("========================================");
   Put_Line ("  TEST SUMMARY");
   Put_Line ("========================================");
   Put_Line ("  Total Tests:  " & Natural'Image (Tests_Run));
   Put_Line ("  Passed:       " & Natural'Image (Tests_Passed));
   Put_Line ("  Failed:       " & Natural'Image (Tests_Failed));

   if Tests_Failed = 0 and Tests_Run > 0 then
      Put_Line ("  Result:       ALL TESTS PASSED");
   elsif Tests_Failed > 0 then
      Put_Line ("  Result:       SOME TESTS FAILED");
   else
      Put_Line ("  Result:       NO TESTS RUN");
   end if;
   Put_Line ("========================================");

end Test_Mldsa87_Nist_KAT;
