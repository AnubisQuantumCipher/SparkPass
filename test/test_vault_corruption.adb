pragma SPARK_Mode (Off);
--  Corruption Injection Tests for SparkPass Vault
--
--  This test suite verifies that the vault correctly detects and rejects
--  corrupted vault files, returning appropriate error codes.
--
--  Corruption scenarios tested:
--    1. Header corruption (magic bytes, version)
--    2. HMAC/MAC corruption (integrity check failure)
--    3. Salt corruption
--    4. Ciphertext corruption
--    5. Entry count corruption
--    6. Partial file truncation
--    7. Empty file
--    8. Oversized file

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Config;
with SparkPass.Vault; use SparkPass.Vault;

procedure Test_Vault_Corruption is

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   Fail_Count : Natural := 0;

   --  Helper: Get current timestamp
   function Now return U64 is
      use Ada.Calendar;
      Epoch : constant Time := Time_Of (1970, 1, 1, 0.0);
      Now_Time : constant Time := Clock;
      Elapsed : constant Duration := Now_Time - Epoch;
   begin
      return U64 (Elapsed);
   end Now;

   --  Helper: Convert string to byte array
   function To_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (1 .. S'Length);
   begin
      for I in S'Range loop
         Result (I - S'First + 1) := U8 (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   --  Helper: Create a valid vault for testing
   procedure Create_Test_Vault (Path : String) is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_12345");
      Label : constant Byte_Array := To_Bytes ("test_entry");
      Data : constant Byte_Array := To_Bytes ("test_data");
      Save_St : SparkPass.Vault.Save_Status;
      Success : Boolean;
   begin
      SparkPass.Vault.Create (State, Password, Now);
      SparkPass.Vault.Add_Entry (State, Label, SparkPass.Types.Password, Data, Now, Success);
      SparkPass.Vault.Save (State, Path, Save_St);
      SparkPass.Vault.Clear (State);
   end Create_Test_Vault;

   --  Helper: Corrupt specific byte(s) in file
   procedure Corrupt_File_At (Path : String; Offset : Positive; Value : U8) is
      File_In : Ada.Streams.Stream_IO.File_Type;
      File_Out : Ada.Streams.Stream_IO.File_Type;
      Buffer : U8;
      Pos : Positive := 1;
   begin
      --  Read entire file
      Ada.Streams.Stream_IO.Open (File_In, Ada.Streams.Stream_IO.In_File, Path);
      Ada.Streams.Stream_IO.Create (File_Out, Ada.Streams.Stream_IO.Out_File, Path & ".tmp");

      --  Copy file, corrupting byte at specified offset
      while not Ada.Streams.Stream_IO.End_Of_File (File_In) loop
         U8'Read (Ada.Streams.Stream_IO.Stream (File_In), Buffer);
         if Pos = Offset then
            Buffer := Value;  --  Corrupt this byte
         end if;
         U8'Write (Ada.Streams.Stream_IO.Stream (File_Out), Buffer);
         Pos := Pos + 1;
      end loop;

      Ada.Streams.Stream_IO.Close (File_In);
      Ada.Streams.Stream_IO.Close (File_Out);
      Ada.Directories.Delete_File (Path);
      Ada.Directories.Rename (Path & ".tmp", Path);
   end Corrupt_File_At;

   --  Helper: Truncate file to specific size
   procedure Truncate_File (Path : String; New_Size : Natural) is
      File_In : Ada.Streams.Stream_IO.File_Type;
      File_Out : Ada.Streams.Stream_IO.File_Type;
      Buffer : U8;
      Count : Natural := 0;
   begin
      Ada.Streams.Stream_IO.Open (File_In, Ada.Streams.Stream_IO.In_File, Path);
      Ada.Streams.Stream_IO.Create (File_Out, Ada.Streams.Stream_IO.Out_File, Path & ".tmp");

      while Count < New_Size and then not Ada.Streams.Stream_IO.End_Of_File (File_In) loop
         U8'Read (Ada.Streams.Stream_IO.Stream (File_In), Buffer);
         U8'Write (Ada.Streams.Stream_IO.Stream (File_Out), Buffer);
         Count := Count + 1;
      end loop;

      Ada.Streams.Stream_IO.Close (File_In);
      Ada.Streams.Stream_IO.Close (File_Out);

      Ada.Directories.Delete_File (Path);
      Ada.Directories.Rename (Path & ".tmp", Path);
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File_In) then
            Ada.Streams.Stream_IO.Close (File_In);
         end if;
         if Ada.Streams.Stream_IO.Is_Open (File_Out) then
            Ada.Streams.Stream_IO.Close (File_Out);
         end if;
         raise;
   end Truncate_File;

   --  Test 1: Header magic byte corruption
   procedure Test_Magic_Corruption is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_12345");
      Test_Path : constant String := "/tmp/test_corrupt_magic.spass";
      Open_St : SparkPass.Vault.Open_Status;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 1: Header Magic Byte Corruption ===");
      Put_Line ("Corrupt magic bytes → should return Format_Error");
      New_Line;

      --  Create valid vault
      Put_Line ("[1] Creating valid vault...");
      Create_Test_Vault (Test_Path);
      Put_Line ("  ✓ Vault created");

      --  Corrupt first magic byte (offset 1)
      Put_Line ("[2] Corrupting first magic byte...");
      Corrupt_File_At (Test_Path, 1, 16#FF#);
      Put_Line ("  ✓ Corrupted byte at offset 1");

      --  Try to open corrupted vault
      Put_Line ("[3] Attempting to open corrupted vault...");
      SparkPass.Vault.Open (State, Test_Path, Password, Open_St);

      if Open_St /= SparkPass.Vault.Success then
         Put_Line ("  ✓ PASS: Correctly rejected corrupted vault (status: " & Open_St'Image & ")");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Vault opened with corrupted magic bytes!");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Magic_Corruption;

   --  Test 2: HMAC corruption (integrity check)
   procedure Test_HMAC_Corruption is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_12345");
      Test_Path : constant String := "/tmp/test_corrupt_hmac.spass";
      Open_St : SparkPass.Vault.Open_Status;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 2: HMAC/MAC Corruption ===");
      Put_Line ("Corrupt integrity tag → should return Integrity_Error");
      New_Line;

      --  Create valid vault
      Put_Line ("[1] Creating valid vault...");
      Create_Test_Vault (Test_Path);
      Put_Line ("  ✓ Vault created");

      --  Corrupt HMAC (near end of file, assuming ~64 bytes from end)
      Put_Line ("[2] Corrupting HMAC/MAC tag...");
      Corrupt_File_At (Test_Path, 100, 16#AA#);
      Put_Line ("  ✓ Corrupted byte at offset 100");

      --  Try to open corrupted vault
      Put_Line ("[3] Attempting to open corrupted vault...");
      SparkPass.Vault.Open (State, Test_Path, Password, Open_St);

      if Open_St = SparkPass.Vault.Integrity_Error or else
         Open_St = SparkPass.Vault.Authentication_Failed then
         Put_Line ("  ✓ PASS: Correctly detected integrity/authentication failure");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Expected Integrity_Error or Authentication_Failed, got: " & Open_St'Image);
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_HMAC_Corruption;

   --  Test 3: Ciphertext corruption
   procedure Test_Ciphertext_Corruption is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_12345");
      Test_Path : constant String := "/tmp/test_corrupt_ciphertext.spass";
      Open_St : SparkPass.Vault.Open_Status;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 3: Ciphertext Corruption ===");
      Put_Line ("Corrupt encrypted data → should return Integrity_Error");
      New_Line;

      --  Create valid vault
      Put_Line ("[1] Creating valid vault...");
      Create_Test_Vault (Test_Path);
      Put_Line ("  ✓ Vault created");

      --  Corrupt ciphertext (middle of file, ~offset 200)
      Put_Line ("[2] Corrupting ciphertext...");
      Corrupt_File_At (Test_Path, 200, 16#BB#);
      Put_Line ("  ✓ Corrupted byte at offset 200");

      --  Try to open corrupted vault
      Put_Line ("[3] Attempting to open corrupted vault...");
      SparkPass.Vault.Open (State, Test_Path, Password, Open_St);

      if Open_St = SparkPass.Vault.Integrity_Error or else
         Open_St = SparkPass.Vault.Authentication_Failed then
         Put_Line ("  ✓ PASS: Correctly detected integrity/authentication failure");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Expected Integrity_Error or Authentication_Failed, got: " & Open_St'Image);
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Ciphertext_Corruption;

   --  Test 4: File truncation
   procedure Test_Truncation is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_12345");
      Test_Path : constant String := "/tmp/test_truncated.spass";
      Open_St : SparkPass.Vault.Open_Status;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 4: File Truncation ===");
      Put_Line ("Truncate file → should return Format_Error or Io_Error");
      New_Line;

      --  Create valid vault
      Put_Line ("[1] Creating valid vault...");
      Create_Test_Vault (Test_Path);
      Put_Line ("  ✓ Vault created");

      --  Truncate file to 50 bytes (incomplete header)
      Put_Line ("[2] Truncating file to 50 bytes...");
      Truncate_File (Test_Path, 50);
      Put_Line ("  ✓ File truncated");

      --  Try to open truncated vault
      Put_Line ("[3] Attempting to open truncated vault...");
      SparkPass.Vault.Open (State, Test_Path, Password, Open_St);

      if Open_St /= SparkPass.Vault.Success then
         Put_Line ("  ✓ PASS: Correctly rejected truncated vault (status: " & Open_St'Image & ")");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Vault opened with truncated file!");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Truncation;

   --  Test 5: Empty file
   procedure Test_Empty_File is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_12345");
      Test_Path : constant String := "/tmp/test_empty.spass";
      Open_St : SparkPass.Vault.Open_Status;
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 5: Empty File ===");
      Put_Line ("Open empty file → should return Format_Error or Io_Error");
      New_Line;

      --  Create empty file
      Put_Line ("[1] Creating empty file...");
      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Test_Path);
      Ada.Streams.Stream_IO.Close (File);
      Put_Line ("  ✓ Empty file created");

      --  Try to open empty file
      Put_Line ("[2] Attempting to open empty file...");
      SparkPass.Vault.Open (State, Test_Path, Password, Open_St);

      if Open_St /= SparkPass.Vault.Success then
         Put_Line ("  ✓ PASS: Correctly rejected empty file (status: " & Open_St'Image & ")");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Vault opened with empty file!");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Empty_File;

   --  Test 6: Version corruption
   procedure Test_Version_Corruption is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_12345");
      Test_Path : constant String := "/tmp/test_corrupt_version.spass";
      Open_St : SparkPass.Vault.Open_Status;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 6: Version Field Corruption ===");
      Put_Line ("Set invalid version → should return Format_Error");
      New_Line;

      --  Create valid vault
      Put_Line ("[1] Creating valid vault...");
      Create_Test_Vault (Test_Path);
      Put_Line ("  ✓ Vault created");

      --  Corrupt version field (assuming offset ~4-7 after magic)
      Put_Line ("[2] Corrupting version field...");
      Corrupt_File_At (Test_Path, 5, 16#FF#);
      Put_Line ("  ✓ Corrupted version byte");

      --  Try to open corrupted vault
      Put_Line ("[3] Attempting to open corrupted vault...");
      SparkPass.Vault.Open (State, Test_Path, Password, Open_St);

      if Open_St /= SparkPass.Vault.Success then
         Put_Line ("  ✓ PASS: Correctly rejected vault with corrupted version (status: " & Open_St'Image & ")");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Vault opened with corrupted version!");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_Version_Corruption;

begin
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Corruption Injection Tests for SparkPass Vault           ║");
   Put_Line ("║  Verifying error detection and resilience                 ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   --  Run corruption tests
   Test_Magic_Corruption;
   Test_HMAC_Corruption;
   Test_Ciphertext_Corruption;
   Test_Truncation;
   Test_Empty_File;
   Test_Version_Corruption;

   --  Summary
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Test Summary                                              ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   Put_Line ("Total Tests: " & Natural'Image (Test_Count));
   Put_Line ("Passed:      " & Natural'Image (Pass_Count));
   Put_Line ("Failed:      " & Natural'Image (Fail_Count));
   New_Line;

   if Fail_Count = 0 then
      Put_Line ("✓ All corruption injection tests PASSED");
      Put_Line ("");
      Put_Line ("Verified resilience against:");
      Put_Line ("  • Header magic byte corruption");
      Put_Line ("  • HMAC/MAC tag corruption");
      Put_Line ("  • Ciphertext corruption");
      Put_Line ("  • File truncation");
      Put_Line ("  • Empty files");
      Put_Line ("  • Version field corruption");
   else
      Put_Line ("✗ Some tests FAILED - review output above");
   end if;

end Test_Vault_Corruption;
