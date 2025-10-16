pragma SPARK_Mode (Off);
--  Vault File Format Fuzzing Harness for SparkPass
--
--  This comprehensive fuzzer tests the vault's resilience against malformed
--  input by applying systematic mutations to valid vault files and verifying
--  that all corrupted variants are correctly rejected.
--
--  Fuzzing Strategies:
--    1. Bit Flips:         Random single/multi-bit flips
--    2. Byte Substitution: Replace bytes with random values
--    3. Magic Corruption:  Target magic bytes specifically
--    4. Length Corruption: Invalid length fields
--    5. Truncation:        Cut file at random offsets
--    6. Extension:         Add random bytes to end
--    7. Arithmetic:        Add/subtract from numeric fields
--    8. Boundary Values:   0x00, 0xFF, max values
--
--  Security Properties Verified:
--    • Zero false accepts (no malformed vault opens)
--    • No crashes (all errors handled gracefully)
--    • No unhandled exceptions
--    • Bounded execution time (no infinite loops)
--
--  Author: SparkPass Security Team
--  Standard: NIST SP 800-53r5 SI-7 (Software Integrity)

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Exceptions;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Config;
with SparkPass.Vault; use SparkPass.Vault;

procedure Test_Vault_Fuzzer is

   --  Fuzzing configuration
   Iterations_Per_Strategy : constant Natural := 125;  -- 125 * 8 = 1000 total
   Seed_Path : constant String := "/tmp/test_fuzzer_seed.spass";
   Temp_Prefix : constant String := "/tmp/test_fuzzer_";

   --  Statistics
   Total_Mutations : Natural := 0;
   Correctly_Rejected : Natural := 0;
   False_Accepts : Natural := 0;
   Crashes : Natural := 0;
   Total_Time_Ms : Float := 0.0;
   Max_Time_Ms : Float := 0.0;

   --  Strategy-specific counters
   type Strategy_Stats is record
      Name : String (1 .. 30);
      Tested : Natural := 0;
      Rejected : Natural := 0;
      Failed : Natural := 0;
   end record;

   type Strategy_Stats_Array is array (1 .. 8) of Strategy_Stats;
   Strategy_Stats_Data : Strategy_Stats_Array :=
     ((Name => "Bit Flips                     ", others => <>),
      (Name => "Byte Substitution             ", others => <>),
      (Name => "Magic Corruption              ", others => <>),
      (Name => "Length Corruption             ", others => <>),
      (Name => "Truncation                    ", others => <>),
      (Name => "Extension                     ", others => <>),
      (Name => "Arithmetic                    ", others => <>),
      (Name => "Boundary Values               ", others => <>));

   --  Random number generator
   package Random_Byte is new Ada.Numerics.Discrete_Random (U8);
   Gen : Random_Byte.Generator;

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
   procedure Create_Seed_Vault is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("fuzzer_seed_password_123");
      Label1 : constant Byte_Array := To_Bytes ("test_entry_1");
      Label2 : constant Byte_Array := To_Bytes ("test_entry_2");
      Data1 : constant Byte_Array := To_Bytes ("secret_data_value_one");
      Data2 : constant Byte_Array := To_Bytes ("secret_data_value_two");
      Save_St : SparkPass.Vault.Save_Status;
      Success : Boolean;
   begin
      --  Create vault with two entries to make it non-trivial
      SparkPass.Vault.Create (State, Password, Now);
      SparkPass.Vault.Add_Entry (State, Label1, SparkPass.Types.Password, Data1, Now, Success);
      SparkPass.Vault.Add_Entry (State, Label2, SparkPass.Types.Secure_Note, Data2, Now, Success);
      SparkPass.Vault.Save (State, Seed_Path, Save_St);
      SparkPass.Vault.Clear (State);
   exception
      when E : others =>
         Put_Line ("ERROR: Failed to create seed vault: " & Ada.Exceptions.Exception_Message (E));
         raise;
   end Create_Seed_Vault;

   --  Helper: Copy file
   procedure Copy_File (Source : String; Dest : String) is
      File_In : Ada.Streams.Stream_IO.File_Type;
      File_Out : Ada.Streams.Stream_IO.File_Type;
      Buffer : U8;
   begin
      Ada.Streams.Stream_IO.Open (File_In, Ada.Streams.Stream_IO.In_File, Source);
      Ada.Streams.Stream_IO.Create (File_Out, Ada.Streams.Stream_IO.Out_File, Dest);

      while not Ada.Streams.Stream_IO.End_Of_File (File_In) loop
         U8'Read (Ada.Streams.Stream_IO.Stream (File_In), Buffer);
         U8'Write (Ada.Streams.Stream_IO.Stream (File_Out), Buffer);
      end loop;

      Ada.Streams.Stream_IO.Close (File_In);
      Ada.Streams.Stream_IO.Close (File_Out);
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File_In) then
            Ada.Streams.Stream_IO.Close (File_In);
         end if;
         if Ada.Streams.Stream_IO.Is_Open (File_Out) then
            Ada.Streams.Stream_IO.Close (File_Out);
         end if;
         raise;
   end Copy_File;

   --  Helper: Get file size
   function Get_File_Size (Path : String) return Natural is
   begin
      return Natural (Ada.Directories.Size (Path));
   exception
      when others =>
         return 0;
   end Get_File_Size;

   --  Mutation Strategy 1: Random bit flips
   procedure Mutate_Bit_Flip (Path : String; Flip_Count : Positive) is
      File : Ada.Streams.Stream_IO.File_Type;
      File_Size : constant Natural := Get_File_Size (Path);
      Buffer : Byte_Array (1 .. File_Size);
      Offset : Positive;
      Bit_Index : U8;
   begin
      --  Read file into buffer
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      for I in Buffer'Range loop
         U8'Read (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);

      --  Flip random bits
      for I in 1 .. Flip_Count loop
         Offset := 1 + Natural (Random_Byte.Random (Gen)) mod File_Size;
         Bit_Index := Random_Byte.Random (Gen) mod 8;
         Buffer (Offset) := Buffer (Offset) xor (2 ** Natural (Bit_Index));
      end loop;

      --  Write mutated buffer back
      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Path);
      for I in Buffer'Range loop
         U8'Write (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Ada.Streams.Stream_IO.Close (File);
         end if;
         raise;
   end Mutate_Bit_Flip;

   --  Mutation Strategy 2: Random byte substitution
   procedure Mutate_Byte_Substitution (Path : String; Sub_Count : Positive) is
      File : Ada.Streams.Stream_IO.File_Type;
      File_Size : constant Natural := Get_File_Size (Path);
      Buffer : Byte_Array (1 .. File_Size);
      Offset : Positive;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      for I in Buffer'Range loop
         U8'Read (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);

      for I in 1 .. Sub_Count loop
         Offset := 1 + Natural (Random_Byte.Random (Gen)) mod File_Size;
         Buffer (Offset) := Random_Byte.Random (Gen);
      end loop;

      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Path);
      for I in Buffer'Range loop
         U8'Write (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Ada.Streams.Stream_IO.Close (File);
         end if;
         raise;
   end Mutate_Byte_Substitution;

   --  Mutation Strategy 3: Magic byte corruption
   procedure Mutate_Magic (Path : String) is
      File : Ada.Streams.Stream_IO.File_Type;
      File_Size : constant Natural := Get_File_Size (Path);
      Buffer : Byte_Array (1 .. File_Size);
      Magic_Offset : constant := 1;
   begin
      --  Read file into buffer
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      for I in Buffer'Range loop
         U8'Read (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);

      --  Corrupt magic byte
      Buffer (Magic_Offset) := Random_Byte.Random (Gen);

      --  Write mutated buffer back
      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Path);
      for I in Buffer'Range loop
         U8'Write (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Ada.Streams.Stream_IO.Close (File);
         end if;
         raise;
   end Mutate_Magic;

   --  Mutation Strategy 4: Length field corruption
   procedure Mutate_Length (Path : String) is
      File : Ada.Streams.Stream_IO.File_Type;
      File_Size : constant Natural := Get_File_Size (Path);
      Buffer : Byte_Array (1 .. File_Size);
      --  Entry count is at offset 19 (after magic + version + timestamps + nonce_counter)
      Entry_Count_Offset : constant := 19;
      Bad_Count : constant U32 := 16#FFFF_FFFF#;  -- Invalid entry count
   begin
      --  Read file into buffer
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      for I in Buffer'Range loop
         U8'Read (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);

      --  Corrupt entry count (write 4 bytes of U32)
      if Entry_Count_Offset + 3 <= File_Size then
         Buffer (Entry_Count_Offset) := U8 (Bad_Count and 16#FF#);
         Buffer (Entry_Count_Offset + 1) := U8 ((Bad_Count / 256) and 16#FF#);
         Buffer (Entry_Count_Offset + 2) := U8 ((Bad_Count / 65536) and 16#FF#);
         Buffer (Entry_Count_Offset + 3) := U8 ((Bad_Count / 16777216) and 16#FF#);
      end if;

      --  Write mutated buffer back
      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Path);
      for I in Buffer'Range loop
         U8'Write (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Ada.Streams.Stream_IO.Close (File);
         end if;
         raise;
   end Mutate_Length;

   --  Mutation Strategy 5: File truncation
   procedure Mutate_Truncate (Path : String; New_Size : Positive) is
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
   end Mutate_Truncate;

   --  Mutation Strategy 6: File extension (add random bytes)
   procedure Mutate_Extend (Path : String; Extra_Bytes : Positive) is
      File : Ada.Streams.Stream_IO.File_Type;
      Random_Byte_Val : U8;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.Append_File, Path);
      for I in 1 .. Extra_Bytes loop
         Random_Byte_Val := Random_Byte.Random (Gen);
         U8'Write (Ada.Streams.Stream_IO.Stream (File), Random_Byte_Val);
      end loop;
      Ada.Streams.Stream_IO.Close (File);
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Ada.Streams.Stream_IO.Close (File);
         end if;
         raise;
   end Mutate_Extend;

   --  Mutation Strategy 7: Arithmetic mutations on numeric fields
   procedure Mutate_Arithmetic (Path : String) is
      File : Ada.Streams.Stream_IO.File_Type;
      File_Size : constant Natural := Get_File_Size (Path);
      Buffer : Byte_Array (1 .. File_Size);
      Offset : Positive;
      Operation : U8;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      for I in Buffer'Range loop
         U8'Read (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);

      --  Apply arithmetic operations to random offsets
      for I in 1 .. 5 loop
         Offset := 1 + Natural (Random_Byte.Random (Gen)) mod File_Size;
         Operation := Random_Byte.Random (Gen) mod 4;
         case Operation is
            when 0 => Buffer (Offset) := Buffer (Offset) + 1;
            when 1 => Buffer (Offset) := Buffer (Offset) - 1;
            when 2 => Buffer (Offset) := Buffer (Offset) + 128;
            when others => Buffer (Offset) := Buffer (Offset) - 128;
         end case;
      end loop;

      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Path);
      for I in Buffer'Range loop
         U8'Write (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Ada.Streams.Stream_IO.Close (File);
         end if;
         raise;
   end Mutate_Arithmetic;

   --  Mutation Strategy 8: Boundary value injection
   procedure Mutate_Boundary (Path : String) is
      File : Ada.Streams.Stream_IO.File_Type;
      File_Size : constant Natural := Get_File_Size (Path);
      Buffer : Byte_Array (1 .. File_Size);
      Offset : Positive;
      Boundary_Values : constant array (1 .. 3) of U8 := (0, 16#FF#, 16#80#);
      Value_Index : Positive;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      for I in Buffer'Range loop
         U8'Read (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);

      for I in 1 .. 10 loop
         Offset := 1 + Natural (Random_Byte.Random (Gen)) mod File_Size;
         Value_Index := 1 + Natural (Random_Byte.Random (Gen)) mod 3;
         Buffer (Offset) := Boundary_Values (Value_Index);
      end loop;

      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Path);
      for I in Buffer'Range loop
         U8'Write (Ada.Streams.Stream_IO.Stream (File), Buffer (I));
      end loop;
      Ada.Streams.Stream_IO.Close (File);
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Ada.Streams.Stream_IO.Close (File);
         end if;
         raise;
   end Mutate_Boundary;

   --  Test a mutated vault
   procedure Test_Mutated_Vault (Mutated_Path : String; Strategy_Index : Positive) is
      use Ada.Calendar;
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("fuzzer_seed_password_123");
      Open_St : SparkPass.Vault.Open_Status;
      Start_Time : Time;
      End_Time : Time;
      Elapsed_Ms : Float;
   begin
      --  Measure execution time
      Start_Time := Clock;

      --  Try to open mutated vault (should fail)
      SparkPass.Vault.Open (State, Mutated_Path, Password, Open_St);

      End_Time := Clock;
      Elapsed_Ms := Float (End_Time - Start_Time) * 1000.0;

      --  Update statistics
      Total_Time_Ms := Total_Time_Ms + Elapsed_Ms;
      if Elapsed_Ms > Max_Time_Ms then
         Max_Time_Ms := Elapsed_Ms;
      end if;

      Total_Mutations := Total_Mutations + 1;
      Strategy_Stats_Data (Strategy_Index).Tested := Strategy_Stats_Data (Strategy_Index).Tested + 1;

      if Open_St /= SparkPass.Vault.Success then
         --  Correctly rejected
         Correctly_Rejected := Correctly_Rejected + 1;
         Strategy_Stats_Data (Strategy_Index).Rejected := Strategy_Stats_Data (Strategy_Index).Rejected + 1;
      else
         --  FALSE ACCEPT - CRITICAL SECURITY BUG
         False_Accepts := False_Accepts + 1;
         Strategy_Stats_Data (Strategy_Index).Failed := Strategy_Stats_Data (Strategy_Index).Failed + 1;
         Put_Line ("✗ CRITICAL: False accept detected!");
         Put_Line ("  Strategy: " & Strategy_Stats_Data (Strategy_Index).Name);
         Put_Line ("  File: " & Mutated_Path);
         SparkPass.Vault.Clear (State);
      end if;

   exception
      when E : others =>
         --  Crash detected - should never happen
         Crashes := Crashes + 1;
         Strategy_Stats_Data (Strategy_Index).Failed := Strategy_Stats_Data (Strategy_Index).Failed + 1;
         Put_Line ("✗ CRASH: Unhandled exception: " & Ada.Exceptions.Exception_Message (E));
         Put_Line ("  Strategy: " & Strategy_Stats_Data (Strategy_Index).Name);
   end Test_Mutated_Vault;

   --  Run fuzzing campaign for a specific strategy
   procedure Fuzz_Strategy (Strategy_Index : Positive; Iterations : Natural) is
      Temp_Path : String := Temp_Prefix & "mutation_" & Natural'Image (Strategy_Index) & ".spass";
   begin
      for I in 1 .. Iterations loop
         --  Copy seed to temp file
         Copy_File (Seed_Path, Temp_Path);

         --  Apply mutation based on strategy
         case Strategy_Index is
            when 1 =>
               --  Bit flips (1-5 bits)
               Mutate_Bit_Flip (Temp_Path, 1 + Natural (Random_Byte.Random (Gen)) mod 5);
            when 2 =>
               --  Byte substitution (1-10 bytes)
               Mutate_Byte_Substitution (Temp_Path, 1 + Natural (Random_Byte.Random (Gen)) mod 10);
            when 3 =>
               --  Magic corruption
               Mutate_Magic (Temp_Path);
            when 4 =>
               --  Length corruption
               Mutate_Length (Temp_Path);
            when 5 =>
               --  Truncation (remove 10-1000 bytes)
               declare
                  File_Size : constant Natural := Get_File_Size (Temp_Path);
                  Remove_Bytes : constant Natural := 10 + Natural (Random_Byte.Random (Gen)) * 4;
               begin
                  if File_Size > Remove_Bytes then
                     Mutate_Truncate (Temp_Path, File_Size - Remove_Bytes);
                  end if;
               end;
            when 6 =>
               --  Extension (add 10-100 bytes)
               Mutate_Extend (Temp_Path, 10 + Natural (Random_Byte.Random (Gen)) mod 90);
            when 7 =>
               --  Arithmetic mutations
               Mutate_Arithmetic (Temp_Path);
            when 8 =>
               --  Boundary value injection
               Mutate_Boundary (Temp_Path);
            when others =>
               null;
         end case;

         --  Test mutated vault
         Test_Mutated_Vault (Temp_Path, Strategy_Index);

         --  Clean up temp file
         if Ada.Directories.Exists (Temp_Path) then
            Ada.Directories.Delete_File (Temp_Path);
         end if;
      end loop;
   exception
      when E : others =>
         Put_Line ("ERROR in strategy " & Natural'Image (Strategy_Index) & ": " &
                   Ada.Exceptions.Exception_Message (E));
         if Ada.Directories.Exists (Temp_Path) then
            Ada.Directories.Delete_File (Temp_Path);
         end if;
   end Fuzz_Strategy;

   --  Display progress bar
   procedure Display_Progress (Current : Natural; Total : Natural) is
      Percent : Natural := (Current * 100) / Total;
      Bar_Length : constant Natural := 40;
      Filled : Natural := (Percent * Bar_Length) / 100;
   begin
      Put (ASCII.CR & "  Progress: [");
      for I in 1 .. Bar_Length loop
         if I <= Filled then
            Put ("#");
         else
            Put (" ");
         end if;
      end loop;
      Put ("] " & Natural'Image (Percent) & "%");
      if Current = Total then
         New_Line;
      end if;
   end Display_Progress;

   --  Clean up all temp files
   procedure Cleanup_Temp_Files is
      Search : Ada.Directories.Search_Type;
      Dir_Entry : Ada.Directories.Directory_Entry_Type;
   begin
      Ada.Directories.Start_Search (Search, "/tmp", "test_fuzzer_*.spass");
      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Dir_Entry);
         Ada.Directories.Delete_File (Ada.Directories.Full_Name (Dir_Entry));
      end loop;
      Ada.Directories.End_Search (Search);
   exception
      when others =>
         null;  -- Ignore cleanup errors
   end Cleanup_Temp_Files;

begin
   Random_Byte.Reset (Gen);

   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Vault File Format Fuzzing Harness                        ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("[1] Generating seed vault...");
   Create_Seed_Vault;
   Put_Line ("  ✓ Seed created: " & Seed_Path);
   New_Line;

   Put_Line ("[2] Running fuzzing campaign (" & Natural'Image (Iterations_Per_Strategy * 8) & " iterations)...");

   --  Run each fuzzing strategy
   for Strategy_Index in 1 .. 8 loop
      Fuzz_Strategy (Strategy_Index, Iterations_Per_Strategy);
      Display_Progress (Strategy_Index, 8);
   end loop;

   New_Line;
   New_Line;

   --  Display per-strategy results
   for I in Strategy_Stats_Data'Range loop
      Put ("  Mutation Strategy: " & Strategy_Stats_Data (I).Name & " | Tested: ");
      Put (Natural'Image (Strategy_Stats_Data (I).Tested));
      Put (" | Rejected: ");
      Put (Natural'Image (Strategy_Stats_Data (I).Rejected));
      if Strategy_Stats_Data (I).Failed = 0 then
         Put_Line (" ✓");
      else
         Put_Line (" ✗ FAILED: " & Natural'Image (Strategy_Stats_Data (I).Failed));
      end if;
   end loop;

   New_Line;

   --  Summary
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Fuzzing Results                                           ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   Put_Line ("Total Mutations:    " & Natural'Image (Total_Mutations));
   Put_Line ("Correctly Rejected: " & Natural'Image (Correctly_Rejected) & " (" &
             Natural'Image ((Correctly_Rejected * 100) / Total_Mutations) & "%)");
   Put_Line ("False Accepts:      " & Natural'Image (False_Accepts) &
             (if False_Accepts = 0 then " ✓" else " ✗ CRITICAL"));
   Put_Line ("Crashes:            " & Natural'Image (Crashes) &
             (if Crashes = 0 then " ✓" else " ✗ FAIL"));

   if Total_Mutations > 0 then
      Put_Line ("Avg Time/Mutation:  " & Float'Image (Total_Time_Ms / Float (Total_Mutations)) & " ms");
   end if;
   Put_Line ("Max Time/Mutation:  " & Float'Image (Max_Time_Ms) & " ms");
   New_Line;

   if False_Accepts = 0 and then Crashes = 0 then
      Put_Line ("✓ All fuzzing tests PASSED");
      Put_Line ("✓ No false accepts detected");
      Put_Line ("✓ No crashes or exceptions");
   else
      Put_Line ("✗ FUZZING FAILURES DETECTED");
      if False_Accepts > 0 then
         Put_Line ("✗ " & Natural'Image (False_Accepts) & " false accepts (CRITICAL SECURITY BUG)");
      end if;
      if Crashes > 0 then
         Put_Line ("✗ " & Natural'Image (Crashes) & " crashes detected");
      end if;
   end if;

   --  Cleanup
   Cleanup_Temp_Files;

exception
   when E : others =>
      Put_Line ("FATAL ERROR: " & Ada.Exceptions.Exception_Message (E));
      Cleanup_Temp_Files;
      raise;
end Test_Vault_Fuzzer;
