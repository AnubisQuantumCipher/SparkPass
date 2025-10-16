pragma SPARK_Mode (Off);
--  Timing Attack Validation Tests for SparkPass
--
--  This test suite validates constant-time behavior for security-critical operations.
--  Timing leaks can allow attackers to extract secrets through statistical analysis
--  of operation timing across many samples.
--
--  Tests validate:
--    1. Password Comparison Timing    - Correct vs incorrect passwords
--    2. Entry Lookup Timing           - Existing vs non-existing labels
--    3. HMAC Verification Timing      - Correct vs corrupted HMACs
--    4. Password Length Independence  - Various password lengths
--
--  Methodology:
--    - High-resolution timing via Ada.Real_Time (nanosecond precision)
--    - Multiple iterations (100+) for statistical significance
--    - Warm-up period to stabilize CPU cache and frequency scaling
--    - Statistical analysis: mean, standard deviation, relative difference
--    - Pass threshold: <5% relative timing difference (configurable)
--
--  **Security Note**: These tests validate relative timing under controlled
--  conditions. They cannot detect all side-channel leaks (cache, power, EM).
--  Defense in depth requires additional mitigations at network and application layers.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Elementary_Functions;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Config;
with SparkPass.Vault; use SparkPass.Vault;
with SparkPass.Crypto.Zeroize;

procedure Test_Timing_Attacks is

   --  Test configuration
   Warmup_Iterations   : constant := 10;
   Test_Iterations     : constant := 100;
   Threshold_Percent   : constant := 5.0;  -- 5% max relative difference

   --  Test state
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   Fail_Count : Natural := 0;

   --  Timing measurement type (in seconds, for easier arithmetic)
   type Duration_Array is array (Positive range <>) of Duration;

   --  Helper: Get current timestamp (UNIX epoch)
   function Now return U64 is
      use Ada.Calendar;
      Epoch : constant Ada.Calendar.Time := Time_Of (1970, 1, 1, 0.0);
      Now_Time : constant Ada.Calendar.Time := Clock;
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

   --  Helper: Compute mean of duration samples
   function Compute_Mean (Samples : Duration_Array) return Duration is
      Sum : Duration := 0.0;
   begin
      for Sample of Samples loop
         Sum := Sum + Sample;
      end loop;
      return Sum / Duration (Samples'Length);
   end Compute_Mean;

   --  Helper: Compute standard deviation of duration samples
   function Compute_Std_Dev (Samples : Duration_Array; Mean : Duration) return Duration is
      use Ada.Numerics.Elementary_Functions;
      Sum_Sq_Diff : Duration := 0.0;
   begin
      for Sample of Samples loop
         declare
            Diff : constant Duration := Sample - Mean;
         begin
            Sum_Sq_Diff := Sum_Sq_Diff + (Diff * Diff);
         end;
      end loop;
      return Duration (Sqrt (Float (Sum_Sq_Diff / Duration (Samples'Length))));
   end Compute_Std_Dev;

   --  Helper: Format duration in milliseconds with 2 decimal places
   function Format_Ms (D : Duration) return String is
      Ms : constant Float := Float (D) * 1000.0;
      Ms_Int : constant Integer := Integer (Ms);
      Ms_Frac : constant Integer := Integer ((Ms - Float (Ms_Int)) * 100.0);
   begin
      return Integer'Image (Ms_Int) & "." &
             (if Ms_Frac < 10 then "0" else "") &
             Integer'Image (Ms_Frac) (2 .. Integer'Image (Ms_Frac)'Last);
   end Format_Ms;

   --  Helper: Format percentage with 2 decimal places
   function Format_Percent (P : Float) return String is
      P_Int : constant Integer := Integer (P);
      P_Frac : constant Integer := Integer ((P - Float (P_Int)) * 100.0);
   begin
      return Integer'Image (P_Int) & "." &
             (if P_Frac < 10 then "0" else "") &
             Integer'Image (P_Frac) (2 .. Integer'Image (P_Frac)'Last);
   end Format_Percent;

   --  Helper: Create temporary vault file with password
   function Create_Test_Vault (Path : String; Password : Byte_Array) return Boolean is
      State : Vault_State;
      Save_St : Save_Status;
   begin
      Create (State, Password, Now);
      if not State.Unlocked then
         Clear (State);
         return False;
      end if;

      Save (State, Path, Save_St);
      Clear (State);
      return Save_St = Saved;
   end Create_Test_Vault;

   --  Helper: Create vault with fixed set of entries for lookup tests
   function Create_Vault_With_Entries
     (Path     : String;
      Password : Byte_Array)
     return Boolean
   is
      State : Vault_State;
      Save_St : Save_Status;
      Success : Boolean;
   begin
      Create (State, Password, Now);
      if not State.Unlocked then
         Clear (State);
         return False;
      end if;

      --  Add 10 fixed entries
      for I in 1 .. 10 loop
         declare
            Label_Str : constant String := "entry_" &
              (if I < 10 then "00" else "0") & Integer'Image (I) (2 .. Integer'Image (I)'Last);
            Label : constant Byte_Array := To_Bytes (Label_Str);
            Data  : constant Byte_Array := To_Bytes ("secret_value_" & Integer'Image (I));
         begin
            Add_Entry (State, Label, SparkPass.Types.Password, Data, Now, Success);
            if not Success then
               Clear (State);
               return False;
            end if;
         end;
      end loop;

      Save (State, Path, Save_St);
      Clear (State);
      return Save_St = Saved;
   end Create_Vault_With_Entries;

   --  Test 1: Password Comparison Timing
   --  Validates that correct and incorrect passwords take the same time
   --  (constant-time Argon2id verification + HMAC comparison)
   procedure Test_Password_Comparison_Timing is
      Test_Path : constant String := "/tmp/test_timing_password.spass";
      Password_Correct   : constant Byte_Array := To_Bytes ("correct_password_12345");
      Password_Incorrect : constant Byte_Array := To_Bytes ("wrong_password_456789");

      Samples_Correct   : Duration_Array (1 .. Test_Iterations);
      Samples_Incorrect : Duration_Array (1 .. Test_Iterations);

      Mean_Correct, Mean_Incorrect : Duration;
      Stddev_Correct, Stddev_Incorrect : Duration;
      Abs_Diff, Rel_Diff : Float;

      State : Vault_State;
      Status : Open_Status;
      Start_Time, End_Time : Ada.Real_Time.Time;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 1: Password Comparison Timing ===");
      Put_Line ("Testing constant-time password verification...");
      New_Line;

      --  Create test vault
      if not Create_Test_Vault (Test_Path, Password_Correct) then
         Put_Line ("  ✗ FAIL: Could not create test vault");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Warm-up: stabilize CPU cache and frequency scaling
      Put_Line ("[Warm-up: Running" & Natural'Image (Warmup_Iterations) & " iterations to stabilize measurements...]");
      for I in 1 .. Warmup_Iterations loop
         Open (State, Test_Path, Password_Correct, Status);
         Clear (State);
      end loop;
      Put_Line ("  ✓ Warm-up complete");
      New_Line;

      --  Measure correct password authentication
      Put_Line ("[1] Measuring correct password authentication (" &
                Natural'Image (Test_Iterations) & " iterations)...");

      for I in Samples_Correct'Range loop
         Start_Time := Clock;
         Open (State, Test_Path, Password_Correct, Status);
         End_Time := Clock;
         Clear (State);

         Samples_Correct (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Correct := Compute_Mean (Samples_Correct);
      Stddev_Correct := Compute_Std_Dev (Samples_Correct, Mean_Correct);

      Put_Line ("  Mean Time:" & Format_Ms (Mean_Correct) & " ms");
      Put_Line ("  Std Dev:  " & Format_Ms (Stddev_Correct) & " ms");
      New_Line;

      --  Measure incorrect password authentication
      Put_Line ("[2] Measuring incorrect password authentication (" &
                Natural'Image (Test_Iterations) & " iterations)...");

      for I in Samples_Incorrect'Range loop
         Start_Time := Clock;
         Open (State, Test_Path, Password_Incorrect, Status);
         End_Time := Clock;
         Clear (State);

         Samples_Incorrect (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Incorrect := Compute_Mean (Samples_Incorrect);
      Stddev_Incorrect := Compute_Std_Dev (Samples_Incorrect, Mean_Incorrect);

      Put_Line ("  Mean Time:" & Format_Ms (Mean_Incorrect) & " ms");
      Put_Line ("  Std Dev:  " & Format_Ms (Stddev_Incorrect) & " ms");
      New_Line;

      --  Compute timing difference
      Put_Line ("[3] Computing timing difference...");
      Abs_Diff := Float (abs (Mean_Correct - Mean_Incorrect));
      Rel_Diff := (Abs_Diff / Float (Mean_Correct)) * 100.0;

      Put_Line ("  Absolute Difference:" & Format_Ms (Duration (Abs_Diff)) & " ms");
      Put_Line ("  Relative Difference:" & Format_Percent (Rel_Diff) & "%");
      Put_Line ("  Threshold:          " & Format_Percent (Threshold_Percent) & "%");
      New_Line;

      --  Verdict
      if Rel_Diff < Threshold_Percent then
         Put_Line ("  ✓ PASS: Constant-time (" & Format_Percent (Rel_Diff) & "% <" &
                   Format_Percent (Threshold_Percent) & "%)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Timing leak detected (" & Format_Percent (Rel_Diff) & "% ≥" &
                   Format_Percent (Threshold_Percent) & "%)");
         Fail_Count := Fail_Count + 1;
      end if;
      New_Line;
   end Test_Password_Comparison_Timing;

   --  Test 2: Entry Lookup Timing
   --  Validates that looking up existing vs non-existing entries takes the same time
   --  (constant-time label comparison via Labels_Match)
   procedure Test_Entry_Lookup_Timing is
      Test_Path : constant String := "/tmp/test_timing_lookup.spass";
      Password : constant Byte_Array := To_Bytes ("test_password_123");

      Label_Exists    : constant Byte_Array := To_Bytes ("entry_005");  -- Middle of list
      Label_Not_Exist : constant Byte_Array := To_Bytes ("nonexistent");

      Samples_Exists    : Duration_Array (1 .. Test_Iterations);
      Samples_Not_Exist : Duration_Array (1 .. Test_Iterations);

      Mean_Exists, Mean_Not_Exist : Duration;
      Stddev_Exists, Stddev_Not_Exist : Duration;
      Abs_Diff, Rel_Diff : Float;

      State : Vault_State;
      Status : Open_Status;
      Plaintext : Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Data_Len : Natural;
      Success : Boolean;
      Start_Time, End_Time : Ada.Real_Time.Time;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 2: Entry Lookup Timing ===");
      Put_Line ("Testing constant-time entry lookup...");
      New_Line;

      --  Create test vault with entries
      if not Create_Vault_With_Entries (Test_Path, Password) then
         Put_Line ("  ✗ FAIL: Could not create test vault with entries");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Open vault once for all tests
      Open (State, Test_Path, Password, Status);
      if Status /= SparkPass.Vault.Success then
         Put_Line ("  ✗ FAIL: Could not open test vault");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Warm-up
      Put_Line ("[Warm-up: Running" & Natural'Image (Warmup_Iterations) & " iterations...]");
      for I in 1 .. Warmup_Iterations loop
         Get_Entry (State, Label_Exists, Plaintext, Data_Len, Success);
      end loop;
      Put_Line ("  ✓ Warm-up complete");
      New_Line;

      --  Measure existing entry lookup
      Put_Line ("[1] Measuring existing entry lookup (" &
                Natural'Image (Test_Iterations) & " iterations)...");

      for I in Samples_Exists'Range loop
         Start_Time := Clock;
         Get_Entry (State, Label_Exists, Plaintext, Data_Len, Success);
         End_Time := Clock;

         Samples_Exists (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Exists := Compute_Mean (Samples_Exists);
      Stddev_Exists := Compute_Std_Dev (Samples_Exists, Mean_Exists);

      Put_Line ("  Mean Time:" & Format_Ms (Mean_Exists) & " ms");
      Put_Line ("  Std Dev:  " & Format_Ms (Stddev_Exists) & " ms");
      New_Line;

      --  Measure non-existing entry lookup
      Put_Line ("[2] Measuring non-existing entry lookup (" &
                Natural'Image (Test_Iterations) & " iterations)...");

      for I in Samples_Not_Exist'Range loop
         Start_Time := Clock;
         Get_Entry (State, Label_Not_Exist, Plaintext, Data_Len, Success);
         End_Time := Clock;

         Samples_Not_Exist (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Not_Exist := Compute_Mean (Samples_Not_Exist);
      Stddev_Not_Exist := Compute_Std_Dev (Samples_Not_Exist, Mean_Not_Exist);

      Put_Line ("  Mean Time:" & Format_Ms (Mean_Not_Exist) & " ms");
      Put_Line ("  Std Dev:  " & Format_Ms (Stddev_Not_Exist) & " ms");
      New_Line;

      --  Compute timing difference
      Put_Line ("[3] Computing timing difference...");
      Abs_Diff := Float (abs (Mean_Exists - Mean_Not_Exist));
      Rel_Diff := (Abs_Diff / Float (Mean_Exists)) * 100.0;

      Put_Line ("  Absolute Difference:" & Format_Ms (Duration (Abs_Diff)) & " ms");
      Put_Line ("  Relative Difference:" & Format_Percent (Rel_Diff) & "%");
      Put_Line ("  Threshold:          " & Format_Percent (Threshold_Percent) & "%");
      New_Line;

      --  Verdict
      if Rel_Diff < Threshold_Percent then
         Put_Line ("  ✓ PASS: Constant-time (" & Format_Percent (Rel_Diff) & "% <" &
                   Format_Percent (Threshold_Percent) & "%)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Timing leak detected (" & Format_Percent (Rel_Diff) & "% ≥" &
                   Format_Percent (Threshold_Percent) & "%)");
         Fail_Count := Fail_Count + 1;
      end if;

      Clear (State);
      New_Line;
   end Test_Entry_Lookup_Timing;

   --  Test 3: HMAC Verification Timing
   --  Validates that HMAC verification with different corruption locations takes the same time
   --  (constant-time comparison, no early termination)
   procedure Test_HMAC_Verification_Timing is
      Test_Path : constant String := "/tmp/test_timing_hmac.spass";
      Corrupt_First_Path : constant String := "/tmp/test_timing_hmac_first.spass";
      Corrupt_Last_Path  : constant String := "/tmp/test_timing_hmac_last.spass";

      Password : constant Byte_Array := To_Bytes ("test_password_123");

      Samples_Correct : Duration_Array (1 .. Test_Iterations);
      Samples_First   : Duration_Array (1 .. Test_Iterations);
      Samples_Last    : Duration_Array (1 .. Test_Iterations);

      Mean_Correct, Mean_First, Mean_Last : Duration;
      Max_Diff, Rel_Diff : Float;

      State : Vault_State;
      Status : Open_Status;
      Start_Time, End_Time : Ada.Real_Time.Time;

      --  Helper: Corrupt byte at position in file
      procedure Corrupt_File_Byte (Source, Dest : String; Position : Positive) is
         use Ada.Streams.Stream_IO;
         File_In, File_Out : Ada.Streams.Stream_IO.File_Type;
         Stream_In, Stream_Out : Ada.Streams.Stream_IO.Stream_Access;
         Byte : Interfaces.Unsigned_8;
         Count : Natural := 0;
      begin
         Open (File_In, In_File, Source);
         Create (File_Out, Out_File, Dest);
         Stream_In := Stream (File_In);
         Stream_Out := Stream (File_Out);

         while not End_Of_File (File_In) loop
            Count := Count + 1;
            Interfaces.Unsigned_8'Read (Stream_In, Byte);

            --  Corrupt byte at specified position
            if Count = Position then
               Byte := Byte xor 16#FF#;  -- Flip all bits
            end if;

            Interfaces.Unsigned_8'Write (Stream_Out, Byte);
         end loop;

         Close (File_In);
         Close (File_Out);
      end Corrupt_File_Byte;

   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 3: HMAC Verification Timing ===");
      Put_Line ("Testing constant-time HMAC comparison...");
      New_Line;

      --  Create test vault
      if not Create_Test_Vault (Test_Path, Password) then
         Put_Line ("  ✗ FAIL: Could not create test vault");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Create corrupted vaults (first byte and last byte of fingerprint)
      --  Fingerprint is in header at known offset
      Corrupt_File_Byte (Test_Path, Corrupt_First_Path, 100);  -- Corrupt near start
      Corrupt_File_Byte (Test_Path, Corrupt_Last_Path, 9600);  -- Corrupt near end (header size ~9697)

      --  Warm-up
      Put_Line ("[Warm-up: Running" & Natural'Image (Warmup_Iterations) & " iterations...]");
      for I in 1 .. Warmup_Iterations loop
         Open (State, Test_Path, Password, Status);
         Clear (State);
      end loop;
      Put_Line ("  ✓ Warm-up complete");
      New_Line;

      --  Measure correct HMAC verification
      Put_Line ("[1] Measuring correct HMAC verification (" &
                Natural'Image (Test_Iterations) & " iterations)...");

      for I in Samples_Correct'Range loop
         Start_Time := Clock;
         Open (State, Test_Path, Password, Status);
         End_Time := Clock;
         Clear (State);

         Samples_Correct (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Correct := Compute_Mean (Samples_Correct);
      Put_Line ("  Mean Time:" & Format_Ms (Mean_Correct) & " ms");
      New_Line;

      --  Measure corrupted HMAC (first byte) verification
      Put_Line ("[2] Measuring incorrect HMAC (first byte) verification (" &
                Natural'Image (Test_Iterations) & " iterations)...");

      for I in Samples_First'Range loop
         Start_Time := Clock;
         Open (State, Corrupt_First_Path, Password, Status);
         End_Time := Clock;
         Clear (State);

         Samples_First (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_First := Compute_Mean (Samples_First);
      Put_Line ("  Mean Time:" & Format_Ms (Mean_First) & " ms");
      New_Line;

      --  Measure corrupted HMAC (last byte) verification
      Put_Line ("[3] Measuring incorrect HMAC (last byte) verification (" &
                Natural'Image (Test_Iterations) & " iterations)...");

      for I in Samples_Last'Range loop
         Start_Time := Clock;
         Open (State, Corrupt_Last_Path, Password, Status);
         End_Time := Clock;
         Clear (State);

         Samples_Last (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Last := Compute_Mean (Samples_Last);
      Put_Line ("  Mean Time:" & Format_Ms (Mean_Last) & " ms");
      New_Line;

      --  Compute maximum timing variance
      Put_Line ("[4] Computing timing variance...");
      Max_Diff := Float'Max (abs (Float (Mean_Correct - Mean_First)),
                             abs (Float (Mean_Correct - Mean_Last)));
      Max_Diff := Float'Max (Max_Diff, abs (Float (Mean_First - Mean_Last)));
      Rel_Diff := (Max_Diff / Float (Mean_Correct)) * 100.0;

      Put_Line ("  Max Difference:    " & Format_Ms (Duration (Max_Diff)) & " ms");
      Put_Line ("  Relative Variance: " & Format_Percent (Rel_Diff) & "%");
      Put_Line ("  Threshold:         " & Format_Percent (Threshold_Percent) & "%");
      New_Line;

      --  Verdict
      if Rel_Diff < Threshold_Percent then
         Put_Line ("  ✓ PASS: Constant-time (" & Format_Percent (Rel_Diff) & "% <" &
                   Format_Percent (Threshold_Percent) & "%)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Timing leak detected (" & Format_Percent (Rel_Diff) & "% ≥" &
                   Format_Percent (Threshold_Percent) & "%)");
         Fail_Count := Fail_Count + 1;
      end if;
      New_Line;
   end Test_HMAC_Verification_Timing;

   --  Test 4: Password Length Independence
   --  Validates that authentication time is independent of password length
   --  (Argon2id normalizes all input lengths)
   procedure Test_Password_Length_Independence is
      Test_Path_Base : constant String := "/tmp/test_timing_pwlen";

      type Length_Array is array (1 .. 4) of Natural;
      Lengths : constant Length_Array := (12, 20, 32, 64);

      type Mean_Array is array (1 .. 4) of Duration;
      Means : Mean_Array;

      Max_Variance, Rel_Variance : Float;
      Min_Mean, Max_Mean : Duration;

      State : Vault_State;
      Status : Open_Status;
      Start_Time, End_Time : Ada.Real_Time.Time;
      Samples : Duration_Array (1 .. Test_Iterations);
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 4: Password Length Correlation ===");
      Put_Line ("Testing independence from password length...");
      New_Line;

      --  Test each password length
      for Idx in Lengths'Range loop
         declare
            Len : constant Natural := Lengths (Idx);
            Password : Byte_Array (1 .. Len);
            Test_Path : constant String := Test_Path_Base & Natural'Image (Len) & ".spass";
         begin
            --  Generate password of specified length
            for I in Password'Range loop
               Password (I) := U8 (Character'Pos ('a') + U8 (I mod 26));
            end loop;

            --  Create test vault
            if not Create_Test_Vault (Test_Path, Password) then
               Put_Line ("  ✗ FAIL: Could not create test vault for length" & Natural'Image (Len));
               Fail_Count := Fail_Count + 1;
               return;
            end if;

            --  Warm-up for this length
            if Idx = 1 then
               Put_Line ("[Warm-up: Running" & Natural'Image (Warmup_Iterations) & " iterations...]");
               for I in 1 .. Warmup_Iterations loop
                  Open (State, Test_Path, Password, Status);
                  Clear (State);
               end loop;
               Put_Line ("  ✓ Warm-up complete");
               New_Line;
            end if;

            --  Measure authentication time
            Put_Line ("Password Length:" & Natural'Image (Len) & " chars | Measuring (" &
                     Natural'Image (Test_Iterations) & " iterations)...");

            for I in Samples'Range loop
               Start_Time := Clock;
               Open (State, Test_Path, Password, Status);
               End_Time := Clock;
               Clear (State);

               Samples (I) := To_Duration (End_Time - Start_Time);
            end loop;

            Means (Idx) := Compute_Mean (Samples);
            Put_Line ("  Mean Time:" & Format_Ms (Means (Idx)) & " ms");
         end;
      end loop;
      New_Line;

      --  Compute maximum variance across all lengths
      Put_Line ("Computing length-independence metric...");
      Min_Mean := Means (1);
      Max_Mean := Means (1);

      for I in 2 .. Means'Last loop
         if Means (I) < Min_Mean then
            Min_Mean := Means (I);
         end if;
         if Means (I) > Max_Mean then
            Max_Mean := Means (I);
         end if;
      end loop;

      Max_Variance := Float (Max_Mean - Min_Mean);
      Rel_Variance := (Max_Variance / Float (Min_Mean)) * 100.0;

      Put_Line ("  Min Mean:" & Format_Ms (Min_Mean) & " ms");
      Put_Line ("  Max Mean:" & Format_Ms (Max_Mean) & " ms");
      Put_Line ("  Max Variance:      " & Format_Ms (Duration (Max_Variance)) & " ms");
      Put_Line ("  Relative Variance: " & Format_Percent (Rel_Variance) & "%");
      Put_Line ("  Threshold:         " & Format_Percent (Threshold_Percent) & "%");
      New_Line;

      --  Verdict
      if Rel_Variance < Threshold_Percent then
         Put_Line ("  ✓ PASS: Length-independent (" & Format_Percent (Rel_Variance) & "% <" &
                   Format_Percent (Threshold_Percent) & "%)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Length correlation detected (" & Format_Percent (Rel_Variance) & "% ≥" &
                   Format_Percent (Threshold_Percent) & "%)");
         Fail_Count := Fail_Count + 1;
      end if;
      New_Line;
   end Test_Password_Length_Independence;

begin
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Timing Attack Validation Tests                           ║");
   Put_Line ("║  Verifying constant-time cryptographic operations         ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("Test Configuration:");
   Put_Line ("  Iterations per test: " & Natural'Image (Test_Iterations));
   Put_Line ("  Warmup iterations:   " & Natural'Image (Warmup_Iterations));
   Put_Line ("  Pass threshold:      " & Format_Percent (Threshold_Percent) & "% relative difference");
   New_Line;

   --  Run timing attack tests
   Test_Password_Comparison_Timing;
   Test_Entry_Lookup_Timing;
   Test_HMAC_Verification_Timing;
   Test_Password_Length_Independence;

   --  Summary
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Timing Attack Test Summary                                ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   Put_Line ("Total Tests: " & Natural'Image (Test_Count));
   Put_Line ("Passed:      " & Natural'Image (Pass_Count));
   Put_Line ("Failed:      " & Natural'Image (Fail_Count));
   New_Line;

   if Fail_Count = 0 then
      Put_Line ("✓ All timing attack tests PASSED");
      Put_Line ("✓ No timing leaks detected");
      Put_Line ("✓ Constant-time operations verified");
      New_Line;
      Put_Line ("Note: These tests validate relative timing behavior under controlled");
      Put_Line ("      conditions. Production deployments should implement additional");
      Put_Line ("      mitigations:");
      Put_Line ("      - Rate limiting on authentication attempts");
      Put_Line ("      - Random delays on failed authentication");
      Put_Line ("      - Timing obfuscation at network layer");
      Put_Line ("      - Account lockout after N failures");
   else
      Put_Line ("✗ Some tests FAILED - review output above");
      Put_Line ("✗ Potential timing leaks detected");
      New_Line;
      Put_Line ("SECURITY CRITICAL: Fix timing leaks before production deployment!");
   end if;

end Test_Timing_Attacks;
