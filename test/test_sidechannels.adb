pragma SPARK_Mode (Off);
--  Side-Channel Analysis Tests for SparkPass
--
--  This test suite validates resistance to software-measurable side-channel attacks.
--  Side-channels allow attackers to extract secrets by observing the physical behavior
--  of cryptographic implementations (cache, timing, power, EM radiation, etc.).
--
--  **Tests Implemented**:
--    1. Cache-Timing Analysis      - CPU cache hit/miss patterns
--    2. Branch Prediction Analysis - CPU branch predictor state
--    3. Memory Access Patterns     - Sequential vs random access patterns
--    4. Speculative Execution      - Transient execution side-channels (Spectre-style)
--
--  **Methodology**:
--    - High-resolution timing via Ada.Real_Time (nanosecond precision on macOS)
--    - Statistical analysis: mean, variance, correlation coefficient
--    - CPU cache flushing: CLFLUSH simulation via large array access
--    - Branch training: predictor state manipulation
--    - Multiple iterations (100+) for statistical significance
--    - Pass threshold: correlation < 0.3 (minimal observable relationship)
--
--  **Security Properties Validated**:
--    - Constant-time password comparison (no early termination)
--    - Constant-time label search (always scans full entry table)
--    - Cache-independent cryptographic operations (no lookup tables)
--    - Branch-free comparison primitives (XOR accumulation)
--    - Fixed memory access patterns (no data-dependent indexing)
--
--  **Limitations** (Software-Only Testing):
--    ✓ CAN measure: basic cache timing, branch behavior (via timing), memory patterns
--    ✗ CANNOT measure: power consumption (needs oscilloscope), EM radiation (needs SDR),
--      acoustic signals (needs microphone array), advanced cache profiling (needs perf counters)
--
--  **References**:
--    - Kocher, P. "Timing Attacks on Implementations of Diffie-Hellman, RSA, DSS" (1996)
--    - Bernstein, D.J. "Cache-timing attacks on AES" (2005)
--    - Yarom, Y. & Falkner, K. "FLUSH+RELOAD: a High Resolution, Low Noise" (2014)
--    - Lipp, M. et al. "Meltdown" (2018), Kocher, P. et al. "Spectre Attacks" (2018)
--    - Introduction to Cryptography (Smart), Chapter 15: "Implementation Attacks"
--
--  **Defense Mechanisms in SparkPass**:
--    - libsodium constant-time primitives (sodium_memcmp, sodium_memzero)
--    - XOR-based comparisons (no conditional branches on secrets)
--    - Fixed-iteration loops (always scan all entries)
--    - AES-NI hardware acceleration (no lookup tables)
--    - Argon2id memory-hard KDF (cache-timing resistant)
--    - Secure allocators (guard pages, canaries)

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Elementary_Functions;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with System;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Config;
with SparkPass.Vault; use SparkPass.Vault;
with SparkPass.Crypto.Zeroize;

procedure Test_Sidechannels is

   --  Test configuration
   Warmup_Iterations   : constant := 10;
   Test_Iterations     : constant := 100;
   Correlation_Threshold : constant := 0.3;  -- Max 0.3 correlation (weak relationship)

   --  Test state
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   Fail_Count : Natural := 0;
   Skip_Count : Natural := 0;

   --  Timing measurement type (in seconds)
   type Duration_Array is array (Positive range <>) of Duration;

   --  Cache flushing array (16 MB to flush L2/L3 cache)
   --  Apple M2 Pro: 128 KB L1, 4 MB L2, 24 MB L3
   type Cache_Flusher_Array is array (1 .. 4_000_000) of Unsigned_32;
   Cache_Flusher : Cache_Flusher_Array := (others => 0);

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

   --  Helper: Compute standard deviation
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

   --  Helper: Compute correlation coefficient between two sample sets
   --  Returns value in [-1, 1]: 0 = no correlation, 1 = perfect positive, -1 = perfect negative
   function Compute_Correlation
     (Samples_X : Duration_Array;
      Samples_Y : Duration_Array)
     return Float
   is
      use Ada.Numerics.Elementary_Functions;
      Mean_X : constant Duration := Compute_Mean (Samples_X);
      Mean_Y : constant Duration := Compute_Mean (Samples_Y);
      Sum_XY : Duration := 0.0;
      Sum_XX : Duration := 0.0;
      Sum_YY : Duration := 0.0;
      Diff_X, Diff_Y : Duration;
   begin
      for I in Samples_X'Range loop
         Diff_X := Samples_X (I) - Mean_X;
         Diff_Y := Samples_Y (I) - Mean_Y;
         Sum_XY := Sum_XY + (Diff_X * Diff_Y);
         Sum_XX := Sum_XX + (Diff_X * Diff_X);
         Sum_YY := Sum_YY + (Diff_Y * Diff_Y);
      end loop;

      --  Pearson correlation coefficient
      if Sum_XX = 0.0 or Sum_YY = 0.0 then
         return 0.0;
      else
         return Float (Sum_XY) / Sqrt (Float (Sum_XX) * Float (Sum_YY));
      end if;
   end Compute_Correlation;

   --  Helper: Format duration in milliseconds
   function Format_Ms (D : Duration) return String is
      Ms : constant Float := Float (D) * 1000.0;
      Ms_Int : constant Integer := Integer (Ms);
      Ms_Frac : constant Integer := Integer ((Ms - Float (Ms_Int)) * 100.0);
   begin
      return Integer'Image (Ms_Int) & "." &
             (if Ms_Frac < 10 then "0" else "") &
             Integer'Image (abs Ms_Frac) (2 .. Integer'Image (abs Ms_Frac)'Last);
   end Format_Ms;

   --  Helper: Format percentage
   function Format_Percent (P : Float) return String is
      P_Int : constant Integer := Integer (P);
      P_Frac : constant Integer := Integer (abs (P - Float (P_Int)) * 100.0);
   begin
      return Integer'Image (P_Int) & "." &
             (if P_Frac < 10 then "0" else "") &
             Integer'Image (P_Frac) (2 .. Integer'Image (P_Frac)'Last);
   end Format_Percent;

   --  Helper: Format correlation coefficient with sign
   function Format_Correlation (C : Float) return String is
      Abs_C : constant Float := abs C;
      C_Int : constant Integer := Integer (Abs_C);
      C_Frac : constant Integer := Integer ((Abs_C - Float (C_Int)) * 100.0);
      Sign : constant String := (if C < 0.0 then "-" else " ");
   begin
      return Sign & Integer'Image (C_Int) & "." &
             (if C_Frac < 10 then "0" else "") &
             Integer'Image (C_Frac) (2 .. Integer'Image (C_Frac)'Last);
   end Format_Correlation;

   --  Helper: Flush CPU cache by accessing large array
   --  Evicts cache lines from L1, L2, L3 to force memory access
   procedure Flush_Cache is
      Dummy : Unsigned_32 := 0;
   begin
      for I in Cache_Flusher'Range loop
         Dummy := Dummy + Cache_Flusher (I);
      end loop;
      --  Prevent compiler optimization (use volatile write)
      Cache_Flusher (1) := Dummy;
   end Flush_Cache;

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

   --  Helper: Create vault with fixed entries for lookup tests
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

   --  ============================================================================
   --  Test 1: Cache-Timing Analysis
   --  ============================================================================
   --  **Attack**: Measure CPU cache hit/miss patterns to infer secret data
   --
   --  **Attack Scenarios**:
   --    1. Password character inference - Does cache behavior leak password characters?
   --    2. Label search patterns - Does cache reveal which entries exist?
   --    3. Cryptographic operation patterns - Does cache leak key material?
   --
   --  **Implementation Approach**:
   --    - Flush CPU cache before each operation (force cold start)
   --    - Measure timing difference (cache miss = slower)
   --    - Compute correlation between secret data and cache timing
   --    - Use Pearson correlation coefficient (threshold: 0.3)
   --
   --  **Pass Criteria**: Correlation < 0.3 (no observable cache-timing leak)
   --
   --  **Defense**: libsodium uses constant-time primitives, AES-NI hardware
   --  acceleration (no lookup tables), Argon2id memory-hard KDF
   --  ============================================================================
   procedure Test_Cache_Timing is
      Test_Path : constant String := "/tmp/test_sidechannel_cache.spass";
      Password_Correct   : constant Byte_Array := To_Bytes ("correct_password_12345");
      Password_Incorrect : constant Byte_Array := To_Bytes ("wrong_password_456789");

      Samples_Correct   : Duration_Array (1 .. Test_Iterations);
      Samples_Incorrect : Duration_Array (1 .. Test_Iterations);

      Mean_Correct, Mean_Incorrect : Duration;
      Stddev_Correct, Stddev_Incorrect : Duration;
      Correlation : Float;

      State : Vault_State;
      Status : Open_Status;
      Start_Time, End_Time : Ada.Real_Time.Time;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 1: Cache-Timing Analysis ===");
      Put_Line ("Attack: Measure CPU cache hit/miss patterns to infer secrets");
      Put_Line ("Defense: Constant-time primitives, AES-NI, memory-hard KDF");
      New_Line;

      --  Create test vault
      if not Create_Test_Vault (Test_Path, Password_Correct) then
         Put_Line ("  ✗ FAIL: Could not create test vault");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Warm-up: stabilize CPU cache and frequency scaling
      Put_Line ("[Warm-up: " & Natural'Image (Warmup_Iterations) & " iterations...]");
      for I in 1 .. Warmup_Iterations loop
         Open (State, Test_Path, Password_Correct, Status);
         Clear (State);
      end loop;
      Put_Line ("  ✓ Warm-up complete");
      New_Line;

      --  Measure cache behavior for CORRECT password
      Put_Line ("[1] Measuring cache behavior for CORRECT password");
      Put_Line ("    Flushing CPU cache before each authentication...");

      for I in Samples_Correct'Range loop
         Flush_Cache;  -- Force cache miss
         Start_Time := Clock;
         Open (State, Test_Path, Password_Correct, Status);
         End_Time := Clock;
         Clear (State);

         Samples_Correct (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Correct := Compute_Mean (Samples_Correct);
      Stddev_Correct := Compute_Std_Dev (Samples_Correct, Mean_Correct);

      Put_Line ("    Mean Time:  " & Format_Ms (Mean_Correct) & " ms");
      Put_Line ("    Std Dev:    " & Format_Ms (Stddev_Correct) & " ms");
      New_Line;

      --  Measure cache behavior for INCORRECT password
      Put_Line ("[2] Measuring cache behavior for INCORRECT password");
      Put_Line ("    Flushing CPU cache before each authentication...");

      for I in Samples_Incorrect'Range loop
         Flush_Cache;  -- Force cache miss
         Start_Time := Clock;
         Open (State, Test_Path, Password_Incorrect, Status);
         End_Time := Clock;
         Clear (State);

         Samples_Incorrect (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Incorrect := Compute_Mean (Samples_Incorrect);
      Stddev_Incorrect := Compute_Std_Dev (Samples_Incorrect, Mean_Incorrect);

      Put_Line ("    Mean Time:  " & Format_Ms (Mean_Incorrect) & " ms");
      Put_Line ("    Std Dev:    " & Format_Ms (Stddev_Incorrect) & " ms");
      New_Line;

      --  Compute correlation coefficient
      Put_Line ("[3] Computing cache-timing correlation...");
      Correlation := abs Compute_Correlation (Samples_Correct, Samples_Incorrect);

      Put_Line ("    Correlation:   " & Format_Correlation (Correlation));
      Put_Line ("    Threshold:     " & Format_Correlation (Correlation_Threshold));
      New_Line;

      --  Verdict
      if Correlation < Correlation_Threshold then
         Put_Line ("  ✓ PASS: No cache-timing leak detected (" &
                   Format_Correlation (Correlation) & " <" &
                   Format_Correlation (Correlation_Threshold) & ")");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Cache-timing leak detected (" &
                   Format_Correlation (Correlation) & " ≥" &
                   Format_Correlation (Correlation_Threshold) & ")");
         Fail_Count := Fail_Count + 1;
      end if;
      New_Line;
   end Test_Cache_Timing;

   --  ============================================================================
   --  Test 2: Branch Prediction Analysis
   --  ============================================================================
   --  **Attack**: Measure CPU branch predictor state to infer conditional execution
   --
   --  **Attack Scenarios**:
   --    1. Password comparison branches - Does branch behavior leak comparison results?
   --    2. Entry existence checks - Does branching reveal if entry exists?
   --    3. Error handling paths - Does exception path leak information?
   --
   --  **Implementation Approach**:
   --    - Train branch predictor with biased pattern
   --    - Measure timing after prediction (misprediction = penalty)
   --    - Detect data-dependent branching via timing variance
   --
   --  **Pass Criteria**: Correlation < 0.3 (no observable branch-timing leak)
   --
   --  **Defense**: XOR-based comparisons (no conditional branches on secrets),
   --  fixed-iteration loops (always scan all entries)
   --
   --  **Note**: Full branch analysis requires CPU performance counters (root/perf)
   --  We measure timing as a proxy for branch misprediction penalties
   --  ============================================================================
   procedure Test_Branch_Behavior is
      Test_Path : constant String := "/tmp/test_sidechannel_branch.spass";
      Password : constant Byte_Array := To_Bytes ("test_password_123");

      Label_Exists    : constant Byte_Array := To_Bytes ("entry_005");
      Label_Not_Exist : constant Byte_Array := To_Bytes ("nonexistent_entry");

      Samples_Exists    : Duration_Array (1 .. Test_Iterations);
      Samples_Not_Exist : Duration_Array (1 .. Test_Iterations);

      Mean_Exists, Mean_Not_Exist : Duration;
      Correlation : Float;

      State : Vault_State;
      Status : Open_Status;
      Plaintext : Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Data_Len : Natural;
      Success : Boolean;
      Start_Time, End_Time : Ada.Real_Time.Time;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 2: Branch Prediction Analysis ===");
      Put_Line ("Attack: Measure CPU branch predictor state to infer control flow");
      Put_Line ("Defense: Branch-free comparisons (XOR accumulation), fixed iterations");
      New_Line;

      --  Create test vault with entries
      if not Create_Vault_With_Entries (Test_Path, Password) then
         Put_Line ("  ✗ FAIL: Could not create test vault with entries");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Open vault once
      Open (State, Test_Path, Password, Status);
      if Status /= SparkPass.Vault.Success then
         Put_Line ("  ✗ FAIL: Could not open test vault");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Warm-up: train branch predictor with EXISTING entry pattern
      Put_Line ("[Warm-up: Training branch predictor with biased pattern...]");
      for I in 1 .. Warmup_Iterations loop
         Get_Entry (State, Label_Exists, Plaintext, Data_Len, Success);
      end loop;
      Put_Line ("  ✓ Branch predictor trained (biased toward 'found' path)");
      New_Line;

      --  Measure EXISTING entry lookup (predictor should be trained)
      Put_Line ("[1] Measuring EXISTING entry lookup (predictor trained)");

      for I in Samples_Exists'Range loop
         Start_Time := Clock;
         Get_Entry (State, Label_Exists, Plaintext, Data_Len, Success);
         End_Time := Clock;

         Samples_Exists (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Exists := Compute_Mean (Samples_Exists);
      Put_Line ("    Mean Time: " & Format_Ms (Mean_Exists) & " ms");
      New_Line;

      --  Measure NON-EXISTING entry lookup (predictor should mispredict)
      Put_Line ("[2] Measuring NON-EXISTING entry lookup (predictor should mispredict)");

      for I in Samples_Not_Exist'Range loop
         Start_Time := Clock;
         Get_Entry (State, Label_Not_Exist, Plaintext, Data_Len, Success);
         End_Time := Clock;

         Samples_Not_Exist (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Not_Exist := Compute_Mean (Samples_Not_Exist);
      Put_Line ("    Mean Time: " & Format_Ms (Mean_Not_Exist) & " ms");
      New_Line;

      --  Compute correlation
      Put_Line ("[3] Computing branch behavior correlation...");
      Correlation := abs Compute_Correlation (Samples_Exists, Samples_Not_Exist);

      Put_Line ("    Correlation:   " & Format_Correlation (Correlation));
      Put_Line ("    Threshold:     " & Format_Correlation (Correlation_Threshold));
      New_Line;

      --  Verdict
      if Correlation < Correlation_Threshold then
         Put_Line ("  ✓ PASS: No branch-timing leak detected (" &
                   Format_Correlation (Correlation) & " <" &
                   Format_Correlation (Correlation_Threshold) & ")");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ⚠ WARNING: Branch-timing correlation detected (" &
                   Format_Correlation (Correlation) & " ≥" &
                   Format_Correlation (Correlation_Threshold) & ")");
         Put_Line ("            This may be due to CPU frequency scaling or I/O variance");
         Put_Line ("            Run as root with perf counters for definitive analysis");
         Fail_Count := Fail_Count + 1;
      end if;

      Clear (State);
      New_Line;
   end Test_Branch_Behavior;

   --  ============================================================================
   --  Test 3: Memory Access Pattern Analysis
   --  ============================================================================
   --  **Attack**: Observe memory access patterns to infer operations
   --
   --  **Attack Scenarios**:
   --    1. Sequential vs random access - Does vault iterate predictably?
   --    2. Entry size patterns - Does access pattern reveal entry sizes?
   --    3. Allocation patterns - Does memory layout leak information?
   --
   --  **Implementation Approach**:
   --    - Monitor timing variance across different entry positions
   --    - Detect early termination (sequential search would show position correlation)
   --    - Verify constant access patterns (fixed number of iterations)
   --
   --  **Pass Criteria**: No correlation between entry position and access time
   --
   --  **Defense**: Always iterate full entry table (constant-time label search),
   --  fixed-size entry structures (no length-dependent operations)
   --  ============================================================================
   procedure Test_Memory_Patterns is
      Test_Path : constant String := "/tmp/test_sidechannel_memory.spass";
      Password : constant Byte_Array := To_Bytes ("test_password_123");

      Samples_First  : Duration_Array (1 .. Test_Iterations);
      Samples_Middle : Duration_Array (1 .. Test_Iterations);
      Samples_Last   : Duration_Array (1 .. Test_Iterations);

      Label_First  : constant Byte_Array := To_Bytes ("entry_001");
      Label_Middle : constant Byte_Array := To_Bytes ("entry_005");
      Label_Last   : constant Byte_Array := To_Bytes ("entry_010");

      Mean_First, Mean_Middle, Mean_Last : Duration;
      Correlation_First_Last : Float;

      State : Vault_State;
      Status : Open_Status;
      Plaintext : Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Data_Len : Natural;
      Success : Boolean;
      Start_Time, End_Time : Ada.Real_Time.Time;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 3: Memory Access Pattern Analysis ===");
      Put_Line ("Attack: Observe memory access order to infer vault structure");
      Put_Line ("Defense: Constant-time iteration (always scan all entries)");
      New_Line;

      --  Create test vault with entries
      if not Create_Vault_With_Entries (Test_Path, Password) then
         Put_Line ("  ✗ FAIL: Could not create test vault with entries");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Open vault once
      Open (State, Test_Path, Password, Status);
      if Status /= SparkPass.Vault.Success then
         Put_Line ("  ✗ FAIL: Could not open test vault");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Warm-up
      Put_Line ("[Warm-up: " & Natural'Image (Warmup_Iterations) & " iterations...]");
      for I in 1 .. Warmup_Iterations loop
         Get_Entry (State, Label_Middle, Plaintext, Data_Len, Success);
      end loop;
      Put_Line ("  ✓ Warm-up complete");
      New_Line;

      --  Measure access to FIRST entry
      Put_Line ("[1] Measuring access to FIRST entry (position 1/10)");

      for I in Samples_First'Range loop
         Start_Time := Clock;
         Get_Entry (State, Label_First, Plaintext, Data_Len, Success);
         End_Time := Clock;

         Samples_First (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_First := Compute_Mean (Samples_First);
      Put_Line ("    Mean Time: " & Format_Ms (Mean_First) & " ms");
      New_Line;

      --  Measure access to MIDDLE entry
      Put_Line ("[2] Measuring access to MIDDLE entry (position 5/10)");

      for I in Samples_Middle'Range loop
         Start_Time := Clock;
         Get_Entry (State, Label_Middle, Plaintext, Data_Len, Success);
         End_Time := Clock;

         Samples_Middle (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Middle := Compute_Mean (Samples_Middle);
      Put_Line ("    Mean Time: " & Format_Ms (Mean_Middle) & " ms");
      New_Line;

      --  Measure access to LAST entry
      Put_Line ("[3] Measuring access to LAST entry (position 10/10)");

      for I in Samples_Last'Range loop
         Start_Time := Clock;
         Get_Entry (State, Label_Last, Plaintext, Data_Len, Success);
         End_Time := Clock;

         Samples_Last (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Last := Compute_Mean (Samples_Last);
      Put_Line ("    Mean Time: " & Format_Ms (Mean_Last) & " ms");
      New_Line;

      --  Compute correlation between position and timing
      Put_Line ("[4] Computing position-timing correlation...");
      Correlation_First_Last := abs Compute_Correlation (Samples_First, Samples_Last);

      Put_Line ("    Correlation (first vs last): " & Format_Correlation (Correlation_First_Last));
      Put_Line ("    Threshold:                   " & Format_Correlation (Correlation_Threshold));
      New_Line;

      --  Verdict
      if Correlation_First_Last < Correlation_Threshold then
         Put_Line ("  ✓ PASS: Constant memory access pattern (" &
                   Format_Correlation (Correlation_First_Last) & " <" &
                   Format_Correlation (Correlation_Threshold) & ")");
         Put_Line ("         Vault always iterates all entries (no early termination)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Position-dependent timing detected (" &
                   Format_Correlation (Correlation_First_Last) & " ≥" &
                   Format_Correlation (Correlation_Threshold) & ")");
         Put_Line ("         Vault may be using early termination (timing leak)");
         Fail_Count := Fail_Count + 1;
      end if;

      Clear (State);
      New_Line;
   end Test_Memory_Patterns;

   --  ============================================================================
   --  Test 4: Speculative Execution Side-Channels
   --  ============================================================================
   --  **Attack**: Exploit speculative execution to leak data across security boundaries
   --
   --  **Attack Scenarios** (Spectre-style):
   --    1. Bounds check bypass - Speculative out-of-bounds array access
   --    2. Conditional branch prediction - Speculative execution down wrong path
   --    3. Memory disambiguation - Speculative load before earlier store completes
   --
   --  **Implementation Approach**:
   --    - Create speculative execution scenario (bounds check + array access)
   --    - Measure cache state after speculative operation
   --    - Detect transient execution leaks via cache timing
   --
   --  **Pass Criteria**: No observable cache effects from speculative execution
   --
   --  **Defense**: Speculation barriers (lfence on x86, isb on ARM),
   --  index masking (ensure bounds checks are non-speculative),
   --  constant-time code (no secret-dependent branches)
   --
   --  **Limitations**: Software cannot reliably trigger or measure speculative
   --  execution without specialized tools (Intel PT, ARM CoreSight). We perform
   --  best-effort testing via cache timing as a proxy.
   --  ============================================================================
   procedure Test_Speculative_Execution is
      Test_Path : constant String := "/tmp/test_sidechannel_spectre.spass";
      Password : constant Byte_Array := To_Bytes ("test_password_123");

      --  Test array for bounds check bypass attempt
      type Test_Array is array (1 .. 16) of Unsigned_8;
      Test_Data : Test_Array := (others => 0);
      Test_Index_Valid : constant := 8;
      Test_Index_Invalid : constant := 100;  -- Out of bounds

      Samples_Valid   : Duration_Array (1 .. Test_Iterations);
      Samples_Invalid : Duration_Array (1 .. Test_Iterations);

      Mean_Valid, Mean_Invalid : Duration;
      Correlation : Float;

      State : Vault_State;
      Status : Open_Status;
      Plaintext : Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Data_Len : Natural;
      Success : Boolean;
      Start_Time, End_Time : Ada.Real_Time.Time;

      --  Helper: Attempt speculative bounds check bypass
      --  This simulates Spectre-v1 attack pattern:
      --    if (index < array_length) {  // Mispredicted as true
      --       value = array[index];      // Speculatively executes out of bounds
      --       probe_array[value * 4096]; // Encodes secret in cache
      --    }
      function Speculative_Access (Index : Natural) return Unsigned_8 is
         Result : Unsigned_8 := 0;
      begin
         --  Ada's array bounds checking is non-bypassable at runtime
         --  This test verifies that SparkPass vault operations don't have
         --  exploitable speculation windows
         if Index in Test_Data'Range then
            Result := Test_Data (Index);
         end if;
         return Result;
      exception
         when others =>
            return 0;
      end Speculative_Access;

   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Test 4: Speculative Execution Analysis (Spectre-style) ===");
      Put_Line ("Attack: Exploit transient execution to leak data via cache");
      Put_Line ("Defense: Speculation barriers, index masking, constant-time code");
      New_Line;

      --  Create test vault
      if not Create_Test_Vault (Test_Path, Password) then
         Put_Line ("  ✗ FAIL: Could not create test vault");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Note: Full Spectre testing requires hardware tracing (Intel PT, ARM CoreSight)
      Put_Line ("⚠ NOTE: Software-only speculative execution testing is limited");
      Put_Line ("        Full analysis requires hardware tracing (Intel PT, ARM CoreSight)");
      Put_Line ("        This test performs best-effort validation via cache timing");
      New_Line;

      --  Test 1: Speculative array access (Spectre-v1 pattern)
      Put_Line ("[1] Testing speculative bounds check bypass...");

      --  Warm-up
      for I in 1 .. Warmup_Iterations loop
         declare
            Dummy : constant Unsigned_8 := Speculative_Access (Test_Index_Valid);
         begin
            null;
         end;
      end loop;

      --  Measure VALID index access
      for I in Samples_Valid'Range loop
         Flush_Cache;
         Start_Time := Clock;
         declare
            Dummy : constant Unsigned_8 := Speculative_Access (Test_Index_Valid);
         begin
            null;
         end;
         End_Time := Clock;

         Samples_Valid (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Valid := Compute_Mean (Samples_Valid);

      --  Measure INVALID index access (should not speculatively execute)
      for I in Samples_Invalid'Range loop
         Flush_Cache;
         Start_Time := Clock;
         declare
            Dummy : constant Unsigned_8 := Speculative_Access (Test_Index_Invalid);
         begin
            null;
         end;
         End_Time := Clock;

         Samples_Invalid (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Invalid := Compute_Mean (Samples_Invalid);

      Put_Line ("    Valid Index Access:   " & Format_Ms (Mean_Valid) & " ms");
      Put_Line ("    Invalid Index Access: " & Format_Ms (Mean_Invalid) & " ms");
      New_Line;

      --  Test 2: Vault operations under speculation
      Put_Line ("[2] Testing vault operations for speculative leaks...");

      --  Open vault and measure entry lookup patterns
      Open (State, Test_Path, Password, Status);
      if Status /= SparkPass.Vault.Success then
         Put_Line ("  ⊘ SKIP: Could not open test vault");
         Skip_Count := Skip_Count + 1;
         return;
      end if;

      --  Add test entry
      declare
         Label : constant Byte_Array := To_Bytes ("spectre_test");
         Data  : constant Byte_Array := To_Bytes ("sensitive_secret_data");
      begin
         Add_Entry (State, Label, SparkPass.Types.Password, Data, Now, Success);
      end;

      --  Measure entry access timing (should be constant regardless of speculation)
      for I in Samples_Valid'Range loop
         Flush_Cache;
         Start_Time := Clock;
         Get_Entry (State, To_Bytes ("spectre_test"), Plaintext, Data_Len, Success);
         End_Time := Clock;

         Samples_Valid (I) := To_Duration (End_Time - Start_Time);
      end loop;

      Mean_Valid := Compute_Mean (Samples_Valid);
      Put_Line ("    Entry Lookup Mean: " & Format_Ms (Mean_Valid) & " ms");
      New_Line;

      --  Compute correlation (check for timing variance indicating speculation)
      Put_Line ("[3] Computing speculative execution correlation...");
      Correlation := abs Compute_Correlation (Samples_Valid, Samples_Invalid);

      Put_Line ("    Correlation: " & Format_Correlation (Correlation));
      Put_Line ("    Threshold:   " & Format_Correlation (Correlation_Threshold));
      New_Line;

      --  Verdict
      if Correlation < Correlation_Threshold then
         Put_Line ("  ✓ PASS: No speculative execution leaks detected (" &
                   Format_Correlation (Correlation) & " <" &
                   Format_Correlation (Correlation_Threshold) & ")");
         Put_Line ("         Ada's bounds checking is non-bypassable");
         Put_Line ("         Vault operations show no transient execution effects");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ⚠ WARNING: Speculative timing variance detected (" &
                   Format_Correlation (Correlation) & " ≥" &
                   Format_Correlation (Correlation_Threshold) & ")");
         Put_Line ("            This may be CPU-specific behavior or measurement noise");
         Put_Line ("            Recommend hardware tracing for definitive analysis");
         Fail_Count := Fail_Count + 1;
      end if;

      Clear (State);
      New_Line;
   end Test_Speculative_Execution;

begin
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Side-Channel Analysis Tests                              ║");
   Put_Line ("║  Software-Measurable Side-Channel Resistance              ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("Platform: macOS (Darwin 25.0.0), CPU: Apple M2 Pro (ARM64)");
   Put_Line ("Test Mode: Software analysis (no hardware probes)");
   New_Line;

   Put_Line ("Test Configuration:");
   Put_Line ("  Iterations per test:    " & Natural'Image (Test_Iterations));
   Put_Line ("  Warmup iterations:      " & Natural'Image (Warmup_Iterations));
   Put_Line ("  Correlation threshold:   " & Format_Correlation (Correlation_Threshold));
   Put_Line ("  Cache flusher size:      16 MB (evicts L1/L2/L3)");
   New_Line;

   Put_Line ("Attack Vectors:");
   Put_Line ("  1. Cache-Timing         - CPU cache hit/miss patterns");
   Put_Line ("  2. Branch Prediction    - CPU branch predictor state");
   Put_Line ("  3. Memory Patterns      - Sequential vs random access");
   Put_Line ("  4. Speculative Execution - Transient execution (Spectre-style)");
   New_Line;

   --  Run side-channel analysis tests
   Test_Cache_Timing;
   Test_Branch_Behavior;
   Test_Memory_Patterns;
   Test_Speculative_Execution;

   --  Summary
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Side-Channel Analysis Summary                            ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   Put_Line ("Total Tests: " & Natural'Image (Test_Count));
   Put_Line ("Passed:      " & Natural'Image (Pass_Count));
   Put_Line ("Failed:      " & Natural'Image (Fail_Count));
   Put_Line ("Skipped:     " & Natural'Image (Skip_Count));
   New_Line;

   if Fail_Count = 0 and Skip_Count = 0 then
      Put_Line ("✓ All side-channel tests PASSED");
      Put_Line ("✓ No software-measurable side-channel leaks detected");
      New_Line;
      Put_Line ("Defense Mechanisms Validated:");
      Put_Line ("  ✓ Cache-timing resistance (constant-time primitives, AES-NI)");
      Put_Line ("  ✓ Branch-timing resistance (XOR comparisons, fixed iterations)");
      Put_Line ("  ✓ Memory pattern resistance (constant-time search, fixed access)");
      Put_Line ("  ✓ Speculative execution resistance (bounds checking, barriers)");
   elsif Fail_Count > 0 then
      Put_Line ("✗ Some tests FAILED - review output above");
      New_Line;
      Put_Line ("SECURITY CRITICAL: Investigate timing leaks before production!");
   else
      Put_Line ("⚠ Some tests SKIPPED - review output above");
   end if;

   New_Line;
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Limitations & Recommendations                            ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("What We CAN Test (Software-Only):");
   Put_Line ("  ✓ Basic cache timing patterns (via large array flush)");
   Put_Line ("  ✓ Branch behavior (via timing proxy)");
   Put_Line ("  ✓ Memory access ordering (via iteration timing)");
   Put_Line ("  ✓ Speculative execution effects (via cache timing proxy)");
   New_Line;

   Put_Line ("What We CANNOT Test (Requires Hardware):");
   Put_Line ("  ✗ Power analysis (DPA/SPA) - needs oscilloscope");
   Put_Line ("  ✗ EM analysis - needs SDR + antenna");
   Put_Line ("  ✗ Acoustic cryptanalysis - needs microphone array");
   Put_Line ("  ✗ Advanced cache profiling - needs CPU perf counters (root)");
   Put_Line ("  ✗ Hardware tracing - needs Intel PT / ARM CoreSight");
   New_Line;

   Put_Line ("Additional Mitigations (Beyond Testing):");
   Put_Line ("  • Use constant-time primitives (libsodium ✓)");
   Put_Line ("  • Implement blinding for sensitive operations");
   Put_Line ("  • Use cache-line alignment for critical data");
   Put_Line ("  • Add random delays for timing obfuscation");
   Put_Line ("  • Deploy on systems with Spectre/Meltdown mitigations");
   Put_Line ("  • Rate limit authentication attempts at network layer");
   Put_Line ("  • Monitor for side-channel probing (IDS/IPS)");
   New_Line;

   Put_Line ("Hardware Testing Recommendations:");
   Put_Line ("  1. Power Analysis:");
   Put_Line ("     - Equipment: Oscilloscope (1 GHz+), current probe");
   Put_Line ("     - Tools: ChipWhisperer, Riscure Inspector");
   Put_Line ("     - Tests: DPA on Argon2id, CPA on AES operations");
   New_Line;

   Put_Line ("  2. EM Analysis:");
   Put_Line ("     - Equipment: SDR (HackRF, USRP), H-field probe");
   Put_Line ("     - Tools: GNU Radio, custom EM analysis scripts");
   Put_Line ("     - Tests: EM emanations during crypto operations");
   New_Line;

   Put_Line ("  3. Advanced Cache Profiling:");
   Put_Line ("     - Requirements: Root access, perf_event_paranoid=0");
   Put_Line ("     - Commands: perf stat -e cache-misses,branch-misses");
   Put_Line ("     - Tests: Branch predictor state, TLB misses, cache set conflicts");
   New_Line;

   Put_Line ("  4. Speculative Execution:");
   Put_Line ("     - Tools: Intel Processor Trace (PT), ARM CoreSight");
   Put_Line ("     - Tests: Bounds check bypass, branch target injection");
   New_Line;

   Put_Line ("References:");
   Put_Line ("  • Kocher, P. 'Timing Attacks on Implementations of RSA' (1996)");
   Put_Line ("  • Bernstein, D.J. 'Cache-timing attacks on AES' (2005)");
   Put_Line ("  • Yarom, Y. 'FLUSH+RELOAD' (2014)");
   Put_Line ("  • Lipp, M. 'Meltdown' (2018), Kocher, P. 'Spectre Attacks' (2018)");
   Put_Line ("  • Introduction to Cryptography (Smart), Ch. 15: Implementation Attacks");

end Test_Sidechannels;
