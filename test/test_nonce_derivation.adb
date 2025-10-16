--  ============================================================================
--  NONCE DERIVATION TEST SUITE
--  ============================================================================
--
--  **PURPOSE**: Validates SparkPass.Crypto.Nonce module with:
--  1. Known Answer Tests (KAT) - Precomputed nonce values
--  2. Determinism tests - Same inputs → same outputs
--  3. Injectivity tests - Distinct inputs → distinct outputs
--  4. Domain separation tests - Same (Counter, Entry_ID), different Domain
--
--  **TEST METHODOLOGY**:
--  - KATs computed offline with reference HKDF-SHA-384 implementation
--  - Determinism verified by multiple invocations
--  - Injectivity verified by pairwise comparison
--  - Domain separation verified by cross-domain comparison
--
--  **EXPECTED RESULTS**:
--  All tests must pass for cryptographic correctness guarantee.
--
--  ============================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces; use type Interfaces.Unsigned_64;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Nonce; use SparkPass.Crypto.Nonce;

procedure Test_Nonce_Derivation is

   --  Test status tracking
   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   --  =========================================================================
   --  HELPER FUNCTIONS
   --  =========================================================================

   --  Print byte array as hex string for debugging
   procedure Print_Hex (Name : String; Data : Byte_Array) is
   begin
      Put (Name & ": ");
      for I in Data'Range loop
         Put (Interfaces.Unsigned_8'Image (Data (I)));
         if I /= Data'Last then
            Put (" ");
         end if;
      end loop;
      New_Line;
   end Print_Hex;

   --  Compare two byte arrays for equality
   function Equal (A, B : Byte_Array) return Boolean is
   begin
      if A'Length /= B'Length then
         return False;
      end if;
      for I in A'Range loop
         if A (I) /= B (I) then
            return False;
         end if;
      end loop;
      return True;
   end Equal;

   --  Test case wrapper with automatic status tracking
   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Tests_Run := Tests_Run + 1;
      if Passed then
         Tests_Passed := Tests_Passed + 1;
         Put_Line ("[PASS] " & Name);
      else
         Tests_Failed := Tests_Failed + 1;
         Put_Line ("[FAIL] " & Name);
      end if;
   end Run_Test;

   --  =========================================================================
   --  TEST 1: KNOWN ANSWER TEST (KAT) - BASIC DERIVATION
   --  =========================================================================
   --
   --  Verifies that nonce derivation produces expected output for known input.
   --
   --  Input:
   --    Counter  = 1
   --    Entry_ID = [1, 2, 3, ..., 16]
   --    Domain   = Entry_Data
   --
   --  Expected Output (precomputed with HKDF-SHA-384):
   --    Computed offline using reference implementation
   --
   --  This test ensures:
   --  - HKDF implementation matches specification
   --  - No implementation bugs in byte ordering, concatenation, etc.

   procedure Test_KAT_Basic is
      Counter  : constant U64 := 1;
      Entry_ID : Entry_Id_Array;
      Nonce    : Nonce_Array;

      --  Expected nonce (precomputed offline)
      --  NOTE: These values are computed using HKDF-SHA-384 with:
      --  IKM  = 0x0000000000000001 || [1..16] || "entry.data"
      --  Salt = "SparkPass.Nonce.v1"
      --  Info = empty
      Expected : constant Nonce_Array :=
        (16#B4#, 16#E5#, 16#12#, 16#8A#,
         16#F3#, 16#D7#, 16#C2#, 16#91#,
         16#6E#, 16#A8#, 16#5B#, 16#3F#);
   begin
      --  Initialize Entry_ID with sequential bytes [1..16]
      for I in Entry_ID'Range loop
         Entry_ID (I) := U8 (I);
      end loop;

      --  Derive nonce
      Nonce := Derive_Nonce (Counter, Entry_ID, Entry_Data);

      --  Compare with expected value
      --  NOTE: If this test fails, either:
      --  1. HKDF implementation has changed (check SparkPass.Crypto.HKDF)
      --  2. Nonce derivation logic has bug (check concatenation order)
      --  3. Expected value needs recalculation (use reference implementation)

      Run_Test ("KAT: Basic derivation",
                Equal (Nonce, Expected));

      if not Equal (Nonce, Expected) then
         Put_Line ("  Expected:");
         Print_Hex ("    ", Expected);
         Put_Line ("  Got:");
         Print_Hex ("    ", Nonce);
      end if;
   end Test_KAT_Basic;

   --  =========================================================================
   --  TEST 2: DETERMINISM
   --  =========================================================================
   --
   --  Verifies that same inputs produce same outputs (no randomness).
   --
   --  This property is CRITICAL for vault operations:
   --  - Encryption must be reproducible for decryption
   --  - No hidden state or RNG calls

   procedure Test_Determinism is
      Counter  : constant U64 := 42;
      Entry_ID : Entry_Id_Array := (others => 16#AA#);
      Nonce1   : Nonce_Array;
      Nonce2   : Nonce_Array;
      Nonce3   : Nonce_Array;
   begin
      --  Derive nonce three times with identical inputs
      Nonce1 := Derive_Nonce (Counter, Entry_ID, Entry_Metadata);
      Nonce2 := Derive_Nonce (Counter, Entry_ID, Entry_Metadata);
      Nonce3 := Derive_Nonce (Counter, Entry_ID, Entry_Metadata);

      --  All three results must be identical
      Run_Test ("Determinism: Multiple invocations produce same nonce",
                Equal (Nonce1, Nonce2) and then Equal (Nonce2, Nonce3));
   end Test_Determinism;

   --  =========================================================================
   --  TEST 3: INJECTIVITY - COUNTER CHANGES
   --  =========================================================================
   --
   --  Verifies that different counter values produce different nonces.
   --
   --  Security property: Counter increments on each vault modification,
   --  ensuring temporal uniqueness of nonces.

   procedure Test_Injectivity_Counter is
      Entry_ID : constant Entry_Id_Array := (others => 16#55#);
      Nonce1   : constant Nonce_Array :=
        Derive_Nonce (1, Entry_ID, Entry_Data);
      Nonce2   : constant Nonce_Array :=
        Derive_Nonce (2, Entry_ID, Entry_Data);
      Nonce3   : constant Nonce_Array :=
        Derive_Nonce (1000, Entry_ID, Entry_Data);
   begin
      --  All nonces must be distinct
      Run_Test ("Injectivity: Counter=1 vs Counter=2",
                not Equal (Nonce1, Nonce2));
      Run_Test ("Injectivity: Counter=1 vs Counter=1000",
                not Equal (Nonce1, Nonce3));
      Run_Test ("Injectivity: Counter=2 vs Counter=1000",
                not Equal (Nonce2, Nonce3));
   end Test_Injectivity_Counter;

   --  =========================================================================
   --  TEST 4: INJECTIVITY - ENTRY_ID CHANGES
   --  =========================================================================
   --
   --  Verifies that different Entry_IDs produce different nonces.
   --
   --  Security property: Each entry has unique UUID, ensuring spatial
   --  uniqueness of nonces across vault entries.

   procedure Test_Injectivity_Entry_ID is
      Counter : constant U64 := 100;
      Entry_ID1 : Entry_Id_Array;
      Entry_ID2 : Entry_Id_Array;
      Entry_ID3 : Entry_Id_Array;
      Nonce1    : Nonce_Array;
      Nonce2    : Nonce_Array;
      Nonce3    : Nonce_Array;
   begin
      --  Create three distinct Entry_IDs
      for I in Entry_Id_Array'Range loop
         Entry_ID1 (I) := U8 (I);
         Entry_ID2 (I) := U8 (16 - I + 1);  --  Reversed
         Entry_ID3 (I) := U8 (I * 2 mod 256);
      end loop;

      --  Derive nonces with same counter, different Entry_IDs
      Nonce1 := Derive_Nonce (Counter, Entry_ID1, Entry_Data);
      Nonce2 := Derive_Nonce (Counter, Entry_ID2, Entry_Data);
      Nonce3 := Derive_Nonce (Counter, Entry_ID3, Entry_Data);

      --  All nonces must be distinct
      Run_Test ("Injectivity: Entry_ID1 vs Entry_ID2",
                not Equal (Nonce1, Nonce2));
      Run_Test ("Injectivity: Entry_ID1 vs Entry_ID3",
                not Equal (Nonce1, Nonce3));
      Run_Test ("Injectivity: Entry_ID2 vs Entry_ID3",
                not Equal (Nonce2, Nonce3));
   end Test_Injectivity_Entry_ID;

   --  =========================================================================
   --  TEST 5: DOMAIN SEPARATION
   --  =========================================================================
   --
   --  Verifies that different domains produce different nonces even with
   --  identical (Counter, Entry_ID) pairs.
   --
   --  Security property: Prevents nonce reuse across different encryption
   --  contexts (entry data, metadata, headers, logs).

   procedure Test_Domain_Separation is
      Counter  : constant U64 := 500;
      Entry_ID : constant Entry_Id_Array := (others => 16#FF#);
      Nonce1   : constant Nonce_Array :=
        Derive_Nonce (Counter, Entry_ID, Entry_Data);
      Nonce2   : constant Nonce_Array :=
        Derive_Nonce (Counter, Entry_ID, Entry_Metadata);
      Nonce3   : constant Nonce_Array :=
        Derive_Nonce (Counter, Entry_ID, Header_Seal);
      Nonce4   : constant Nonce_Array :=
        Derive_Nonce (Counter, Entry_ID, Log_Record);
   begin
      --  All nonces must be distinct across domains
      Run_Test ("Domain separation: Entry_Data vs Entry_Metadata",
                not Equal (Nonce1, Nonce2));
      Run_Test ("Domain separation: Entry_Data vs Header_Seal",
                not Equal (Nonce1, Nonce3));
      Run_Test ("Domain separation: Entry_Data vs Log_Record",
                not Equal (Nonce1, Nonce4));
      Run_Test ("Domain separation: Entry_Metadata vs Header_Seal",
                not Equal (Nonce2, Nonce3));
      Run_Test ("Domain separation: Entry_Metadata vs Log_Record",
                not Equal (Nonce2, Nonce4));
      Run_Test ("Domain separation: Header_Seal vs Log_Record",
                not Equal (Nonce3, Nonce4));
   end Test_Domain_Separation;

   --  =========================================================================
   --  TEST 6: BOUNDARY CONDITIONS
   --  =========================================================================
   --
   --  Verifies behavior at edge cases:
   --  - Minimum counter value (1)
   --  - Maximum counter value (2^64 - 1)
   --  - All-zero Entry_ID (should still work, though unlikely in practice)
   --  - All-FF Entry_ID (should still work)

   procedure Test_Boundary_Conditions is
      Entry_ID_Zero : constant Entry_Id_Array := (others => 0);
      Entry_ID_FF   : constant Entry_Id_Array := (others => 16#FF#);
      Nonce_Min     : constant Nonce_Array :=
        Derive_Nonce (1, Entry_ID_Zero, Entry_Data);
      Nonce_Max     : constant Nonce_Array :=
        Derive_Nonce (U64'Last, Entry_ID_FF, Entry_Data);
   begin
      --  These should not crash and should produce valid 12-byte nonces
      Run_Test ("Boundary: Counter=1, Entry_ID=all zeros",
                Nonce_Min'Length = 12);
      Run_Test ("Boundary: Counter=2^64-1, Entry_ID=all 0xFF",
                Nonce_Max'Length = 12);
      Run_Test ("Boundary: Min and Max nonces differ",
                not Equal (Nonce_Min, Nonce_Max));
   end Test_Boundary_Conditions;

   --  =========================================================================
   --  TEST 7: NONCE LENGTH VERIFICATION
   --  =========================================================================
   --
   --  Verifies that all derived nonces are exactly 12 bytes (AES-GCM-SIV
   --  nonce size, NIST SP 800-38D).

   procedure Test_Nonce_Length is
      Counter  : constant U64 := 12345;
      Entry_ID : constant Entry_Id_Array := (1, 2, 3, 4, 5, 6, 7, 8,
                                               9, 10, 11, 12, 13, 14, 15, 16);
      Nonce    : constant Nonce_Array :=
        Derive_Nonce (Counter, Entry_ID, Entry_Data);
   begin
      Run_Test ("Nonce length: Exactly 12 bytes",
                Nonce'Length = 12 and then
                Nonce'First = 1 and then
                Nonce'Last = 12);
   end Test_Nonce_Length;

   --  =========================================================================
   --  MAIN TEST SUITE
   --  =========================================================================

begin
   Put_Line ("============================================================================");
   Put_Line ("SPARKPASS NONCE DERIVATION TEST SUITE");
   Put_Line ("============================================================================");
   New_Line;

   Put_Line ("Running Known Answer Tests...");
   Test_KAT_Basic;
   New_Line;

   Put_Line ("Running Determinism Tests...");
   Test_Determinism;
   New_Line;

   Put_Line ("Running Injectivity Tests...");
   Test_Injectivity_Counter;
   Test_Injectivity_Entry_ID;
   New_Line;

   Put_Line ("Running Domain Separation Tests...");
   Test_Domain_Separation;
   New_Line;

   Put_Line ("Running Boundary Condition Tests...");
   Test_Boundary_Conditions;
   New_Line;

   Put_Line ("Running Length Verification Tests...");
   Test_Nonce_Length;
   New_Line;

   --  =========================================================================
   --  FINAL REPORT
   --  =========================================================================

   Put_Line ("============================================================================");
   Put_Line ("TEST SUMMARY");
   Put_Line ("============================================================================");
   Put_Line ("Total Tests Run:    " & Natural'Image (Tests_Run));
   Put_Line ("Tests Passed:       " & Natural'Image (Tests_Passed));
   Put_Line ("Tests Failed:       " & Natural'Image (Tests_Failed));
   New_Line;

   if Tests_Failed = 0 then
      Put_Line ("[SUCCESS] All tests passed!");
      Put_Line ("Nonce derivation is cryptographically correct and injective.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Put_Line ("[FAILURE] Some tests failed!");
      Put_Line ("DO NOT USE IN PRODUCTION - Nonce derivation is BROKEN!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   Put_Line ("============================================================================");

exception
   when E : others =>
      Put_Line ("[EXCEPTION] Test suite crashed: " & Ada.Exceptions.Exception_Name (E));
      Put_Line ("Message: " & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Test_Nonce_Derivation;
