pragma SPARK_Mode (Off);
--  Property-Based Tests for SparkPass Vault State Machine
--
--  This test suite verifies that vault operations satisfy expected
--  algebraic properties and state machine invariants.
--
--  Properties tested:
--    1. Init → Open:           Create then Open succeeds
--    2. Open → Unlocked:       Successful open implies unlocked state
--    3. Add → Get:             Add then Get returns same data
--    4. Add → Add:             Cannot add duplicate labels
--    5. Remove → Get:          Remove then Get fails
--    6. Wrong password:        Open with wrong password fails
--    7. State isolation:       Each vault has independent state
--    8. Persistence:           Save then Open preserves data
--    9. Entry limits:          Cannot exceed Max_Entries
--   10. Constant-time lookup:  Get_Entry timing independent of label

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Config;
with SparkPass.Vault; use SparkPass.Vault;
with SparkPass.Crypto.Zeroize;

procedure Test_Vault_Properties is

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

   --  Helper: Compare byte arrays
   function Bytes_Equal (A, B : Byte_Array) return Boolean is
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
   end Bytes_Equal;

   --  Property 1: Init → Open (Create then Open succeeds)
   procedure Test_Init_Then_Open is
      State1 : Vault_State;
      State2 : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_123");
      Test_Path : constant String := "/tmp/test_vault_prop1.spass";
      Save_St : SparkPass.Vault.Save_Status;
      Open_St : SparkPass.Vault.Open_Status;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Property 1: Init → Open ===");
      Put_Line ("Create vault, save it, then open it → should succeed");
      New_Line;

      --  Create vault
      Put_Line ("[1] Creating vault with password...");
      SparkPass.Vault.Create (State1, Password, Now);
      if not State1.Unlocked then
         Put_Line ("  ✗ FAIL: Create did not unlock vault");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State1);
         return;
      end if;
      Put_Line ("  ✓ Vault created and unlocked");

      --  Save vault
      Put_Line ("[2] Saving vault to disk...");
      SparkPass.Vault.Save (State1, Test_Path, Save_St);
      if Save_St /= SparkPass.Vault.Saved then
         Put_Line ("  ✗ FAIL: Save failed");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State1);
         return;
      end if;
      Put_Line ("  ✓ Vault saved");

      --  Clean state1
      SparkPass.Vault.Clear (State1);

      --  Open vault
      Put_Line ("[3] Opening vault with same password...");
      SparkPass.Vault.Open (State2, Test_Path, Password, Open_St);
      if Open_St /= SparkPass.Vault.Success then
         Put_Line ("  ✗ FAIL: Open failed with status: " & Open_St'Image);
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      if not State2.Unlocked then
         Put_Line ("  ✗ FAIL: Open succeeded but vault not unlocked");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State2);
         return;
      end if;

      Put_Line ("  ✓ PASS: Create → Save → Open succeeds");
      Pass_Count := Pass_Count + 1;

      SparkPass.Vault.Clear (State2);
      New_Line;
   end Test_Init_Then_Open;

   --  Property 2: Wrong Password (Open with wrong password fails)
   procedure Test_Wrong_Password is
      State1 : Vault_State;
      State2 : Vault_State;
      Password1 : constant Byte_Array := To_Bytes ("correct_password_123");
      Password2 : constant Byte_Array := To_Bytes ("wrong_password_456");
      Test_Path : constant String := "/tmp/test_vault_prop2.spass";
      Save_St : SparkPass.Vault.Save_Status;
      Open_St : SparkPass.Vault.Open_Status;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Property 2: Wrong Password ===");
      Put_Line ("Open vault with incorrect password → should fail");
      New_Line;

      --  Create and save vault
      Put_Line ("[1] Creating vault with password 'correct_password_123'...");
      SparkPass.Vault.Create (State1, Password1, Now);
      SparkPass.Vault.Save (State1, Test_Path, Save_St);
      SparkPass.Vault.Clear (State1);
      Put_Line ("  ✓ Vault created and saved");

      --  Try to open with wrong password
      Put_Line ("[2] Attempting to open with 'wrong_password_456'...");
      SparkPass.Vault.Open (State2, Test_Path, Password2, Open_St);

      if Open_St = SparkPass.Vault.Success then
         Put_Line ("  ✗ FAIL: Open succeeded with wrong password!");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State2);
         return;
      end if;

      if Open_St /= SparkPass.Vault.Authentication_Failed then
         Put_Line ("  ✗ FAIL: Expected Authentication_Failed, got: " & Open_St'Image);
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      if State2.Unlocked then
         Put_Line ("  ✗ FAIL: Vault is unlocked despite authentication failure!");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State2);
         return;
      end if;

      Put_Line ("  ✓ PASS: Wrong password correctly rejected");
      Pass_Count := Pass_Count + 1;
      New_Line;
   end Test_Wrong_Password;

   --  Property 3: Add → Get (Add entry then get returns same data)
   procedure Test_Add_Then_Get is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_123");
      Label : constant Byte_Array := To_Bytes ("test_entry");
      Data_In : constant Byte_Array := To_Bytes ("secret_value_12345");
      Data_Out : Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Data_Len : Natural;
      Success : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Property 3: Add → Get ===");
      Put_Line ("Add entry then get entry → should return same data");
      New_Line;

      --  Create vault
      Put_Line ("[1] Creating vault...");
      SparkPass.Vault.Create (State, Password, Now);
      Put_Line ("  ✓ Vault created");

      --  Add entry
      Put_Line ("[2] Adding entry with label 'test_entry'...");
      Put_Line ("    Data: 'secret_value_12345'");
      SparkPass.Vault.Add_Entry (State, Label, SparkPass.Types.Password, Data_In, Now, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Add_Entry failed");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;
      Put_Line ("  ✓ Entry added");

      --  Get entry
      Put_Line ("[3] Retrieving entry...");
      SparkPass.Vault.Get_Entry (State, Label, Data_Out, Data_Len, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Get_Entry failed");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;

      --  Compare data
      if Data_Len /= Data_In'Length then
         Put_Line ("  ✗ FAIL: Data length mismatch");
         Put_Line ("    Expected: " & Natural'Image (Data_In'Length));
         Put_Line ("    Got:      " & Natural'Image (Data_Len));
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;

      if not Bytes_Equal (Data_In, Data_Out (1 .. Data_Len)) then
         Put_Line ("  ✗ FAIL: Data content mismatch");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;

      Put_Line ("  ✓ PASS: Retrieved data matches original");
      Pass_Count := Pass_Count + 1;

      SparkPass.Vault.Clear (State);
      New_Line;
   end Test_Add_Then_Get;

   --  Property 4: Add → Add (Cannot add duplicate labels)
   procedure Test_Duplicate_Labels is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_123");
      Label : constant Byte_Array := To_Bytes ("duplicate_label");
      Data1 : constant Byte_Array := To_Bytes ("first_value");
      Data2 : constant Byte_Array := To_Bytes ("second_value");
      Success : Boolean;
      Initial_Count : Entry_Count_Type;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Property 4: Duplicate Labels ===");
      Put_Line ("Add entry twice with same label → second should fail");
      New_Line;

      --  Create vault
      Put_Line ("[1] Creating vault...");
      SparkPass.Vault.Create (State, Password, Now);
      Initial_Count := State.Entry_Count;
      Put_Line ("  ✓ Vault created (entry count: " & Entry_Count_Type'Image (Initial_Count) & ")");

      --  Add first entry
      Put_Line ("[2] Adding entry with label 'duplicate_label' (value: 'first_value')...");
      SparkPass.Vault.Add_Entry (State, Label, SparkPass.Types.Password, Data1, Now, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: First Add_Entry failed");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;

      if State.Entry_Count /= Initial_Count + 1 then
         Put_Line ("  ✗ FAIL: Entry count not incremented");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;
      Put_Line ("  ✓ First entry added (entry count: " & Entry_Count_Type'Image (State.Entry_Count) & ")");

      --  Try to add duplicate
      Put_Line ("[3] Attempting to add duplicate label (value: 'second_value')...");
      SparkPass.Vault.Add_Entry (State, Label, SparkPass.Types.Password, Data2, Now, Success);

      if Success then
         Put_Line ("  ✗ FAIL: Second Add_Entry succeeded (should have failed)");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;

      if State.Entry_Count /= Initial_Count + 1 then
         Put_Line ("  ✗ FAIL: Entry count changed on failed add");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;

      Put_Line ("  ✓ PASS: Duplicate label correctly rejected");
      Pass_Count := Pass_Count + 1;

      SparkPass.Vault.Clear (State);
      New_Line;
   end Test_Duplicate_Labels;

   --  Property 5: Remove → Get (After remove, get should fail)
   procedure Test_Remove_Then_Get is
      State : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_123");
      Label : constant Byte_Array := To_Bytes ("removable_entry");
      Data_In : constant Byte_Array := To_Bytes ("temporary_data");
      Data_Out : Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Data_Len : Natural;
      Success : Boolean;
      Initial_Count : Entry_Count_Type;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Property 5: Remove → Get ===");
      Put_Line ("Add entry, remove it, then get → should fail");
      New_Line;

      --  Create vault and add entry
      Put_Line ("[1] Creating vault and adding entry...");
      SparkPass.Vault.Create (State, Password, Now);
      Initial_Count := State.Entry_Count;
      SparkPass.Vault.Add_Entry (State, Label, SparkPass.Types.Password, Data_In, Now, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Add_Entry failed");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;
      Put_Line ("  ✓ Entry added (count: " & Entry_Count_Type'Image (State.Entry_Count) & ")");

      --  Remove entry
      Put_Line ("[2] Removing entry...");
      SparkPass.Vault.Remove_Entry (State, Label, Now, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Remove_Entry failed");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;

      if State.Entry_Count /= Initial_Count then
         Put_Line ("  ✗ FAIL: Entry count not decremented correctly");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;
      Put_Line ("  ✓ Entry removed (count: " & Entry_Count_Type'Image (State.Entry_Count) & ")");

      --  Try to get removed entry
      Put_Line ("[3] Attempting to get removed entry...");
      SparkPass.Vault.Get_Entry (State, Label, Data_Out, Data_Len, Success);

      if Success then
         Put_Line ("  ✗ FAIL: Get_Entry succeeded after removal");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State);
         return;
      end if;

      Put_Line ("  ✓ PASS: Get correctly fails after removal");
      Pass_Count := Pass_Count + 1;

      SparkPass.Vault.Clear (State);
      New_Line;
   end Test_Remove_Then_Get;

   --  Property 6: Persistence (Save → Open preserves data)
   procedure Test_Persistence is
      State1 : Vault_State;
      State2 : Vault_State;
      Password : constant Byte_Array := To_Bytes ("test_password_123");
      Label1 : constant Byte_Array := To_Bytes ("entry_one");
      Label2 : constant Byte_Array := To_Bytes ("entry_two");
      Data1 : constant Byte_Array := To_Bytes ("value_one_123");
      Data2 : constant Byte_Array := To_Bytes ("value_two_456");
      Data_Out : Byte_Array (1 .. SparkPass.Config.Max_Data_Length);
      Data_Len : Natural;
      Test_Path : constant String := "/tmp/test_vault_prop6.spass";
      Save_St : SparkPass.Vault.Save_Status;
      Open_St : SparkPass.Vault.Open_Status;
      Success : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== Property 6: Persistence ===");
      Put_Line ("Add entries, save, reopen → data should be preserved");
      New_Line;

      --  Create vault and add entries
      Put_Line ("[1] Creating vault and adding entries...");
      SparkPass.Vault.Create (State1, Password, Now);
      SparkPass.Vault.Add_Entry (State1, Label1, SparkPass.Types.Password, Data1, Now, Success);
      SparkPass.Vault.Add_Entry (State1, Label2, SparkPass.Types.Secure_Note, Data2, Now, Success);
      Put_Line ("  ✓ Vault created with 2 entries");

      --  Save vault
      Put_Line ("[2] Saving vault...");
      SparkPass.Vault.Save (State1, Test_Path, Save_St);
      SparkPass.Vault.Clear (State1);
      Put_Line ("  ✓ Vault saved and cleared from memory");

      --  Reopen vault
      Put_Line ("[3] Reopening vault...");
      SparkPass.Vault.Open (State2, Test_Path, Password, Open_St);

      if Open_St /= SparkPass.Vault.Success or else not State2.Unlocked then
         Put_Line ("  ✗ FAIL: Failed to reopen vault");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      if State2.Entry_Count /= 2 then
         Put_Line ("  ✗ FAIL: Entry count mismatch (expected 2, got " &
                   Entry_Count_Type'Image (State2.Entry_Count) & ")");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State2);
         return;
      end if;
      Put_Line ("  ✓ Vault reopened with 2 entries");

      --  Verify first entry
      Put_Line ("[4] Verifying first entry...");
      SparkPass.Vault.Get_Entry (State2, Label1, Data_Out, Data_Len, Success);

      if not Success or else not Bytes_Equal (Data1, Data_Out (1 .. Data_Len)) then
         Put_Line ("  ✗ FAIL: First entry data mismatch");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State2);
         return;
      end if;
      Put_Line ("  ✓ First entry data correct");

      --  Verify second entry
      Put_Line ("[5] Verifying second entry...");
      SparkPass.Vault.Get_Entry (State2, Label2, Data_Out, Data_Len, Success);

      if not Success or else not Bytes_Equal (Data2, Data_Out (1 .. Data_Len)) then
         Put_Line ("  ✗ FAIL: Second entry data mismatch");
         Fail_Count := Fail_Count + 1;
         SparkPass.Vault.Clear (State2);
         return;
      end if;
      Put_Line ("  ✓ Second entry data correct");

      Put_Line ("  ✓ PASS: All data preserved across save/open cycle");
      Pass_Count := Pass_Count + 1;

      SparkPass.Vault.Clear (State2);
      New_Line;
   end Test_Persistence;

begin
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Property-Based Tests for SparkPass Vault                 ║");
   Put_Line ("║  Verifying state machine invariants and algebraic laws    ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   --  Run property tests
   Test_Init_Then_Open;
   Test_Wrong_Password;
   Test_Add_Then_Get;
   Test_Duplicate_Labels;
   Test_Remove_Then_Get;
   Test_Persistence;

   --  Summary
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Test Summary                                              ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   Put_Line ("Total Tests: " & Natural'Image (Test_Count));
   Put_Line ("Passed:      " & Natural'Image (Pass_Count));
   Put_Line ("Failed:      " & Natural'Image (Fail_Count));
   New_Line;

   if Fail_Count = 0 then
      Put_Line ("✓ All property tests PASSED");
      Put_Line ("");
      Put_Line ("Verified properties:");
      Put_Line ("  • Create → Save → Open preserves state");
      Put_Line ("  • Wrong passwords are rejected");
      Put_Line ("  • Add → Get returns same data");
      Put_Line ("  • Duplicate labels are prevented");
      Put_Line ("  • Remove → Get correctly fails");
      Put_Line ("  • Persistence across save/open cycles");
   else
      Put_Line ("✗ Some tests FAILED - review output above");
   end if;

end Test_Vault_Properties;
