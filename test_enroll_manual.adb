with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Interfaces;
with SparkPass.Vault; use type SparkPass.Vault.Open_Status; use type SparkPass.Vault.Save_Status;
with SparkPass.Crypto.Random;
with SparkPass.Types; use SparkPass.Types;

procedure Test_Enroll_Manual is
   Vault_State : SparkPass.Vault.Vault_State;
   Open_Status : SparkPass.Vault.Open_Status;
   Save_Status : SparkPass.Vault.Save_Status;
   Enroll_Success : Boolean := False;

   Device_Secret : Key_Array := (others => 0);
   Password : constant Byte_Array := (
      Byte_Array'(Character'Pos ('t'), Character'Pos ('e'), Character'Pos ('s'), Character'Pos ('t'),
                  Character'Pos ('_'), Character'Pos ('p'), Character'Pos ('a'), Character'Pos ('s'),
                  Character'Pos ('s'), Character'Pos ('w'), Character'Pos ('o'), Character'Pos ('r'),
                  Character'Pos ('d'), Character'Pos ('_'), Character'Pos ('1'), Character'Pos ('2'),
                  Character'Pos ('3'), Character'Pos ('4'), Character'Pos ('5')));

   Vault_Path : constant String := "test_vault.spass";

   function Timestamp return U64 is
      Now : constant Time := Clock;
      Epoch : constant Time := Time_Of (Year => 1970, Month => 1, Day => 1, Seconds => 0.0);
      Time_Delta : constant Duration := Now - Epoch;
   begin
      if Time_Delta <= 0.0 then
         return 0;
      end if;
      declare
         Elapsed_Seconds : constant Long_Long_Integer := Long_Long_Integer (Time_Delta);
      begin
         if Elapsed_Seconds <= 0 then
            return 0;
         end if;
         return U64 (Interfaces.Unsigned_64 (Elapsed_Seconds));
      end;
   end Timestamp;

begin
   Put_Line ("Manual Touch ID Enrollment Test");
   Put_Line ("================================");
   Put_Line ("");

   --  Step 1: Open vault
   Put_Line ("Step 1: Opening vault...");
   SparkPass.Vault.Clear (Vault_State);
   SparkPass.Vault.Open (Vault_State, Vault_Path, Password, Open_Status);

   if Open_Status /= SparkPass.Vault.Success then
      Put_Line ("ERROR: Failed to open vault!");
      case Open_Status is
         when SparkPass.Vault.Authentication_Failed =>
            Put_Line ("  Reason: Incorrect password");
         when SparkPass.Vault.Io_Error =>
            Put_Line ("  Reason: I/O error");
         when SparkPass.Vault.Format_Error =>
            Put_Line ("  Reason: Format error");
         when SparkPass.Vault.Integrity_Error =>
            Put_Line ("  Reason: Integrity error");
         when others =>
            null;
      end case;
      return;
   end if;
   Put_Line ("  Vault opened successfully");
   Put_Line ("");

   --  Step 2: Generate device secret
   Put_Line ("Step 2: Generating device secret...");
   SparkPass.Crypto.Random.Fill (Device_Secret);
   Put_Line ("  Device secret generated");
   Put_Line ("");

   --  Step 3: Enroll Touch ID
   Put_Line ("Step 3: Enrolling Touch ID...");
   SparkPass.Vault.Enroll_Touch_ID (Vault_State, Device_Secret, Timestamp, Enroll_Success);

   if not Enroll_Success then
      Put_Line ("ERROR: Enrollment failed!");
      SparkPass.Vault.Clear (Vault_State);
      return;
   end if;
   Put_Line ("  Enrollment successful");
   Put ("  Has_Wrap_D flag: ");
   if Vault_State.Header.Has_Wrap_D then
      Put_Line ("TRUE");
   else
      Put_Line ("FALSE");
   end if;
   Put_Line ("");

   --  Step 4: Save vault
   Put_Line ("Step 4: Saving vault...");
   SparkPass.Vault.Save (Vault_State, Vault_Path, Save_Status);

   if Save_Status /= SparkPass.Vault.Saved then
      Put_Line ("ERROR: Failed to save vault!");
      SparkPass.Vault.Clear (Vault_State);
      return;
   end if;
   Put_Line ("  Vault saved successfully");
   Put_Line ("");

   --  Step 5: Verify enrollment by checking file
   Put_Line ("Step 5: Verifying enrollment...");
   declare
      Is_Enrolled : Boolean;
   begin
      Is_Enrolled := SparkPass.Vault.Is_Touch_ID_Enrolled (Vault_Path);
      Put ("  Is_Touch_ID_Enrolled result: ");
      if Is_Enrolled then
         Put_Line ("TRUE");
      else
         Put_Line ("FALSE");
      end if;
   end;

   --  Cleanup
   SparkPass.Vault.Clear (Vault_State);

   Put_Line ("");
   Put_Line ("Test complete!");

end Test_Enroll_Manual;
