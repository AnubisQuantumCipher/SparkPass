with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with SparkPass.Vault;
with SparkPass.Platform.Keychain;
with Interfaces;

procedure Test_Enrollment_Status is
   use type Interfaces.Unsigned_64;

   Vault_Path : constant String := "test_vault.spass";
   Has_Wrap_D : Boolean;
   Has_Cached : Boolean;

begin
   Put_Line ("=== Testing Enrollment Status Logic ===");
   Put_Line ("");

   --  Test 1: Check if vault has Wrap D
   Put_Line ("Test 1: Checking if vault has Wrap D...");
   Has_Wrap_D := SparkPass.Vault.Is_Touch_ID_Enrolled (Vault_Path);
   Put_Line ("  Result: " & Boolean'Image (Has_Wrap_D));
   Put_Line ("");

   if not Has_Wrap_D then
      Put_Line ("FAIL: Wrap D not found in vault header");
      return;
   end if;

   --  Test 2: Check keychain entry
   Put_Line ("Test 2: Checking keychain entry...");
   Has_Cached := SparkPass.Platform.Keychain.Has_Cached_Key (Vault_Path);
   Put_Line ("  Has_Cached_Key: " & Boolean'Image (Has_Cached));
   Put_Line ("");

   if not Has_Cached then
      Put_Line ("FAIL: Keychain entry not found");
      return;
   end if;

   Put_Line ("");
   Put_Line ("SUCCESS: All checks passed - should show as Enrolled");
   Put_Line ("Note: Cache age/expiration is checked during actual retrieval with authentication");
end Test_Enrollment_Status;
