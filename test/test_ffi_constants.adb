pragma SPARK_Mode (Off);
with Ada.Text_IO; use Ada.Text_IO;
with System;
with Bindings.Keychain_Darwin; use Bindings.Keychain_Darwin;

procedure Test_FFI_Constants is
   use type System.Address;
begin
   Put_Line ("=== Testing FFI Constants ===");
   Put_Line ("");

   Put_Line ("[1] Testing kSecClass:");
   declare
      Addr : constant System.Address := System.Address (kSecClass);
   begin
      if Addr = System.Null_Address then
         Put_Line ("    ✗ kSecClass is NULL");
      else
         Put_Line ("    ✓ kSecClass is NOT NULL");
      end if;
   end;

   Put_Line ("[2] Testing kSecClassGenericPassword:");
   declare
      Addr : constant System.Address := System.Address (kSecClassGenericPassword);
   begin
      if Addr = System.Null_Address then
         Put_Line ("    ✗ kSecClassGenericPassword is NULL");
      else
         Put_Line ("    ✓ kSecClassGenericPassword is NOT NULL");
      end if;
   end;

   Put_Line ("[3] Testing kSecAttrAccessibleWhenUnlockedThisDeviceOnly:");
   declare
      Addr : constant System.Address := System.Address (kSecAttrAccessibleWhenUnlockedThisDeviceOnly);
   begin
      if Addr = System.Null_Address then
         Put_Line ("    ✗ kSecAttrAccessibleWhenUnlockedThisDeviceOnly is NULL");
      else
         Put_Line ("    ✓ kSecAttrAccessibleWhenUnlockedThisDeviceOnly is NOT NULL");
      end if;
   end;

   Put_Line ("[4] Testing kCFTypeDictionaryKeyCallBacks:");
   declare
      Addr : constant System.Address := kCFTypeDictionaryKeyCallBacks;
   begin
      if Addr = System.Null_Address then
         Put_Line ("    ✗ kCFTypeDictionaryKeyCallBacks is NULL");
      else
         Put_Line ("    ✓ kCFTypeDictionaryKeyCallBacks is NOT NULL");
      end if;
   end;

   Put_Line ("");
   Put_Line ("=== Done ===");
end Test_FFI_Constants;
