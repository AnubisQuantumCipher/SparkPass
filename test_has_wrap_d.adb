with Ada.Text_IO; use Ada.Text_IO;
with SparkPass.Types;
with SparkPass.Vault.Storage; use type SparkPass.Vault.Storage.Status;

procedure Test_Has_Wrap_D is
   Header : SparkPass.Types.Header;
   Entries : SparkPass.Types.Entry_Table;
   Count : SparkPass.Types.Entry_Count_Type := 0;
   Storage_Status : SparkPass.Vault.Storage.Status;
begin
   Put_Line ("Testing Has_Wrap_D directly...");

   --  Load test vault
   SparkPass.Vault.Storage.Load ("test_vault.spass", Header, Entries, Count, Storage_Status);

   if Storage_Status = SparkPass.Vault.Storage.Ok then
      Put ("Has_Wrap_D (direct check): ");
      if Header.Has_Wrap_D then
         Put_Line ("TRUE");
      else
         Put_Line ("FALSE");
      end if;
   else
      Put_Line ("Error loading vault");
   end if;
end Test_Has_Wrap_D;
