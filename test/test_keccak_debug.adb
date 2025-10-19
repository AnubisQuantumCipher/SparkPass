--  Minimal Keccak debug test to isolate the NIST vector failure
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Keccak; use SparkPass.Crypto.Keccak;

procedure Test_Keccak_Debug is

   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Put_Hex;

   procedure Dump_Bytes (Label : String; Data : Byte_Array) is
   begin
      Put (Label & ": ");
      for I in Data'Range loop
         Put_Hex (Data (I));
         if I < Data'Last then
            Put (" ");
         end if;
      end loop;
      New_Line;
   end Dump_Bytes;

begin
   Put_Line ("========================================");
   Put_Line ("Keccak Debug: SHA3-256 Empty String");
   Put_Line ("========================================");
   New_Line;

   --  Test SHA3-256 of empty string
   --  Expected: a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a
   declare
      Input  : Byte_Array (1 .. 0);  --  Empty
      Output : SHA3_256_Digest;
   begin
      Put_Line ("Input: (empty string)");
      Put_Line ("Expected output: a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a");
      New_Line;

      SHA3_256_Hash (Input, Output);

      Put_Line ("Actual output:");
      Dump_Bytes ("  ", Output);
      New_Line;

      --  Check individual bytes
      Put_Line ("Byte-by-byte comparison:");
      Put_Line ("  Expected[0] = 0xa7, Got[0] = 0x" & Integer'Image (Integer (Output (1))));
      Put_Line ("  Expected[1] = 0xff, Got[1] = 0x" & Integer'Image (Integer (Output (2))));
      Put_Line ("  Expected[2] = 0xc6, Got[2] = 0x" & Integer'Image (Integer (Output (3))));
      Put_Line ("  Expected[3] = 0xf8, Got[3] = 0x" & Integer'Image (Integer (Output (4))));
   end;

   New_Line;
   Put_Line ("========================================");
   Put_Line ("Keccak Debug: SHA3-256 of 'abc'");
   Put_Line ("========================================");
   New_Line;

   --  Test SHA3-256 of "abc"
   --  Expected: 3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532
   declare
      Input  : constant Byte_Array := (97, 98, 99);  --  "abc"
      Output : SHA3_256_Digest;
   begin
      Put_Line ("Input: 'abc' (61 62 63)");
      Put_Line ("Expected output: 3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532");
      New_Line;

      SHA3_256_Hash (Input, Output);

      Put_Line ("Actual output:");
      Dump_Bytes ("  ", Output);
      New_Line;

      --  Check first few bytes
      Put_Line ("First 4 bytes:");
      Put_Line ("  Expected: 3a 98 5d a7");
      Put ("  Got:      ");
      Put_Hex (Output (1)); Put (" ");
      Put_Hex (Output (2)); Put (" ");
      Put_Hex (Output (3)); Put (" ");
      Put_Hex (Output (4));
      New_Line;
   end;

end Test_Keccak_Debug;
