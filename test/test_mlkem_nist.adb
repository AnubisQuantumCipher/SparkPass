pragma SPARK_Mode (Off);
--  NIST Known Answer Test for ML-KEM-1024 using official test vector
--  Test vector from: https://github.com/post-quantum-cryptography/KAT/blob/main/MLKEM/kat_MLKEM_1024.rsp

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Sequential_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Config;
with SparkPass.Crypto.MLKEM;

procedure Test_MLKEM_NIST is

   --  Helper: Convert hex string to byte array
   function Hex_To_Bytes (Hex : String) return Byte_Array is
      Result : Byte_Array (1 .. Hex'Length / 2);
      Idx : Positive := 1;

      function Hex_Char_To_U8 (C : Character) return U8 is
      begin
         case C is
            when '0' .. '9' => return U8 (Character'Pos (C) - Character'Pos ('0'));
            when 'a' .. 'f' => return U8 (Character'Pos (C) - Character'Pos ('a') + 10);
            when 'A' .. 'F' => return U8 (Character'Pos (C) - Character'Pos ('A') + 10);
            when others => raise Constraint_Error with "Invalid hex character: " & C;
         end case;
      end Hex_Char_To_U8;
   begin
      for I in Result'Range loop
         Result (I) := Hex_Char_To_U8 (Hex (Idx)) * 16 + Hex_Char_To_U8 (Hex (Idx + 1));
         Idx := Idx + 2;
      end loop;
      return Result;
   end Hex_To_Bytes;

   --  Helper: Load hex string from file
   function Load_Hex_File (Filename : String) return String is
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 10000);  -- Max line length
      Last : Natural;
   begin
      Open (File, In_File, Filename);
      Get_Line (File, Line, Last);
      Close (File);
      return Line (1 .. Last);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Load_Hex_File;

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

   --  Helper: Print bytes for debugging
   procedure Print_Bytes (Label : String; Data : Byte_Array; Show_Length : Positive := 32) is
   begin
      Put (Label & " (" & Natural'Image (Data'Length) & " bytes): ");
      for I in Data'First .. Integer'Min (Data'First + Show_Length - 1, Data'Last) loop
         Put (U8'Image (Data (I)));
         if I < Data'Last and I < Data'First + Show_Length - 1 then
            Put (" ");
         end if;
      end loop;
      if Data'Length > Show_Length then
         Put (" ...");
      end if;
      New_Line;
   end Print_Bytes;

   --  Test with NIST vector count=0
   procedure Test_NIST_Vector_0 is
      PK_Hex : constant String := Load_Hex_File ("mlkem_pk_0.hex");
      SK_Hex : constant String := Load_Hex_File ("mlkem_sk_0.hex");
      CT_Hex : constant String := Load_Hex_File ("mlkem_ct_0.hex");
      SS_Hex : constant String := Load_Hex_File ("mlkem_ss_0.hex");

      Public_Key : SparkPass.Crypto.MLKEM.Public_Key;
      Secret_Key : SparkPass.Crypto.MLKEM.Secret_Key;
      Ciphertext : SparkPass.Crypto.MLKEM.Ciphertext;
      Shared_Secret : SparkPass.Crypto.MLKEM.Shared_Key;
      Expected_SS : SparkPass.Crypto.MLKEM.Shared_Key;
      Success : Boolean;
   begin
      Put_Line ("=== NIST KAT Test Vector (count=0) ===");
      Put_Line ("Source: kat_MLKEM_1024.rsp from post-quantum-cryptography/KAT");
      New_Line;

      --  Parse test vector data
      Put_Line ("[1] Loading test vector data...");
      Public_Key := Hex_To_Bytes (PK_Hex);
      Secret_Key := Hex_To_Bytes (SK_Hex);
      Ciphertext := Hex_To_Bytes (CT_Hex);
      Expected_SS := Hex_To_Bytes (SS_Hex);

      Put_Line ("  ✓ Test vector loaded");
      Print_Bytes ("  Public Key ", Public_Key, 16);
      Print_Bytes ("  Secret Key ", Secret_Key, 16);
      Print_Bytes ("  Ciphertext ", Ciphertext, 16);
      Print_Bytes ("  Expected SS", Expected_SS);

      --  Perform decapsulation
      Put_Line ("[2] Performing ML-KEM-1024 decapsulation...");
      SparkPass.Crypto.MLKEM.Decapsulate (Secret_Key, Ciphertext, Shared_Secret, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Decapsulation failed");
         return;
      end if;

      Put_Line ("  ✓ Decapsulation succeeded");
      Print_Bytes ("  Got SS     ", Shared_Secret);

      --  Verify shared secret matches expected value
      Put_Line ("[3] Verifying against expected shared secret...");
      if Bytes_Equal (Shared_Secret, Expected_SS) then
         Put_Line ("  ✓ PASS: Shared secret matches NIST expected value!");
      else
         Put_Line ("  ✗ FAIL: Shared secret mismatch");
         Print_Bytes ("    Expected", Expected_SS);
         Print_Bytes ("    Got     ", Shared_Secret);
      end if;

      New_Line;
   end Test_NIST_Vector_0;

begin
   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  NIST KAT Test for ML-KEM-1024                            ║");
   Put_Line ("║  Using official test vectors from NIST PQC KAT repository ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("ML-KEM-1024 Parameters:");
   Put_Line ("  Public Key:    " & Positive'Image (SparkPass.Config.MLKem_Public_Key_Length) & " bytes");
   Put_Line ("  Secret Key:    " & Positive'Image (SparkPass.Config.MLKem_Secret_Key_Length) & " bytes");
   Put_Line ("  Ciphertext:    " & Positive'Image (SparkPass.Config.MLKem_Ciphertext_Length) & " bytes");
   Put_Line ("  Shared Secret: " & Positive'Image (SparkPass.Config.MLKem_Shared_Key_Length) & " bytes");
   New_Line;

   --  Run test
   Test_NIST_Vector_0;

   Put_Line ("╔════════════════════════════════════════════════════════════╗");
   Put_Line ("║  Test Complete                                             ║");
   Put_Line ("╚════════════════════════════════════════════════════════════╝");

end Test_MLKEM_NIST;