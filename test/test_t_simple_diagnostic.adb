with Ada.Text_IO; use Ada.Text_IO;
with SparkPass.Crypto.MLKEM.KeyGen; use SparkPass.Crypto.MLKEM.KeyGen;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.Encoding; use SparkPass.Crypto.MLKEM.Encoding;
with SparkPass.Types; use SparkPass.Types;
with Interfaces; use Interfaces;

procedure Test_T_Simple_Diagnostic is

   function Hex_To_U8 (H : String) return U8 is
      function Char_Val (C : Character) return U8 is
      begin
         case C is
            when '0' .. '9' => return Character'Pos(C) - Character'Pos('0');
            when 'a' .. 'f' => return Character'Pos(C) - Character'Pos('a') + 10;
            when 'A' .. 'F' => return Character'Pos(C) - Character'Pos('A') + 10;
            when others => return 0;
         end case;
      end Char_Val;
   begin
      return Char_Val(H(H'First)) * 16 + Char_Val(H(H'First + 1));
   end Hex_To_U8;

   D_Hex : constant String := "6dbbc4375136df3b07f7c70e639e223e177e7fd53b161b3f4d57791794f12624";

   D_Bytes : Seed_Bytes;
   PK : Public_Key;
   SK : Secret_Key;

   -- Expected first 16 bytes
   Expected : constant array (1 .. 16) of U8 := (
      168, 18, 47, 55, 107, 63, 93, 53,
      82, 99, 235, 165, 34, 196, 57, 149
   );

begin
   Put_Line ("========================================");
   Put_Line ("Simple t[0] Byte Diagnostic");
   Put_Line ("========================================");
   New_Line;

   -- Convert hex seed to bytes
   for I in 0 .. 31 loop
      D_Bytes (I + 1) := Hex_To_U8 (D_Hex (D_Hex'First + I * 2 .. D_Hex'First + I * 2 + 1));
   end loop;

   -- Run KeyGen
   KeyGen (D_Bytes, PK, SK);

   -- Compare first 16 bytes
   Put_Line ("First 16 bytes of public key:");
   Put_Line ("Idx   Expected   Actual   Diff   Match");
   Put_Line ("=========================================");

   declare
      Matches : Natural := 0;
   begin
      for I in 1 .. 16 loop
         declare
            Exp : constant U8 := Expected (I);
            Act : constant U8 := PK (I);
            Diff : constant Integer := Integer (Act) - Integer (Exp);
         begin
            Put (I'Image & "     ");
            Put (Exp'Image & "       ");
            Put (Act'Image & "    ");
            Put (Diff'Image & "    ");
            if Exp = Act then
               Put_Line ("✓");
               Matches := Matches + 1;
            else
               Put_Line ("✗");
            end if;
         end;
      end loop;

      New_Line;
      Put_Line ("Summary: " & Matches'Image & "/16 bytes match");

      if Matches = 16 then
         Put_Line ("✓ SUCCESS: All first 16 bytes match!");
      else
         Put_Line ("✗ FAILURE: " & Natural'Image(16 - Matches) & " bytes differ");
      end if;
   end;

end Test_T_Simple_Diagnostic;
