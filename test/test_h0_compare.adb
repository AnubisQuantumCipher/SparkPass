--  Test H0 computation directly against reference
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Blake2b; use SparkPass.Crypto;

procedure Test_H0_Compare is

   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Put_Hex;

   --  H0 input computed from Python script (80 bytes)
   H0_Input : constant Byte_Array := (
      16#01#, 16#00#, 16#00#, 16#00#,  -- p=1
      16#20#, 16#00#, 16#00#, 16#00#,  -- taglen=32
      16#00#, 16#40#, 16#00#, 16#00#,  -- m=16384
      16#04#, 16#00#, 16#00#, 16#00#,  -- t=4
      16#13#, 16#00#, 16#00#, 16#00#,  -- v=0x13
      16#02#, 16#00#, 16#00#, 16#00#,  -- y=2 (Argon2id)
      16#08#, 16#00#, 16#00#, 16#00#,  -- password length=8
      16#70#, 16#61#, 16#73#, 16#73#, 16#77#, 16#6f#, 16#72#, 16#64#,  -- "password"
      16#20#, 16#00#, 16#00#, 16#00#,  -- salt length=32
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  -- "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#,  -- "SOMESALT"
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  -- "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#,  -- "SOMESALT"
      16#00#, 16#00#, 16#00#, 16#00#,  -- key length=0
      16#00#, 16#00#, 16#00#, 16#00#   -- associated data length=0
   );

   H0_Output : Byte_Array (1 .. 64);

   Expected : constant String :=
      "6271e97ec9c549b58bfab47607c9714367bf06c44f94289f6e40424151cbb7f6" &
      "fc93d870ee15c4f7795699dd3049673e88513df61d6e6d077428bb4c5582a2d3";

begin
   Put_Line ("Testing H0 computation (Blake2b-512)");
   Put_Line ("Input: 80 bytes (Argon2id parameters + password + salt)");
   New_Line;

   Blake2b.Hash (
      Message => H0_Input,
      Output  => H0_Output
   );

   Put ("Got:      ");
   for B of H0_Output loop
      Put_Hex (B);
   end loop;
   New_Line;

   Put_Line ("Expected: " & Expected);
   New_Line;

   --  Compare
   declare
      Match : Boolean := True;
      Got_Str : String (1 .. 128);
      Idx : Positive := 1;
   begin
      for B of H0_Output loop
         declare
            Hex : constant String := "0123456789abcdef";
            Hi : constant Natural := Natural (Shift_Right (B, 4));
            Lo : constant Natural := Natural (B and 16#0F#);
         begin
            Got_Str (Idx) := Hex (Hi + 1);
            Got_Str (Idx + 1) := Hex (Lo + 1);
            Idx := Idx + 2;
         end;
      end loop;

      if Got_Str = Expected then
         Put_Line ("PASS: H0 computation matches reference!");
      else
         Put_Line ("FAIL: H0 computation does NOT match!");
         Match := False;
      end if;
   end;

end Test_H0_Compare;
