--  Debug test to print intermediate block values
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id;

procedure Test_Argon2_Debug_Blocks is

   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Put_Hex;

   Password : constant Byte_Array := (
      16#70#, 16#61#, 16#73#, 16#73#, 16#77#, 16#6f#, 16#72#, 16#64#  -- "password"
   );

   Salt : constant Byte_Array (1 .. 32) := (
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  -- "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#,  -- "SOMESALT"
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  -- "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#   -- "SOMESALT"
   );

   Params : SparkPass.Crypto.Argon2id.Parameters;
   Output : SparkPass.Types.Key_Array := (others => 0);
   Success : Boolean;

begin
   Put_Line ("=== Argon2id Debug - Test Vector 1 ===");
   Put_Line ("Password: 'password'");
   Put_Line ("Salt: 'somesaltSOMESALTsomesaltSOMESALT'");
   Put_Line ("Parameters: t=4, m=16384, p=1");
   New_Line;

   Put_Line ("Expected output:");
   Put_Line ("dbda37811a190cf4dffda38f6aaeef2f2bb74c675d1c333512790d4d902107a3");
   New_Line;

   --  Set parameters
   Params.Iterations := 4;
   Params.Memory_Cost := 16384;
   Params.Parallelism := 1;
   Params.Salt := Salt;

   SparkPass.Crypto.Argon2id.Derive (
      Password => Password,
      Params   => Params,
      Output   => Output,
      Success  => Success
   );

   if Success then
      Put ("Got output:      ");
      for B of Output loop
         Put_Hex (B);
      end loop;
      New_Line;
   else
      Put_Line ("ERROR: Argon2id derivation failed!");
   end if;

end Test_Argon2_Debug_Blocks;
