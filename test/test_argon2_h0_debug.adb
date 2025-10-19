--  Debug H0 computation
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id.H0;

procedure Test_Argon2_H0_Debug is

   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Put_Hex;

   Password : constant Byte_Array := (
      Unsigned_8(Character'Pos('p')),
      Unsigned_8(Character'Pos('a')),
      Unsigned_8(Character'Pos('s')),
      Unsigned_8(Character'Pos('s')),
      Unsigned_8(Character'Pos('w')),
      Unsigned_8(Character'Pos('o')),
      Unsigned_8(Character'Pos('r')),
      Unsigned_8(Character'Pos('d'))
   );

   Salt : Byte_Array (1 .. 32);
   H0_Output : Byte_Array (1 .. 64);

begin
   --  Salt = "somesaltSOMESALTsomesaltSOMESALT"
   Salt := (
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  -- "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#,  -- "SOMESALT"
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  -- "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#   -- "SOMESALT"
   );

   Put_Line ("Computing H0 for Argon2id test vector 1");
   Put_Line ("Password: 'password' (8 bytes)");
   Put_Line ("Salt: 'somesaltSOMESALTsomesaltSOMESALT' (32 bytes)");
   Put_Line ("Parameters: m=16384, t=4, p=1, taglen=32");
   New_Line;

   SparkPass.Crypto.Argon2id.H0.Compute_H0 (
      Password    => Password,
      Salt        => Salt,
      Parallelism => 1,
      Tag_Length  => 32,
      Memory_KiB  => 16384,
      Iterations  => 4,
      H0_Out      => H0_Output
   );

   Put ("H0 output: ");
   for I in H0_Output'Range loop
      Put_Hex (H0_Output (I));
   end loop;
   New_Line;

end Test_Argon2_H0_Debug;
