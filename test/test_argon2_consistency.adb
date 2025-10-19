--  Simple Argon2id consistency test
--  Tests that calling Argon2id.Derive with the same inputs produces the same output
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id;

procedure Test_Argon2_Consistency is

   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Put_Hex;

   procedure Dump_Bytes (Label : String; Data : Key_Array) is
   begin
      Put (Label & ": ");
      for I in 1 .. 16 loop
         Put_Hex (Data (I));
         if I < 16 then
            Put (" ");
         end if;
      end loop;
      New_Line;
      Put ("          ");
      for I in 17 .. 32 loop
         Put_Hex (Data (I));
         if I < 32 then
            Put (" ");
         end if;
      end loop;
      New_Line;
   end Dump_Bytes;

   Password : constant Byte_Array := (
      Unsigned_8(Character'Pos('t')),
      Unsigned_8(Character'Pos('e')),
      Unsigned_8(Character'Pos('s')),
      Unsigned_8(Character'Pos('t')),
      Unsigned_8(Character'Pos('1')),
      Unsigned_8(Character'Pos('2')),
      Unsigned_8(Character'Pos('3'))
   );

   Params : SparkPass.Crypto.Argon2id.Parameters;
   Result1 : Key_Array;
   Result2 : Key_Array;
   Success : Boolean;
   Match : Boolean := True;

begin
   Put_Line ("========================================");
   Put_Line ("Argon2id Consistency Test");
   Put_Line ("========================================");
   New_Line;

   --  Set up parameters with fixed salt
   Params.Salt := (others => 16#42#);  -- Fixed salt for reproducibility
   Params.Memory_Cost := 8192;  -- 8 MiB (small for faster testing)
   Params.Iterations := 1;
   Params.Parallelism := 1;

   Put_Line ("Password: 'test123'");
   Put_Line ("Salt: all 0x42");
   Put_Line ("Memory: 8192 KiB");
   Put_Line ("Iterations: 1");
   Put_Line ("Parallelism: 1");
   New_Line;

   --  First derivation
   Put_Line ("First derivation...");
   SparkPass.Crypto.Argon2id.Derive (Password, Params, Result1, Success);

   if not Success then
      Put_Line ("ERROR: First derivation failed!");
      return;
   end if;

   Dump_Bytes ("Result 1", Result1);
   New_Line;

   --  Second derivation with same parameters
   Put_Line ("Second derivation (same parameters)...");
   SparkPass.Crypto.Argon2id.Derive (Password, Params, Result2, Success);

   if not Success then
      Put_Line ("ERROR: Second derivation failed!");
      return;
   end if;

   Dump_Bytes ("Result 2", Result2);
   New_Line;

   --  Compare results
   for I in Result1'Range loop
      if Result1 (I) /= Result2 (I) then
         Match := False;
         exit;
      end if;
   end loop;

   if Match then
      Put_Line ("SUCCESS: Both derivations match! Argon2id is consistent.");
   else
      Put_Line ("FAILURE: Derivations DO NOT match! Argon2id is broken!");
   end if;

   Put_Line ("========================================");

end Test_Argon2_Consistency;
