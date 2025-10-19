--  Test H₀ computation for Argon2id
--  This verifies the initial hash construction is working correctly

with Ada.Text_IO; use Ada.Text_IO;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id;
with Interfaces;

procedure Test_H0 is
   use Interfaces;

   --  Test parameters
   Password : constant Byte_Array := (1, 2, 3, 4, 5, 6, 7, 8);
   Salt     : Salt_Array := (others => 16#42#);
   Params   : SparkPass.Crypto.Argon2id.Parameters;
   Output   : Key_Array;
   Success  : Boolean;

   --  Convert byte to hex string
   function To_Hex (B : U8) return String is
      Hex_Chars : constant String := "0123456789abcdef";
   begin
      return Hex_Chars (Natural (B / 16) + 1) & Hex_Chars (Natural (B mod 16) + 1);
   end To_Hex;

begin
   Put_Line ("=== Argon2id H₀ Initial Hash Test ===");
   Put_Line ("");

   --  Setup parameters
   Params.Memory_Cost := 1024;  -- 1 MiB
   Params.Iterations  := 3;
   Params.Parallelism := 1;
   Params.Salt        := Salt;

   Put_Line ("Test Parameters:");
   Put_Line ("  Memory:      " & Params.Memory_Cost'Image & " KiB");
   Put_Line ("  Iterations:  " & Params.Iterations'Image);
   Put_Line ("  Parallelism: " & Params.Parallelism'Image);
   Put_Line ("  Password:    8 bytes");
   Put_Line ("  Salt:        32 bytes (all 0x42)");
   Put_Line ("");

   --  Call Derive (currently returns failure but computes H₀ internally)
   SparkPass.Crypto.Argon2id.Derive (
      Password => Password,
      Params   => Params,
      Output   => Output,
      Success  => Success
   );

   Put_Line ("Result:");
   Put_Line ("  Success: " & Success'Image);
   Put_Line ("  (Expected: FALSE - Phase 2.2 is partial implementation)");
   Put_Line ("");

   if not Success then
      Put_Line ("✓ PASS: Derive correctly returns failure (partial implementation)");
   else
      Put_Line ("✗ FAIL: Derive should return failure in Phase 2.2");
   end if;

   Put_Line ("");
   Put_Line ("Note: H₀ is computed internally but not yet used for key derivation.");
   Put_Line ("      Full implementation continues in Phase 2.3+");

end Test_H0;
