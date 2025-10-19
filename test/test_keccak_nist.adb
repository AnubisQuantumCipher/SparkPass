--  NIST SHA3-256 Test Vector Validation
--  Tests the Keccak State(Y,X) fix against FIPS 202 test vectors
with Ada.Text_IO; use Ada.Text_IO;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Keccak; use SparkPass.Crypto.Keccak;
with Interfaces; use Interfaces;

procedure Test_Keccak_NIST is

   procedure Print_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Print_Hex;

   procedure Test_Empty_String is
      Empty : Byte_Array (1 .. 0);
      Output : SHA3_256_Digest;
      --  NIST FIPS 202 test vector for SHA3-256("")
      Expected : constant SHA3_256_Digest :=
        (16#a7#, 16#ff#, 16#c6#, 16#f8#, 16#bf#, 16#1e#, 16#d7#, 16#66#,
         16#51#, 16#c1#, 16#47#, 16#56#, 16#a0#, 16#61#, 16#d6#, 16#62#,
         16#f5#, 16#80#, 16#ff#, 16#4d#, 16#e4#, 16#3b#, 16#49#, 16#fa#,
         16#82#, 16#d8#, 16#0a#, 16#4b#, 16#80#, 16#f8#, 16#43#, 16#4a#);
      Match : Boolean := True;
   begin
      Put_Line ("======================================");
      Put_Line ("SHA3-256 of empty string");
      Put_Line ("======================================");

      SHA3_256_Hash (Empty, Output);

      Put ("Output:   ");
      for I in Output'Range loop
         Print_Hex (Output (I));
         if I mod 16 = 0 and I /= Output'Last then
            New_Line;
            Put ("          ");
         elsif I /= Output'Last then
            Put (" ");
         end if;
      end loop;
      New_Line;

      Put ("Expected: ");
      for I in Expected'Range loop
         Print_Hex (Expected (I));
         if I mod 16 = 0 and I /= Expected'Last then
            New_Line;
            Put ("          ");
         elsif I /= Expected'Last then
            Put (" ");
         end if;
      end loop;
      New_Line; New_Line;

      for I in Output'Range loop
         if Output (I) /= Expected (I) then
            Match := False;
            exit;
         end if;
      end loop;

      if Match then
         Put_Line ("SUCCESS: SHA3-256("""") matches NIST FIPS 202!");
      else
         Put_Line ("FAILURE: SHA3-256("""") does NOT match!");
      end if;
   end Test_Empty_String;

   procedure Test_ABC is
      Input : constant Byte_Array := (16#61#, 16#62#, 16#63#);  -- "abc"
      Output : SHA3_256_Digest;
      --  NIST FIPS 202 test vector for SHA3-256("abc") - CORRECTED
      Expected : constant SHA3_256_Digest :=
        (16#3a#, 16#98#, 16#5d#, 16#a7#, 16#4f#, 16#e2#, 16#25#, 16#b2#,
         16#04#, 16#5c#, 16#17#, 16#2d#, 16#6b#, 16#d3#, 16#90#, 16#bd#,
         16#85#, 16#5f#, 16#08#, 16#6e#, 16#3e#, 16#9d#, 16#52#, 16#5b#,
         16#46#, 16#bf#, 16#e2#, 16#45#, 16#11#, 16#43#, 16#15#, 16#32#);
      Match : Boolean := True;
   begin
      New_Line;
      Put_Line ("======================================");
      Put_Line ("SHA3-256 of 'abc'");
      Put_Line ("======================================");

      SHA3_256_Hash (Input, Output);

      Put ("Output:   ");
      for I in Output'Range loop
         Print_Hex (Output (I));
         if I mod 16 = 0 and I /= Output'Last then
            New_Line;
            Put ("          ");
         elsif I /= Output'Last then
            Put (" ");
         end if;
      end loop;
      New_Line;

      Put ("Expected: ");
      for I in Expected'Range loop
         Print_Hex (Expected (I));
         if I mod 16 = 0 and I /= Expected'Last then
            New_Line;
            Put ("          ");
         elsif I /= Expected'Last then
            Put (" ");
         end if;
      end loop;
      New_Line; New_Line;

      for I in Output'Range loop
         if Output (I) /= Expected (I) then
            Match := False;
            exit;
         end if;
      end loop;

      if Match then
         Put_Line ("SUCCESS: SHA3-256('abc') matches NIST FIPS 202!");
      else
         Put_Line ("FAILURE: SHA3-256('abc') does NOT match!");
      end if;
   end Test_ABC;

begin
   Test_Empty_String;
   Test_ABC;
   New_Line;
   Put_Line ("======================================");
   Put_Line ("KECCAK NIST VALIDATION COMPLETE");
   Put_Line ("======================================");
end Test_Keccak_NIST;
