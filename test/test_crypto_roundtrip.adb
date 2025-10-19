--  Minimal cryptographic round-trip test
--  Tests: Argon2id → ChaCha20Poly1305 encrypt → decrypt
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id;
with SparkPass.Crypto.ChaCha20Poly1305;
with SparkPass.Crypto.Random;

procedure Test_Crypto_Roundtrip is

   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (Shift_Right (B, 4)) + 1));
      Put (Hex (Natural (B and 16#0F#) + 1));
   end Put_Hex;

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
   Wrap_Key : Key_Array := (others => 0);
   Success : Boolean;

   Plaintext : constant Key_Array := (
      1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
      17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32
   );
   Nonce : Nonce_Array := (others => 16#42#);
   AAD : Salt_Array := (others => 16#99#);
   Ciphertext : Key_Array := (others => 0);
   Tag : Tag_Array := (others => 0);
   Decrypted : Key_Array := (others => 0);
   Match : Boolean := True;

begin
   Put_Line ("========================================");
   Put_Line ("Crypto Round-Trip Test");
   Put_Line ("========================================");
   New_Line;

   --  Step 1: Set up fixed Argon2 parameters
   Params.Salt := (others => 16#55#);  -- Fixed salt
   Params.Memory_Cost := 8192;  -- 8 MiB (small for speed)
   Params.Iterations := 1;
   Params.Parallelism := 1;

   Put_Line ("Password: 'test123'");
   Put_Line ("Salt: all 0x55");
   Put_Line ("Memory: 8192 KiB");
   New_Line;

   --  Step 2: Derive wrap_key
   Put_Line ("Deriving wrap_key with Argon2id...");
   SparkPass.Crypto.Argon2id.Derive (Password, Params, Wrap_Key, Success);

   if not Success then
      Put_Line ("FAIL: Argon2id derivation failed!");
      return;
   end if;

   Put ("Wrap_Key: ");
   for I in 1 .. 8 loop
      Put_Hex (Wrap_Key (I));
      Put (" ");
   end loop;
   Put_Line ("...");
   New_Line;

   --  Step 3: Encrypt with ChaCha20Poly1305
   Put_Line ("Encrypting plaintext...");
   SparkPass.Crypto.ChaCha20Poly1305.Seal
     (Key        => Wrap_Key,
      Nonce      => Nonce,
      Plaintext  => Plaintext,
      AAD        => AAD,
      Ciphertext => Ciphertext,
      Tag        => Tag);

   Put ("Ciphertext: ");
   for I in 1 .. 8 loop
      Put_Hex (Ciphertext (I));
      Put (" ");
   end loop;
   Put_Line ("...");
   New_Line;

   --  Step 4: Decrypt with same key
   Put_Line ("Decrypting with same wrap_key...");
   SparkPass.Crypto.ChaCha20Poly1305.Open
     (Key        => Wrap_Key,
      Nonce      => Nonce,
      Ciphertext => Ciphertext,
      AAD        => AAD,
      Tag        => Tag,
      Plaintext  => Decrypted,
      Success    => Success);

   if not Success then
      Put_Line ("FAIL: ChaCha20Poly1305 decryption failed!");
      return;
   end if;

   Put ("Decrypted:  ");
   for I in 1 .. 8 loop
      Put_Hex (Decrypted (I));
      Put (" ");
   end loop;
   Put_Line ("...");
   New_Line;

   --  Step 5: Compare plaintext and decrypted
   for I in Plaintext'Range loop
      if Plaintext (I) /= Decrypted (I) then
         Match := False;
         exit;
      end if;
   end loop;

   if Match then
      Put_Line ("SUCCESS: Plaintext matches decrypted data!");
      Put_Line ("Crypto round-trip is working correctly.");
   else
      Put_Line ("FAIL: Plaintext does NOT match decrypted data!");
   end if;

   Put_Line ("========================================");

end Test_Crypto_Roundtrip;
