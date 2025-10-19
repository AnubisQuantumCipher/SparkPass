pragma SPARK_Mode (On);

with Interfaces;
with SparkPass.Crypto.Blake2b;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.Argon2id.H0 with
   SPARK_Mode => On
is
   --  Little-Endian 32-bit encoding (RFC 9106 Section 2)
   --
   --  Converts a 32-bit unsigned integer to 4 bytes in little-endian order.
   --  Used for encoding all parameters in H₀ construction.
   --
   --  @param Value  32-bit unsigned integer
   --  @return       4 bytes in little-endian order
   --
   --  Example: LE32(0x12345678) = [0x78, 0x56, 0x34, 0x12]
   --
   function LE32 (Value : U32) return Byte_Array is
     ((1 => U8 (Value and 16#FF#),
       2 => U8 (Interfaces.Shift_Right (Value, 8) and 16#FF#),
       3 => U8 (Interfaces.Shift_Right (Value, 16) and 16#FF#),
       4 => U8 (Interfaces.Shift_Right (Value, 24) and 16#FF#)))
   with
      Global => null,
      Post   => LE32'Result'Length = 4;

   ------------------------------------------------------------
   --  Compute_H0
   ------------------------------------------------------------

   procedure Compute_H0 (
      Password    : Byte_Array;
      Salt        : Byte_Array;
      Parallelism : Positive;
      Tag_Length  : Positive;
      Memory_KiB  : Positive;
      Iterations  : Positive;
      H0_Out      : out Byte_Array
   ) is
      --  Maximum input size:
      --    10 parameters * 4 bytes = 40 bytes
      --    Password = 128 bytes (max)
      --    Salt = 32 bytes
      --    Total = 200 bytes
      Input : Byte_Array (1 .. 200) := (others => 0);

      --  Current write position in Input buffer
      subtype Offset_Range is Positive range 1 .. 201;
      Offset : Offset_Range := 1;

      --  Argon2 constants (RFC 9106)
      Version : constant U32 := 16#13#;  -- Version 1.3 (decimal 19)
      Argon2id_Type : constant U32 := 2; -- Argon2id

      --  Empty key and associated data
      Key_Length : constant U32 := 0;
      AD_Length  : constant U32 := 0;
   begin
      --  Initialize output to zero (fail-closed)
      H0_Out := (others => 0);

      ------------------------------------------------------------
      --  Construct H₀ input per RFC 9106 Section 3.4
      ------------------------------------------------------------

      --  1. LE32(p) - Parallelism
      Input (Offset .. Offset + 3) := LE32 (U32 (Parallelism));
      Offset := Offset + 4;
      pragma Assert (Offset = 5);

      --  2. LE32(τ) - Tag length
      Input (Offset .. Offset + 3) := LE32 (U32 (Tag_Length));
      Offset := Offset + 4;
      pragma Assert (Offset = 9);

      --  3. LE32(m) - Memory size in KiB
      Input (Offset .. Offset + 3) := LE32 (U32 (Memory_KiB));
      Offset := Offset + 4;
      pragma Assert (Offset = 13);

      --  4. LE32(t) - Iterations
      Input (Offset .. Offset + 3) := LE32 (U32 (Iterations));
      Offset := Offset + 4;
      pragma Assert (Offset = 17);

      --  5. LE32(v) - Version
      Input (Offset .. Offset + 3) := LE32 (Version);
      Offset := Offset + 4;
      pragma Assert (Offset = 21);

      --  6. LE32(y) - Argon2 type
      Input (Offset .. Offset + 3) := LE32 (Argon2id_Type);
      Offset := Offset + 4;
      pragma Assert (Offset = 25);

      --  7. LE32(|P|) - Password length
      Input (Offset .. Offset + 3) := LE32 (U32 (Password'Length));
      Offset := Offset + 4;
      pragma Assert (Offset = 29);

      --  8. P - Password (variable length)
      declare
         Password_Start : constant Positive := Offset;
         Password_End   : constant Positive := Offset + Password'Length - 1;
      begin
         pragma Assert (Password_End >= Password_Start);
         pragma Assert (Password_End <= 200);

         Input (Password_Start .. Password_End) := Password;
         Offset := Password_End + 1;
         pragma Assert (Offset = 29 + Password'Length);
      end;

      --  9. LE32(|S|) - Salt length
      Input (Offset .. Offset + 3) := LE32 (32);
      Offset := Offset + 4;
      pragma Assert (Offset = 33 + Password'Length);

      --  10. S - Salt (32 bytes)
      declare
         Salt_Start : constant Positive := Offset;
         Salt_End   : constant Positive := Offset + 31;
      begin
         pragma Assert (Salt_End = Salt_Start + 31);
         pragma Assert (Salt_End <= 200);

         Input (Salt_Start .. Salt_End) := Salt;
         Offset := Salt_End + 1;
         pragma Assert (Offset = 65 + Password'Length);
      end;

      --  11. LE32(|K|) - Key length (0 for us)
      Input (Offset .. Offset + 3) := LE32 (Key_Length);
      Offset := Offset + 4;
      pragma Assert (Offset = 69 + Password'Length);

      --  12. K - Key (empty, no data to append)

      --  13. LE32(|X|) - Associated data length (0 for us)
      Input (Offset .. Offset + 3) := LE32 (AD_Length);
      Offset := Offset + 4;
      pragma Assert (Offset = 73 + Password'Length);

      --  14. X - Associated data (empty, no data to append)

      --  Final input length
      declare
         Input_Length : constant Positive := Offset - 1;
      begin
         pragma Assert (Input_Length = 72 + Password'Length);
         pragma Assert (Input_Length >= 73);  -- Minimum: 72 + 1 (min password)
         pragma Assert (Input_Length <= 200); -- Maximum: 72 + 128 (max password)

         ------------------------------------------------------------
         --  Compute Blake2b-512 hash
         ------------------------------------------------------------

         Blake2b.Hash (
            Message => Input (1 .. Input_Length),
            Output  => H0_Out
         );
      end;

      ------------------------------------------------------------
      --  Zeroize sensitive data
      ------------------------------------------------------------

      --  Clear input buffer (contains password)
      SparkPass.Crypto.Zeroize.Wipe (Input);

   exception
      when others =>
         --  Fail-closed: zeroize on any error
         H0_Out := (others => 0);
         SparkPass.Crypto.Zeroize.Wipe (Input);
         raise;
   end Compute_H0;

end SparkPass.Crypto.Argon2id.H0;
