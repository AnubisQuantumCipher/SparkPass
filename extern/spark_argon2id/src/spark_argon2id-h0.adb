pragma SPARK_Mode (On);

with Interfaces;
with Spark_Argon2id.Blake2b;
with Spark_Argon2id.Zeroize;

package body Spark_Argon2id.H0 with
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
     [1 => U8 (Value and 16#FF#),
      2 => U8 (Interfaces.Shift_Right (Value, 8) and 16#FF#),
      3 => U8 (Interfaces.Shift_Right (Value, 16) and 16#FF#),
      4 => U8 (Interfaces.Shift_Right (Value, 24) and 16#FF#)]
   with
      Global => null,
      Post   => LE32'Result'Length = 4;

   ------------------------------------------------------------
   --  Compute_H0
   ------------------------------------------------------------

   procedure Compute_H0 (
      Password        : Byte_Array;
      Salt            : Byte_Array;
      Key             : Byte_Array;
      Associated_Data : Byte_Array;
      Parallelism     : Positive;
      Tag_Length      : Positive;
      Memory_KiB      : Positive;
      Iterations      : Positive;
      H0_Out          : out Byte_Array
   ) is
      --  Argon2 constants (RFC 9106)
      Version : constant U32 := 16#13#;  -- Version 1.3 (decimal 19)
      Argon2id_Type : constant U32 := 2; -- Argon2id

      --  Allocate Input buffer to exact size:
      --    10 parameters * 4 bytes = 40 bytes + |P| + |S| + |K| + |X|
      Input_Length : constant Natural := 40 + Password'Length + Salt'Length + Key'Length + Associated_Data'Length;
      Input        : Byte_Array (1 .. Input_Length) := [others => 0];

      --  Current write position in Input buffer
      Offset : Natural := 1;

      --  Key and associated data lengths
      Key_Len : constant U32 := U32 (Key'Length);
      AD_Len  : constant U32 := U32 (Associated_Data'Length);
   begin
      --  Initialize output to zero (fail-closed)
      H0_Out := [others => 0];

      --  1. LE32(p)
      Input (Offset .. Offset + 3) := LE32 (U32 (Parallelism));
      Offset := Offset + 4;

      --  2. LE32(τ)
      Input (Offset .. Offset + 3) := LE32 (U32 (Tag_Length));
      Offset := Offset + 4;

      --  3. LE32(m)
      Input (Offset .. Offset + 3) := LE32 (U32 (Memory_KiB));
      Offset := Offset + 4;

      --  4. LE32(t)
      Input (Offset .. Offset + 3) := LE32 (U32 (Iterations));
      Offset := Offset + 4;

      --  5. LE32(v)
      Input (Offset .. Offset + 3) := LE32 (Version);
      Offset := Offset + 4;

      --  6. LE32(y)
      Input (Offset .. Offset + 3) := LE32 (Argon2id_Type);
      Offset := Offset + 4;

      --  7. LE32(|P|) || P
      Input (Offset .. Offset + 3) := LE32 (U32 (Password'Length));
      Offset := Offset + 4;
      Input (Offset .. Offset + Password'Length - 1) := Password;
      Offset := Offset + Password'Length;

      --  8. LE32(|S|) || S
      Input (Offset .. Offset + 3) := LE32 (U32 (Salt'Length));
      Offset := Offset + 4;
      Input (Offset .. Offset + Salt'Length - 1) := Salt;
      Offset := Offset + Salt'Length;

      --  9. LE32(|K|) || K (optional)
      Input (Offset .. Offset + 3) := LE32 (Key_Len);
      Offset := Offset + 4;
      if Key'Length > 0 then
         Input (Offset .. Offset + Key'Length - 1) := Key;
         Offset := Offset + Key'Length;
      end if;

      -- 10. LE32(|X|) || X (optional)
      Input (Offset .. Offset + 3) := LE32 (AD_Len);
      Offset := Offset + 4;
      if Associated_Data'Length > 0 then
         Input (Offset .. Offset + Associated_Data'Length - 1) := Associated_Data;
         Offset := Offset + Associated_Data'Length;
      end if;

      --  Compute Blake2b-512(H0_input)
      declare
         Final_Length : constant Positive := Positive (Offset - 1);
      begin
         Blake2b.Hash (
            Message => Input (1 .. Final_Length),
            Output  => H0_Out
         );
      end;

      --  Zeroize sensitive input
      Spark_Argon2id.Zeroize.Wipe (Input);

   exception
      when others =>
         H0_Out := [others => 0];
         Spark_Argon2id.Zeroize.Wipe (Input);
         raise;
   end Compute_H0;

end Spark_Argon2id.H0;
