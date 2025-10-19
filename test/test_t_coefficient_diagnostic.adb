with Ada.Text_IO; use Ada.Text_IO;
with SparkPass.Crypto.MLKEM.KeyGen;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.Encoding; use SparkPass.Crypto.MLKEM.Encoding;
with Interfaces; use Interfaces;

procedure Test_T_Coefficient_Diagnostic is

   function Hex_To_Byte (C1, C2 : Character) return U8 is
      function Hex_Digit (C : Character) return U8 is
      begin
         case C is
            when '0' .. '9' => return U8 (Character'Pos (C) - Character'Pos ('0'));
            when 'a' .. 'f' => return U8 (Character'Pos (C) - Character'Pos ('a') + 10);
            when 'A' .. 'F' => return U8 (Character'Pos (C) - Character'Pos ('A') + 10);
            when others => return 0;
         end case;
      end Hex_Digit;
   begin
      return Hex_Digit (C1) * 16 + Hex_Digit (C2);
   end Hex_To_Byte;

   procedure Hex_To_Bytes (Hex : String; Output : out Byte_Array) is
      Hex_Idx : Positive := Hex'First;
   begin
      for I in Output'Range loop
         Output (I) := Hex_To_Byte (Hex (Hex_Idx), Hex (Hex_Idx + 1));
         Hex_Idx := Hex_Idx + 2;
      end loop;
   end Hex_To_Bytes;

   -- NIST test vector count=0
   D_Hex : constant String :=
      "6dbbc4375136df3b07f7c70e639e223e177e7fd53b161b3f4d57791794f12624";

   -- Expected public key (first 48 bytes to decode t[0])
   PK_Expected_Hex : constant String :=
      "a8122f376b3f5d35" &  -- Bytes 0-7
      "a812a8ffd5ebb5e2" &  -- Bytes 8-15
      "63eba522c43995e8" &  -- Bytes 16-23
      "04968f4b8c3e9ad3" &  -- Bytes 24-31
      "0df45d77b08ebe3e" &  -- Bytes 32-39
      "ec81e914ad25a7ff";   -- Bytes 40-47

   D_Bytes : Seed_Bytes;
   PK_Actual : Public_Key;
   SK : Secret_Key;
   PK_Components : Public_Key_Components;
   SK_Components : Secret_Key_Components;

   -- Decode first 48 bytes to get t[0]
   T_Expected : Polynomial;
   T_Actual : Polynomial;

   PK_Expected_Bytes : Byte_Array (1 .. 48);

begin
   Put_Line ("========================================");
   Put_Line ("t[0] Coefficient Diagnostic");
   Put_Line ("========================================");
   New_Line;

   -- Convert seed
   Hex_To_Bytes (D_Hex, D_Bytes);

   -- Convert expected PK bytes
   Hex_To_Bytes (PK_Expected_Hex, PK_Expected_Bytes);

   -- Decode expected t[0] from first 384 bytes (but we only have 48 for now)
   -- t[0] uses bytes 0..383 (384 bytes = 256 coefficients at 12 bits each)
   -- For now, decode first 32 coefficients from first 48 bytes
   declare
      Coeff_Idx : Natural := 0;
   begin
      for Byte_Idx in 1 .. 48 loop
         if (Byte_Idx - 1) mod 3 = 0 and Coeff_Idx < 256 then
            -- Start of 3-byte group, decode 2 coefficients
            if Byte_Idx + 2 <= 48 then
               declare
                  Byte_0 : constant U8 := PK_Expected_Bytes (Byte_Idx);
                  Byte_1 : constant U8 := PK_Expected_Bytes (Byte_Idx + 1);
                  Byte_2 : constant U8 := PK_Expected_Bytes (Byte_Idx + 2);

                  Coeff_0 : Coefficient;
                  Coeff_1 : Coefficient;
               begin
                  -- Decode using ByteDecode_12 logic
                  Coeff_0 := Coefficient (Unsigned_16 (Byte_0) or
                                         ((Unsigned_16 (Byte_1) / 16) * 256));
                  Coeff_1 := Coefficient ((Unsigned_16 (Byte_1 and 16#0F#)) or
                                         (Unsigned_16 (Byte_2) * 16));

                  T_Expected (Coeff_Idx) := Coeff_0;
                  T_Expected (Coeff_Idx + 1) := Coeff_1;
                  Coeff_Idx := Coeff_Idx + 2;
               end;
            end if;
         end if;
      end loop;
   end;

   -- Run KeyGen
   SparkPass.Crypto.MLKEM.KeyGen.KeyGen_Expanded (
      Random_Seed => D_Bytes,
      PK => PK_Actual,
      SK => SK,
      Public_Components => PK_Components,
      Secret_Components => SK_Components
   );

   -- Get actual t[0] - decode from public key
   -- The first 384 bytes of PK are the encoded t[0]
   declare
      T0_Bytes : Byte_Array (1 .. 384);
   begin
      for I in 1 .. 384 loop
         T0_Bytes (I) := PK_Actual (I);
      end loop;
      ByteDecode_12 (T0_Bytes, T_Actual);
   end;

   -- Compare first 32 coefficients
   Put_Line ("Comparing first 32 coefficients of t[0]:");
   Put_Line ("========================================");
   Put_Line ("Idx    Expected    Actual    Diff    Match");
   Put_Line ("========================================");

   declare
      Matches : Natural := 0;
      Mismatches : Natural := 0;
   begin
      for I in 0 .. 31 loop
         declare
            Exp : constant Coefficient := T_Expected (I);
            Act : constant Coefficient := T_Actual (I);
            Diff : constant Integer := Integer (Act) - Integer (Exp);
            Match_Str : constant String := (if Exp = Act then "✓" else "✗");
         begin
            Put_Line (
               I'Image & "     " &
               Exp'Image & "     " &
               Act'Image & "     " &
               Diff'Image & "     " &
               Match_Str
            );

            if Exp = Act then
               Matches := Matches + 1;
            else
               Mismatches := Mismatches + 1;
            end if;
         end;
      end loop;

      New_Line;
      Put_Line ("Summary: " & Matches'Image & " matches, " &
                Mismatches'Image & " mismatches");
   end;

   New_Line;
   Put_Line ("Byte-level comparison (first 16 bytes):");
   Put_Line ("========================================");
   declare
      Exp_Byte, Act_Byte : U8;
   begin
      for I in 1 .. 16 loop
         Exp_Byte := PK_Expected_Bytes (I);
         Act_Byte := PK_Actual (I);
         Put_Line (
            "Byte" & I'Image & ":  Expected " &
            Exp_Byte'Image & "  Got " &
            Act_Byte'Image &
            (if Exp_Byte = Act_Byte then "  ✓" else "  ✗")
         );
      end loop;
   end;

end Test_T_Coefficient_Diagnostic;
