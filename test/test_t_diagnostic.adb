--  Test to diagnose t vector computation

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.KeyGen;
with SparkPass.Crypto.MLKEM.Encoding;

procedure Test_T_Diagnostic is

   function Hex_To_Byte (C : Character) return U8 is
   begin
      case C is
         when '0' => return 0;
         when '1' => return 1;
         when '2' => return 2;
         when '3' => return 3;
         when '4' => return 4;
         when '5' => return 5;
         when '6' => return 6;
         when '7' => return 7;
         when '8' => return 8;
         when '9' => return 9;
         when 'a' | 'A' => return 10;
         when 'b' | 'B' => return 11;
         when 'c' | 'C' => return 12;
         when 'd' | 'D' => return 13;
         when 'e' | 'E' => return 14;
         when 'f' | 'F' => return 15;
         when others => return 0;
      end case;
   end Hex_To_Byte;

   procedure Hex_To_Bytes (Hex : String; Output : out Byte_Array) is
      J : Natural := Output'First;
   begin
      for I in Hex'Range loop
         if I mod 2 = 1 and I + 1 <= Hex'Last then
            Output(J) := Hex_To_Byte(Hex(I)) * 16 + Hex_To_Byte(Hex(I + 1));
            J := J + 1;
         end if;
      end loop;
   end Hex_To_Bytes;

   D_Hex : constant String := "6dbbc4375136df3b07f7c70e639e223e177e7fd53b161b3f4d57791794f12624";
   PK_Hex : constant String := "a8122f376b3f5d355263eba522c43995044beca78b9ba9924b87035490ae78a9";

   D_Seed : Seed_Array;
   PK_Expected_First : Byte_Array(1 .. 32);
   PK_Actual : Public_Key_Array;
   SK_Actual : Secret_Key_Array;

begin
   Put_Line("Diagnosing t vector computation");
   Put_Line("=================================");
   New_Line;

   Hex_To_Bytes(D_Hex, D_Seed);
   Hex_To_Bytes(PK_Hex, PK_Expected_First);

   --  Run KeyGen
   SparkPass.Crypto.MLKEM.KeyGen.KeyGen(D_Seed, PK_Actual, SK_Actual);

   --  Show first 32 bytes
   Put_Line("First 32 bytes of public key:");
   Put("  Expected: ");
   for I in 1 .. 32 loop
      Put(U8'Image(PK_Expected_First(I)));
      if I < 32 then Put(" "); end if;
   end loop;
   New_Line;

   Put("  Got:      ");
   for I in 1 .. 32 loop
      Put(U8'Image(PK_Actual(I)));
      if I < 32 then Put(" "); end if;
   end loop;
   New_Line;
   New_Line;

   --  Now let's decode the first 3 bytes and see what polynomial coefficients they represent
   declare
      use SparkPass.Crypto.MLKEM.Encoding;
      T_Decoded : Polynomial_Vector;
      T_Bytes : Byte_Array(1 .. 1536);
   begin
      T_Bytes := PK_Actual(1 .. 1536);
      Decode_Vector_12(T_Bytes, T_Decoded);

      Put_Line("Decoded t vector (first polynomial, first 8 coefficients):");
      for I in 0 .. 7 loop
         Put("  t[0][" & Natural'Image(I) & "] = " & Coefficient'Image(T_Decoded(0)(I)));
         New_Line;
      end loop;
   end;

   New_Line;
   Put_Line("Expected first 3 bytes: a8 12 2f");
   Put_Line("Actual first 3 bytes:   " & U8'Image(PK_Actual(1)) & " " &
            U8'Image(PK_Actual(2)) & " " & U8'Image(PK_Actual(3)));
   New_Line;

   --  Manually decode what those bytes should represent:
   --  ByteEncode_12 packs 2 coefficients into 3 bytes:
   --  Byte 0: a[0:7]
   --  Byte 1: a[8:11] || b[0:3]
   --  Byte 2: b[4:11]

   --  Expected: a8 12 2f means:
   --  a8 = 168, 12 = 18, 2f = 47
   --  coeff_0 = 168 | ((18/16) * 256) = 168 | (1 * 256) = 168 | 256 = 424
   --  coeff_1 = (18 & 0x0F) | (47 * 16) = 2 | 752 = 754

   declare
      C0_expected, C1_expected : Integer;
      C0_actual, C1_actual : Integer;
   begin
      --  From expected bytes
      C0_expected := 168 + ((18 / 16) * 256);
      C1_expected := (18 mod 16) + (47 * 16);

      --  From actual bytes
      C0_actual := Integer(PK_Actual(1)) + ((Integer(PK_Actual(2)) / 16) * 256);
      C1_actual := (Integer(PK_Actual(2)) mod 16) + (Integer(PK_Actual(3)) * 16);

      Put_Line("Expected coeff_0 = " & Integer'Image(C0_expected));
      Put_Line("Actual coeff_0   = " & Integer'Image(C0_actual));
      Put_Line("Expected coeff_1 = " & Integer'Image(C1_expected));
      Put_Line("Actual coeff_1   = " & Integer'Image(C1_actual));
   end;

end Test_T_Diagnostic;
