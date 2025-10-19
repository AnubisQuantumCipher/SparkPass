--  ========================================================================
--  KeyGen Detailed Diagnostic
--  ========================================================================
--
--  **Purpose**: Step-by-step validation of KeyGen internals
--              Compare intermediate values against manual calculations
--
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.Hash;

procedure Test_KeyGen_Detailed is

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

   procedure Print_Hex (Label : String; Data : Byte_Array; Count : Natural := 32) is
   begin
      Put(Label & ": ");
      for I in Data'First .. Natural'Min(Data'First + Count - 1, Data'Last) loop
         Put(U8'Image(Data(I)));
         if I < Natural'Min(Data'First + Count - 1, Data'Last) then
            Put(" ");
         end if;
      end loop;
      New_Line;
   end Print_Hex;

   --  NIST Test Vector 0
   D_Hex : constant String := "6dbbc4375136df3b07f7c70e639e223e177e7fd53b161b3f4d57791794f12624";

   --  Expected ρ and σ from G(d || 4)
   --  We need to compute this and see if it matches what's expected

   D_Seed : Seed_Array;
   G_Output : SparkPass.Crypto.MLKEM.Hash.SHA3_512_Digest;
   Rho_Actual : Seed_Array;
   Sigma_Actual : Seed_Array;

begin
   Put_Line("========================================");
   Put_Line("KeyGen Step-by-Step Diagnostic");
   Put_Line("========================================");
   New_Line;

   --  Parse input
   Hex_To_Bytes(D_Hex, D_Seed);

   Put_Line("[Step 1] Input d (KeyGen seed):");
   Print_Hex("  d", D_Seed);
   New_Line;

   Put_Line("[Step 2] Compute G(d || k) where k=4:");
   Put_Line("  G = SHA3-512");
   Put_Line("  Input: d || 0x04");

   --  Call G_Expand_Seed
   SparkPass.Crypto.MLKEM.Hash.G_Expand_Seed(D_Seed, 4, G_Output);

   Rho_Actual := G_Output(1 .. 32);
   Sigma_Actual := G_Output(33 .. 64);

   Put_Line("");
   Print_Hex("  ρ (first 32 bytes of G output)", Rho_Actual);
   Print_Hex("  σ (last 32 bytes of G output)", Sigma_Actual);

   New_Line;
   Put_Line("========================================");
   Put_Line("This shows what seeds are generated.");
   Put_Line("Next: Check if matrix A generation uses");
   Put_Line("correct XOF(ρ || i || j) byte order.");
   Put_Line("========================================");

end Test_KeyGen_Detailed;
