--  ========================================================================
--  ML-KEM Diagnostic Test: Detailed Roundtrip Analysis
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.KeyGen;
with SparkPass.Crypto.MLKEM.Encaps;
with SparkPass.Crypto.MLKEM.Decaps;
with SparkPass.Crypto.Random;

procedure Test_MLKEM_Diagnostic2 is

   procedure Print_Hex_Bytes (Label : String; Data : Byte_Array; Max : Natural := 32) is
   begin
      Put (Label & ": ");
      for I in Data'First .. Natural'Min(Data'First + Max - 1, Data'Last) loop
         Put (U8'Image(Data(I)));
         if I < Natural'Min(Data'First + Max - 1, Data'Last) then
            Put (" ");
         end if;
      end loop;
      if Data'Length > Max then
         Put (" ... (" & Natural'Image(Data'Length) & " bytes total)");
      end if;
      New_Line;
   end Print_Hex_Bytes;

   --  Keys
   Seed : Seed_Array;
   PK   : Public_Key_Array;
   SK   : Secret_Key_Array;

   --  Encapsulation
   CT1  : Ciphertext_Array;
   SS1  : Shared_Secret_Array;
   Msg1 : Seed_Array;
   U1   : Polynomial_Vector;
   V1   : Polynomial;

   --  Decapsulation
   SS2     : Shared_Secret_Array;
   Msg2    : Seed_Array;
   Valid   : Boolean;

   Match : Boolean;

begin
   Put_Line("========================================");
   Put_Line("ML-KEM-1024 Diagnostic Test");
   Put_Line("========================================");
   New_Line;

   --  Step 1: Generate deterministic key pair
   Put_Line("[1] Generating key pair with fixed seed...");
   Seed := (others => 42);  -- Fixed seed for reproducibility

   SparkPass.Crypto.MLKEM.KeyGen.KeyGen(Seed, PK, SK);

   Print_Hex_Bytes("   Public key", PK, 32);
   Print_Hex_Bytes("   Secret key", SK, 32);
   New_Line;

   --  Step 2: Encapsulate with fixed message
   Put_Line("[2] Encapsulating with fixed message...");
   Msg1 := (others => 123);  -- Fixed message for reproducibility

   SparkPass.Crypto.MLKEM.Encaps.Encapsulate_Expanded(
      PK, Msg1, CT1, SS1, U1, V1
   );

   Print_Hex_Bytes("   Message m", Msg1, 32);
   Print_Hex_Bytes("   Ciphertext c", CT1, 32);
   Print_Hex_Bytes("   Shared secret SS1", SS1, 32);
   Put_Line("   U vector[0][0] = " & Coefficient'Image(U1(0)(0)));
   Put_Line("   V poly[0] = " & Coefficient'Image(V1(0)));
   New_Line;

   --  Step 3: Decapsulate
   Put_Line("[3] Decapsulating ciphertext...");

   SparkPass.Crypto.MLKEM.Decaps.Decapsulate_Expanded(
      SK, CT1, SS2, Msg2, Valid
   );

   Print_Hex_Bytes("   Recovered message m'", Msg2, 32);
   Print_Hex_Bytes("   Shared secret SS2", SS2, 32);
   Put_Line("   Ciphertext valid: " & Boolean'Image(Valid));
   New_Line;

   --  Step 4: Compare
   Put_Line("[4] Verification:");

   --  Check if messages match
   Match := True;
   for I in Msg1'Range loop
      if Msg1(I) /= Msg2(I) then
         Match := False;
         exit;
      end if;
   end loop;
   Put_Line("   Messages match (m = m'): " & Boolean'Image(Match));

   if not Match then
      Put_Line("   DIVERGENCE: Message recovery failed!");
      Print_Hex_Bytes("     Original m", Msg1, 16);
      Print_Hex_Bytes("     Recovered m'", Msg2, 16);
   end if;

   --  Check if shared secrets match
   Match := True;
   for I in SS1'Range loop
      if SS1(I) /= SS2(I) then
         Match := False;
         exit;
      end if;
   end loop;
   Put_Line("   Shared secrets match (SS1 = SS2): " & Boolean'Image(Match));

   if not Match then
      Put_Line("   DIVERGENCE: Shared secret mismatch!");
      Print_Hex_Bytes("     Encaps SS1", SS1);
      Print_Hex_Bytes("     Decaps SS2", SS2);
   end if;

   New_Line;

   if Match then
      Put_Line("========================================");
      Put_Line("TEST RESULT: SUCCESS");
      Put_Line("========================================");
   else
      Put_Line("========================================");
      Put_Line("TEST RESULT: FAILURE");
      Put_Line("========================================");
   end if;

end Test_MLKEM_Diagnostic2;
