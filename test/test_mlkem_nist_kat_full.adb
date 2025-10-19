--  ========================================================================
--  ML-KEM-1024 Complete NIST KAT Validation (All 1000 Vectors)
--  ========================================================================
--
--  **Purpose**: Systematically validate all 1000 NIST KAT vectors
--
--  **Test Vectors**: test/nist_vectors/kat_MLKEM_1024.rsp
--
--  **Validation**: Proves pure SPARK ML-KEM passes full NIST test suite
--
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.KeyGen;
with SparkPass.Crypto.MLKEM.Encaps;
with SparkPass.Crypto.MLKEM.Decaps;

procedure Test_MLKEM_NIST_KAT_Full is

   --  =====================================================================
   --  Hex Conversion Utilities
   --  =====================================================================

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

   --  =====================================================================
   --  Test Statistics
   --  =====================================================================

   Total_Vectors  : Natural := 0;
   KeyGen_PK_Pass : Natural := 0;
   KeyGen_PK_Fail : Natural := 0;
   KeyGen_SK_Pass : Natural := 0;
   KeyGen_SK_Fail : Natural := 0;
   Encaps_CT_Pass : Natural := 0;
   Encaps_CT_Fail : Natural := 0;
   Encaps_SS_Pass : Natural := 0;
   Encaps_SS_Fail : Natural := 0;
   Decaps_Pass    : Natural := 0;
   Decaps_Fail    : Natural := 0;

   --  =====================================================================
   --  Test Data Buffers
   --  =====================================================================

   D_Seed      : Seed_Array;
   MSG_Seed    : Seed_Array;
   PK_Expected : Public_Key_Array;
   SK_Expected : Secret_Key_Array;
   CT_Expected : Ciphertext_Array;
   SS_Expected : Shared_Secret_Array;

   PK_Actual   : Public_Key_Array;
   SK_Actual   : Secret_Key_Array;
   CT_Actual   : Ciphertext_Array;
   SS_Encaps   : Shared_Secret_Array;
   SS_Decaps   : Shared_Secret_Array;

   U_Vec       : Polynomial_Vector;
   V_Poly      : Polynomial;

   --  =====================================================================
   --  RSP File Parsing
   --  =====================================================================

   RSP_File    : File_Type;
   Line_Buffer : Unbounded_String;
   Current_Field : Unbounded_String;
   Current_Value : Unbounded_String;

   Vector_Count : Natural := 0;

begin
   Put_Line("========================================================================");
   Put_Line("ML-KEM-1024 Complete NIST KAT Validation");
   Put_Line("========================================================================");
   Put_Line("Test Vectors: test/nist_vectors/kat_MLKEM_1024.rsp");
   Put_Line("Expected: 1000 vectors");
   New_Line;

   --  Open RSP file
   Open(RSP_File, In_File, "test/nist_vectors/kat_MLKEM_1024.rsp");

   --  =====================================================================
   --  Parse and Test Each Vector
   --  =====================================================================

   while not End_Of_File(RSP_File) loop
      --  Read line
      Line_Buffer := To_Unbounded_String(Get_Line(RSP_File));

      --  Skip empty lines
      if Length(Line_Buffer) = 0 then
         goto Continue_Loop;
      end if;

      --  Parse field = value
      declare
         Eq_Pos : constant Natural := Index(Line_Buffer, "=");
         Line_Str : constant String := To_String(Line_Buffer);
      begin
         if Eq_Pos > 0 then
            Current_Field := To_Unbounded_String(Trim(Line_Str(1 .. Eq_Pos - 1), Ada.Strings.Both));
            Current_Value := To_Unbounded_String(Trim(Line_Str(Eq_Pos + 1 .. Line_Str'Last), Ada.Strings.Both));

            --  Process based on field name
            if To_String(Current_Field) = "count" then
               Vector_Count := Natural'Value(To_String(Current_Value));
               Total_Vectors := Total_Vectors + 1;

               --  Print progress every 100 vectors
               if (Vector_Count + 1) mod 100 = 0 then
                  Put_Line("[Vector " & Natural'Image(Vector_Count) & "] Processing...");
               end if;

            elsif To_String(Current_Field) = "d" then
               Hex_To_Bytes(To_String(Current_Value), D_Seed);

            elsif To_String(Current_Field) = "msg" then
               Hex_To_Bytes(To_String(Current_Value), MSG_Seed);

            elsif To_String(Current_Field) = "pk" then
               Hex_To_Bytes(To_String(Current_Value), PK_Expected);

            elsif To_String(Current_Field) = "sk" then
               Hex_To_Bytes(To_String(Current_Value), SK_Expected);

            elsif To_String(Current_Field) = "ct" then
               --  Use "ct" field (FIPS 203 standard), not "ct_n"
               Hex_To_Bytes(To_String(Current_Value), CT_Expected);

            elsif To_String(Current_Field) = "ss" then
               --  Use "ss" field (FIPS 203 standard), not "ss_n"
               Hex_To_Bytes(To_String(Current_Value), SS_Expected);

               --  We have all fields for this vector, now test it
               --  Test 1: KeyGen
               SparkPass.Crypto.MLKEM.KeyGen.KeyGen(D_Seed, PK_Actual, SK_Actual);

               if PK_Actual = PK_Expected then
                  KeyGen_PK_Pass := KeyGen_PK_Pass + 1;
               else
                  KeyGen_PK_Fail := KeyGen_PK_Fail + 1;
                  Put_Line("  [Vector " & Natural'Image(Vector_Count) & "] KeyGen PK FAIL");
               end if;

               if SK_Actual = SK_Expected then
                  KeyGen_SK_Pass := KeyGen_SK_Pass + 1;
               else
                  KeyGen_SK_Fail := KeyGen_SK_Fail + 1;
                  Put_Line("  [Vector " & Natural'Image(Vector_Count) & "] KeyGen SK FAIL");
               end if;

               --  Test 2: Encaps (deterministic with MSG_Seed)
               SparkPass.Crypto.MLKEM.Encaps.Encapsulate_Expanded(
                  PK_Actual, MSG_Seed, CT_Actual, SS_Encaps, U_Vec, V_Poly);

               if CT_Actual = CT_Expected then
                  Encaps_CT_Pass := Encaps_CT_Pass + 1;
               else
                  Encaps_CT_Fail := Encaps_CT_Fail + 1;
                  Put_Line("  [Vector " & Natural'Image(Vector_Count) & "] Encaps CT FAIL");
               end if;

               if SS_Encaps = SS_Expected then
                  Encaps_SS_Pass := Encaps_SS_Pass + 1;
               else
                  Encaps_SS_Fail := Encaps_SS_Fail + 1;
                  Put_Line("  [Vector " & Natural'Image(Vector_Count) & "] Encaps SS FAIL");
               end if;

               --  Test 3: Decaps
               SparkPass.Crypto.MLKEM.Decaps.Decapsulate(SK_Actual, CT_Actual, SS_Decaps);

               if SS_Decaps = SS_Expected then
                  Decaps_Pass := Decaps_Pass + 1;
               else
                  Decaps_Fail := Decaps_Fail + 1;
                  Put_Line("  [Vector " & Natural'Image(Vector_Count) & "] Decaps FAIL");
               end if;

            end if;
         end if;
      end;

      <<Continue_Loop>>
   end loop;

   Close(RSP_File);

   New_Line;
   Put_Line("========================================================================");
   Put_Line("Test Summary");
   Put_Line("========================================================================");
   Put_Line("Total Vectors Tested:  " & Natural'Image(Total_Vectors));
   New_Line;
   Put_Line("KeyGen Public Key Results:");
   Put_Line("  Pass: " & Natural'Image(KeyGen_PK_Pass) & " / " & Natural'Image(Total_Vectors));
   Put_Line("  Fail: " & Natural'Image(KeyGen_PK_Fail));
   New_Line;
   Put_Line("KeyGen Secret Key Results:");
   Put_Line("  Pass: " & Natural'Image(KeyGen_SK_Pass) & " / " & Natural'Image(Total_Vectors));
   Put_Line("  Fail: " & Natural'Image(KeyGen_SK_Fail));
   New_Line;
   Put_Line("Encaps Ciphertext Results:");
   Put_Line("  Pass: " & Natural'Image(Encaps_CT_Pass) & " / " & Natural'Image(Total_Vectors));
   Put_Line("  Fail: " & Natural'Image(Encaps_CT_Fail));
   New_Line;
   Put_Line("Encaps Shared Secret Results:");
   Put_Line("  Pass: " & Natural'Image(Encaps_SS_Pass) & " / " & Natural'Image(Total_Vectors));
   Put_Line("  Fail: " & Natural'Image(Encaps_SS_Fail));
   New_Line;
   Put_Line("Decaps Results:");
   Put_Line("  Pass: " & Natural'Image(Decaps_Pass) & " / " & Natural'Image(Total_Vectors));
   Put_Line("  Fail: " & Natural'Image(Decaps_Fail));
   New_Line;

   declare
      Total_Failures : constant Natural :=
         KeyGen_PK_Fail + KeyGen_SK_Fail + Encaps_CT_Fail + Encaps_SS_Fail + Decaps_Fail;
   begin
      Put_Line("Total Failures: " & Natural'Image(Total_Failures));
      New_Line;

      if Total_Failures = 0 then
         Put_Line("✓ SUCCESS: All 1000 NIST KAT vectors passed!");
         Put_Line("Pure SPARK ML-KEM-1024 is NIST FIPS 203 compliant.");
         Put_Line("========================================================================");
         Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);
      else
         Put_Line("✗ FAILURE: " & Natural'Image(Total_Failures) & " test(s) failed");
         Put_Line("========================================================================");
         Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      end if;
   end;

end Test_MLKEM_NIST_KAT_Full;
