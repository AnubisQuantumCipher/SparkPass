--  H' (Variable-Length Hash) Debug Test
--
--  Tests the H' function used in Argon2id initial block generation.
--  H'(1024, input) should produce 1024 bytes output using iterated Blake2b.

pragma Ada_2012;
pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Argon2id.HPrime;
with SparkPass.Crypto.Blake2b;

procedure Test_HPrime_Debug is

   package HPrime renames SparkPass.Crypto.Argon2id.HPrime;
   package Blake2b renames SparkPass.Crypto.Blake2b;

   --  Helper to print hex bytes
   procedure Put_Hex (B : U8) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (B / 16) + 1));
      Put (Hex (Natural (B mod 16) + 1));
   end Put_Hex;

   procedure Print_Bytes (Label : String; Data : Byte_Array; Max : Natural := 64) is
      Count : constant Natural := Natural'Min (Data'Length, Max);
   begin
      Put (Label & ": ");
      for I in Data'First .. Data'First + Count - 1 loop
         Put_Hex (Data (I));
      end loop;
      if Data'Length > Max then
         Put (" ...");
      end if;
      New_Line;
   end Print_Bytes;

   --  Test 1: Short output (<= 64 bytes)
   procedure Test_Short_Output is
      Input : constant Byte_Array := (1, 2, 3, 4, 5, 6, 7, 8);
      Output : Byte_Array (1 .. 32);
   begin
      Put_Line ("========================================");
      Put_Line ("Test 1: H'(32, input) - Short Output");
      Put_Line ("========================================");
      Print_Bytes ("Input (8 bytes)", Input);

      HPrime.Compute_H_Prime (
         Output_Length => 32,
         Input => Input,
         Output => Output
      );

      Print_Bytes ("Output (32 bytes)", Output);
      Put_Line ("");
   end Test_Short_Output;

   --  Test 2: Exact Blake2b output size (64 bytes)
   procedure Test_Blake2b_Size is
      Input : constant Byte_Array := (1, 2, 3, 4, 5, 6, 7, 8);
      Output : Byte_Array (1 .. 64);
   begin
      Put_Line ("========================================");
      Put_Line ("Test 2: H'(64, input) - Blake2b Size");
      Put_Line ("========================================");
      Print_Bytes ("Input (8 bytes)", Input);

      HPrime.Compute_H_Prime (
         Output_Length => 64,
         Input => Input,
         Output => Output
      );

      Print_Bytes ("Output (64 bytes)", Output);
      Put_Line ("");
   end Test_Blake2b_Size;

   --  Test 3: Long output (1024 bytes for Argon2 block)
   procedure Test_Long_Output is
      function LE32 (Val : Unsigned_32) return Byte_Array is
         Result : Byte_Array (1 .. 4);
      begin
         Result (1) := U8 (Val and 16#FF#);
         Result (2) := U8 (Shift_Right (Val, 8) and 16#FF#);
         Result (3) := U8 (Shift_Right (Val, 16) and 16#FF#);
         Result (4) := U8 (Shift_Right (Val, 24) and 16#FF#);
         return Result;
      end LE32;

      --  Simulate first block generation: H'(1024, H0 || LE32(0) || LE32(0))
      --  Using a fake H0 for testing
      H0 : constant Byte_Array (1 .. 64) := (others => 16#AA#);
      Input : Byte_Array (1 .. 72);
      Output : Byte_Array (1 .. 1024);
   begin
      Put_Line ("========================================");
      Put_Line ("Test 3: H'(1024, H0||0||0) - Long Output");
      Put_Line ("========================================");

      --  Build input: H0 || LE32(block_index=0) || LE32(lane=0)
      Input (1 .. 64) := H0;
      Input (65 .. 68) := LE32 (0);  --  Block index
      Input (69 .. 72) := LE32 (0);  --  Lane

      Print_Bytes ("Input (72 bytes)", Input);

      HPrime.Compute_H_Prime (
         Output_Length => 1024,
         Input => Input,
         Output => Output
      );

      Put_Line ("Output (1024 bytes):");
      Print_Bytes ("  First 64 bytes", Output (1 .. 64));
      Print_Bytes ("  Bytes 65-128", Output (65 .. 128));
      Print_Bytes ("  Bytes 129-192", Output (129 .. 192));
      Print_Bytes ("  Last 64 bytes", Output (961 .. 1024));
      Put_Line ("");
   end Test_Long_Output;

   --  Test 4: Manual H' algorithm trace for 128 bytes
   procedure Test_Manual_Trace is
      function LE32 (Val : Unsigned_32) return Byte_Array is
         Result : Byte_Array (1 .. 4);
      begin
         Result (1) := U8 (Val and 16#FF#);
         Result (2) := U8 (Shift_Right (Val, 8) and 16#FF#);
         Result (3) := U8 (Shift_Right (Val, 16) and 16#FF#);
         Result (4) := U8 (Shift_Right (Val, 24) and 16#FF#);
         return Result;
      end LE32;

      Input : constant Byte_Array := (1, 2, 3, 4);
      Tau_Bytes : constant Byte_Array := LE32 (128);  --  Output length
      V1_Input : Byte_Array (1 .. 8);
      V1, V2, V3 : Byte_Array (1 .. 64);
      Output_Manual : Byte_Array (1 .. 128);
      Output_HPrime : Byte_Array (1 .. 128);
   begin
      Put_Line ("========================================");
      Put_Line ("Test 4: Manual H' Trace (128 bytes)");
      Put_Line ("========================================");

      --  V₁ = Blake2b-512(LE32(128) || Input)
      V1_Input (1 .. 4) := Tau_Bytes;
      V1_Input (5 .. 8) := Input;

      Put_Line ("Computing V₁ = Blake2b-512(LE32(128) || input):");
      Print_Bytes ("  V₁ input", V1_Input);

      Blake2b.Hash (Message => V1_Input, Output => V1);
      Print_Bytes ("  V₁ output (64 bytes)", V1);

      --  Copy first 32 bytes of V₁
      Output_Manual (1 .. 32) := V1 (1 .. 32);

      --  V₂ = Blake2b-512(V₁)
      Put_Line ("Computing V₂ = Blake2b-512(V₁):");
      Blake2b.Hash (Message => V1, Output => V2);
      Print_Bytes ("  V₂ output (64 bytes)", V2);

      --  Copy first 32 bytes of V₂
      Output_Manual (33 .. 64) := V2 (1 .. 32);

      --  V₃ = Blake2b-512(V₂)
      Put_Line ("Computing V₃ = Blake2b-512(V₂):");
      Blake2b.Hash (Message => V2, Output => V3);
      Print_Bytes ("  V₃ output (64 bytes)", V3);

      --  Copy first 32 bytes of V₃
      Output_Manual (65 .. 96) := V3 (1 .. 32);

      --  V₄ = Blake2b-512(V₃)
      Put_Line ("Computing V₄ = Blake2b-512(V₃) [partial]:");
      declare
         V4 : Byte_Array (1 .. 64);
      begin
         Blake2b.Hash (Message => V3, Output => V4);
         Print_Bytes ("  V₄ output (64 bytes)", V4);

         --  Copy remaining 32 bytes
         Output_Manual (97 .. 128) := V4 (1 .. 32);
      end;

      Put_Line ("");
      Print_Bytes ("Manual H'(128) output", Output_Manual);

      --  Now compare with HPrime function
      HPrime.Compute_H_Prime (
         Output_Length => 128,
         Input => Input,
         Output => Output_HPrime
      );

      Print_Bytes ("HPrime.Compute output", Output_HPrime);

      Put_Line ("");
      if Output_Manual = Output_HPrime then
         Put_Line ("✓ SUCCESS: H' implementation matches manual trace");
      else
         Put_Line ("✗ FAIL: H' implementation differs from manual trace");

         --  Find first mismatch
         for I in 1 .. 128 loop
            if Output_Manual (I) /= Output_HPrime (I) then
               Put_Line ("First mismatch at byte" & I'Image);
               Put ("  Manual:  ");
               Put_Hex (Output_Manual (I));
               New_Line;
               Put ("  HPrime:  ");
               Put_Hex (Output_HPrime (I));
               New_Line;
               exit;
            end if;
         end loop;
      end if;

      Put_Line ("");
   end Test_Manual_Trace;

begin
   Put_Line ("========================================================================");
   Put_Line ("H' (Variable-Length Hash) Debug Test");
   Put_Line ("========================================================================");
   New_Line;

   Test_Short_Output;
   Test_Blake2b_Size;
   Test_Long_Output;
   Test_Manual_Trace;

   Put_Line ("========================================================================");
end Test_HPrime_Debug;
