--  Argon2id Diagnostic Test - RFC 9106 Vector 1 Deep Dive
--
--  This test implements Test Vector 1 from RFC 9106 with comprehensive
--  debug output to identify the exact divergence point.
--
--  Expected values from RFC 9106 / phc-winner-argon2 reference:
--    Password: "password" (8 bytes)
--    Salt: "somesaltSOMESALTsomesaltSOMESALT" (32 bytes)
--    Memory: 16384 KiB (16 MiB)
--    Iterations: 4
--    Parallelism: 1
--    Output: dbda37811a190cf4dffda38f6aaeef2f2bb74c675d1c333512790d4d902107a3

pragma Ada_2012;
pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Crypto.Argon2id;
with SparkPass.Crypto.Blake2b;
with SparkPass.Types; use SparkPass.Types;

procedure Test_Argon2_Diagnostic is

   package Argon2id renames SparkPass.Crypto.Argon2id;
   package Blake2b renames SparkPass.Crypto.Blake2b;

   subtype Byte is U8;

   --  Test parameters matching RFC 9106 Test Vector 1
   Password : constant Byte_Array := (
      16#70#, 16#61#, 16#73#, 16#73#, 16#77#, 16#6f#, 16#72#, 16#64#  --  "password"
   );

   Salt : constant Salt_Array := (
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  --  "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#,  --  "SOMESALT"
      16#73#, 16#6f#, 16#6d#, 16#65#, 16#73#, 16#61#, 16#6c#, 16#74#,  --  "somesalt"
      16#53#, 16#4f#, 16#4d#, 16#45#, 16#53#, 16#41#, 16#4c#, 16#54#   --  "SOMESALT"
   );

   Memory_KiB   : constant := 16_384;  --  16 MiB
   Iterations   : constant := 4;
   Parallelism  : constant := 1;
   Tag_Length   : constant := 32;

   Expected_Output : constant Key_Array := (
      16#db#, 16#da#, 16#37#, 16#81#, 16#1a#, 16#19#, 16#0c#, 16#f4#,
      16#df#, 16#fd#, 16#a3#, 16#8f#, 16#6a#, 16#ae#, 16#ef#, 16#2f#,
      16#2b#, 16#b7#, 16#4c#, 16#67#, 16#5d#, 16#1c#, 16#33#, 16#35#,
      16#12#, 16#79#, 16#0d#, 16#4d#, 16#90#, 16#21#, 16#07#, 16#a3#
   );

   Output : Key_Array := (others => 0);
   Params : Argon2id.Parameters;
   Success : Boolean := False;

   --  Helper to print hex bytes
   procedure Put_Hex (B : Byte) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put (Hex (Natural (B / 16) + 1));
      Put (Hex (Natural (B mod 16) + 1));
   end Put_Hex;

   procedure Print_Bytes (Label : String; Data : Byte_Array) is
   begin
      Put (Label & ": ");
      for I in Data'Range loop
         Put_Hex (Data (I));
      end loop;
      New_Line;
   end Print_Bytes;

   --  Manually compute H0 with debug output
   procedure Debug_H0 is
      function LE32 (Val : Unsigned_32) return Byte_Array is
         Result : Byte_Array (1 .. 4);
      begin
         Result (1) := Byte (Val and 16#FF#);
         Result (2) := Byte (Shift_Right (Val, 8) and 16#FF#);
         Result (3) := Byte (Shift_Right (Val, 16) and 16#FF#);
         Result (4) := Byte (Shift_Right (Val, 24) and 16#FF#);
         return Result;
      end LE32;

      Input : Byte_Array (1 .. 200) := (others => 0);
      Offset : Natural := 0;
      H0_Out : Byte_Array (1 .. 64);

      Version : constant Unsigned_32 := 16#13#;      --  19 decimal
      Type_Id : constant Unsigned_32 := 2;           --  Argon2id
   begin
      Put_Line ("========================================");
      Put_Line ("H0 COMPUTATION (RFC 9106 Section 3.4)");
      Put_Line ("========================================");

      --  Build H0 input according to RFC 9106 Section 3.4
      --  H0 = H^(64)(LE32(p) || LE32(τ) || LE32(m) || LE32(t) || LE32(v) || LE32(y) ||
      --               LE32(|P|) || P || LE32(|S|) || S || LE32(|K|) || K || LE32(|X|) || X)

      Put_Line ("Parameter encoding:");

      --  1. Parallelism (p)
      Input (Offset + 1 .. Offset + 4) := LE32 (Unsigned_32 (Parallelism));
      Put ("  p (parallelism) = " & Parallelism'Image & " -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      --  2. Tag length (τ)
      Input (Offset + 1 .. Offset + 4) := LE32 (Unsigned_32 (Tag_Length));
      Put ("  τ (tag length)  = " & Tag_Length'Image & " -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      --  3. Memory (m)
      Input (Offset + 1 .. Offset + 4) := LE32 (Unsigned_32 (Memory_KiB));
      Put ("  m (memory KiB)  = " & Memory_KiB'Image & " -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      --  4. Iterations (t)
      Input (Offset + 1 .. Offset + 4) := LE32 (Unsigned_32 (Iterations));
      Put ("  t (iterations)  = " & Iterations'Image & " -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      --  5. Version (v)
      Input (Offset + 1 .. Offset + 4) := LE32 (Version);
      Put ("  v (version)     = 0x13 (19) -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      --  6. Type (y)
      Input (Offset + 1 .. Offset + 4) := LE32 (Type_Id);
      Put ("  y (type)        = 2 (Argon2id) -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      --  7. Password length
      Input (Offset + 1 .. Offset + 4) := LE32 (Unsigned_32 (Password'Length));
      Put ("  |P| (pwd len)   = " & Password'Length'Image & " -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      --  8. Password
      Input (Offset + 1 .. Offset + Password'Length) := Password;
      Print_Bytes ("  P (password)", Password);
      Offset := Offset + Password'Length;

      --  9. Salt length
      Input (Offset + 1 .. Offset + 4) := LE32 (32);
      Put ("  |S| (salt len)  = 32 -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      --  10. Salt
      Input (Offset + 1 .. Offset + 32) := Salt;
      Print_Bytes ("  S (salt)", Salt);
      Offset := Offset + 32;

      --  11. Key length (0)
      Input (Offset + 1 .. Offset + 4) := LE32 (0);
      Put ("  |K| (key len)   = 0 -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      --  12. Associated data length (0)
      Input (Offset + 1 .. Offset + 4) := LE32 (0);
      Put ("  |X| (AD len)    = 0 -> ");
      Print_Bytes ("LE32", Input (Offset + 1 .. Offset + 4));
      Offset := Offset + 4;

      Put_Line ("");
      Put_Line ("Total H0 input length: " & Offset'Image & " bytes");
      Print_Bytes ("Full H0 input", Input (1 .. Offset));

      --  Compute Blake2b-512
      Blake2b.Hash (Message => Input (1 .. Offset), Output => H0_Out);

      Put_Line ("");
      Print_Bytes ("Computed H0 (64 bytes)", H0_Out);

      --  TODO: Compare against reference implementation's H0
      Put_Line ("");
      Put_Line ("ACTION: Compare this H0 against phc-winner-argon2 reference output");
      Put_Line ("========================================");
      New_Line;
   end Debug_H0;

begin
   Put_Line ("========================================================================");
   Put_Line ("Argon2id Diagnostic Test - RFC 9106 Test Vector 1");
   Put_Line ("========================================================================");
   Put_Line ("Parameters:");
   Put_Line ("  Password:    ""password"" (8 bytes)");
   Put_Line ("  Salt:        ""somesaltSOMESALTsomesaltSOMESALT"" (32 bytes)");
   Put_Line ("  Memory:      16384 KiB (16 MiB)");
   Put_Line ("  Iterations:  4");
   Put_Line ("  Parallelism: 1");
   Put_Line ("  Tag Length:  32 bytes");
   New_Line;

   --  Step 1: Debug H0 computation
   Debug_H0;

   --  Step 2: Run full Argon2id and compare
   Put_Line ("========================================");
   Put_Line ("RUNNING FULL ARGON2ID");
   Put_Line ("========================================");

   Params.Memory_Cost := Memory_KiB;
   Params.Iterations := Iterations;
   Params.Parallelism := Parallelism;
   Params.Salt := Salt;

   Argon2id.Derive (Password, Params, Output, Success);

   if not Success then
      Put_Line ("ERROR: Argon2id.Derive failed");
      return;
   end if;

   New_Line;
   Print_Bytes ("Expected output", Expected_Output);
   Print_Bytes ("Actual output  ", Output);

   New_Line;
   if Output = Expected_Output then
      Put_Line ("✓ SUCCESS: Output matches RFC 9106 Test Vector 1");
   else
      Put_Line ("✗ FAIL: Output mismatch");

      --  Find first divergent byte
      for I in 1 .. 32 loop
         if Output (I) /= Expected_Output (I) then
            Put_Line ("First mismatch at byte" & I'Image & ":");
            Put ("  Expected: ");
            Put_Hex (Expected_Output (I));
            New_Line;
            Put ("  Actual:   ");
            Put_Hex (Output (I));
            New_Line;
            exit;
         end if;
      end loop;
   end if;

   Put_Line ("========================================================================");
end Test_Argon2_Diagnostic;
