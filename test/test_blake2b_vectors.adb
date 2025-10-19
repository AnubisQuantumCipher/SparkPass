pragma SPARK_Mode (Off);  -- Test harness uses I/O

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Blake2b;

--  Blake2b Test Vector Validation (RFC 7693 Appendix A)
--
--  This test harness validates the Blake2b implementation against
--  official test vectors from RFC 7693. It tests:
--    1. Empty message (0 bytes)
--    2. "abc" message (3 bytes)
--    3. Long message (256 bytes)
--
--  All test vectors are from RFC 7693 Appendix A and the official
--  Blake2 reference implementation test suite.

procedure Test_Blake2b_Vectors is
   use SparkPass.Crypto.Blake2b;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   --  Helper: Print byte array as hex
   procedure Print_Hex (Name : String; Data : Byte_Array) is
   begin
      Put (Name & ": ");
      for I in Data'Range loop
         Put (U8'Image (Data (I)));
         if I /= Data'Last then
            Put (" ");
         end if;
      end loop;
      New_Line;
   end Print_Hex;

   --  Helper: Compare byte arrays
   function Equals (A, B : Byte_Array) return Boolean is
   begin
      if A'Length /= B'Length then
         return False;
      end if;

      for I in A'Range loop
         if A (I) /= B (I - A'First + B'First) then
            return False;
         end if;
      end loop;

      return True;
   end Equals;

   --  Test case wrapper
   procedure Test_Case
     (Name     : String;
      Message  : Byte_Array;
      Expected : Hash_Type)
   is
      Result : Hash_Type;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("================================================================================");
      Put_Line ("Test " & Natural'Image (Test_Count) & ": " & Name);
      Put_Line ("================================================================================");

      Hash (Message, Result);

      if Equals (Result, Expected) then
         Put_Line ("PASS: " & Name);
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("FAIL: " & Name);
         Print_Hex ("  Expected", Expected);
         Print_Hex ("  Got     ", Result);
      end if;

      New_Line;
   end Test_Case;

   ------------------------------------------------------------
   --  Test Vector 1: Empty Message
   ------------------------------------------------------------

   --  Input: "" (0 bytes)
   --  Blake2b-512(""): (RFC 7693 Appendix A)
   Empty_Message : Byte_Array (1 .. 0);

   Empty_Expected : constant Hash_Type :=
     (16#78#, 16#6a#, 16#02#, 16#f7#, 16#42#, 16#01#, 16#59#, 16#03#,
      16#c6#, 16#c6#, 16#fd#, 16#85#, 16#25#, 16#52#, 16#d2#, 16#72#,
      16#91#, 16#2f#, 16#47#, 16#40#, 16#e1#, 16#58#, 16#47#, 16#61#,
      16#8a#, 16#86#, 16#e2#, 16#17#, 16#f7#, 16#1f#, 16#54#, 16#19#,
      16#d2#, 16#5e#, 16#10#, 16#31#, 16#af#, 16#ee#, 16#58#, 16#53#,
      16#13#, 16#89#, 16#64#, 16#44#, 16#93#, 16#4e#, 16#b0#, 16#4b#,
      16#90#, 16#3a#, 16#68#, 16#5b#, 16#14#, 16#48#, 16#b7#, 16#55#,
      16#d5#, 16#6f#, 16#70#, 16#1a#, 16#fe#, 16#9b#, 16#e2#, 16#ce#);

   ------------------------------------------------------------
   --  Test Vector 2: "abc"
   ------------------------------------------------------------

   --  Input: "abc" (3 bytes: 0x61 0x62 0x63)
   --  Blake2b-512("abc"): (RFC 7693 Appendix A)
   ABC_Message : constant Byte_Array (1 .. 3) :=
     (16#61#, 16#62#, 16#63#);

   ABC_Expected : constant Hash_Type :=
     (16#ba#, 16#80#, 16#a5#, 16#3f#, 16#98#, 16#1c#, 16#4d#, 16#0d#,
      16#6a#, 16#27#, 16#97#, 16#b6#, 16#9f#, 16#12#, 16#f6#, 16#e9#,
      16#4c#, 16#21#, 16#2f#, 16#14#, 16#68#, 16#5a#, 16#c4#, 16#b7#,
      16#4b#, 16#12#, 16#bb#, 16#6f#, 16#db#, 16#ff#, 16#a2#, 16#d1#,
      16#7d#, 16#87#, 16#c5#, 16#39#, 16#2a#, 16#ab#, 16#79#, 16#2d#,
      16#c2#, 16#52#, 16#d5#, 16#de#, 16#45#, 16#33#, 16#cc#, 16#95#,
      16#18#, 16#d3#, 16#8a#, 16#a8#, 16#db#, 16#f1#, 16#92#, 16#5a#,
      16#b9#, 16#23#, 16#86#, 16#ed#, 16#d4#, 16#00#, 16#99#, 16#23#);

   ------------------------------------------------------------
   --  Test Vector 3: 128-byte block
   ------------------------------------------------------------

   --  Input: 128 bytes of 0x00
   --  Tests single complete block processing
   Block128_Message : constant Byte_Array (1 .. 128) := (others => 0);

   Block128_Expected : constant Hash_Type :=
     (16#86#, 16#59#, 16#39#, 16#e1#, 16#20#, 16#e6#, 16#80#, 16#54#,
      16#38#, 16#47#, 16#88#, 16#41#, 16#af#, 16#b7#, 16#39#, 16#ae#,
      16#42#, 16#50#, 16#cf#, 16#37#, 16#26#, 16#53#, 16#07#, 16#8a#,
      16#06#, 16#5c#, 16#dc#, 16#ff#, 16#fc#, 16#a4#, 16#ca#, 16#f7#,
      16#98#, 16#e6#, 16#d4#, 16#62#, 16#b6#, 16#5d#, 16#65#, 16#8f#,
      16#c1#, 16#65#, 16#78#, 16#26#, 16#40#, 16#ed#, 16#ed#, 16#70#,
      16#96#, 16#34#, 16#49#, 16#ae#, 16#15#, 16#00#, 16#fb#, 16#0f#,
      16#24#, 16#98#, 16#1d#, 16#77#, 16#27#, 16#e2#, 16#2c#, 16#41#);

   ------------------------------------------------------------
   --  Test Vector 4: 256-byte message
   ------------------------------------------------------------

   --  Input: 256 bytes of 0x00
   --  Tests multi-block processing (2 complete blocks)
   Block256_Message : constant Byte_Array (1 .. 256) := (others => 0);

   Block256_Expected : constant Hash_Type :=
     (16#ec#, 16#9c#, 16#6b#, 16#30#, 16#1a#, 16#6c#, 16#98#, 16#94#,
      16#6d#, 16#74#, 16#2a#, 16#74#, 16#71#, 16#0e#, 16#65#, 16#8f#,
      16#02#, 16#43#, 16#e0#, 16#e6#, 16#d3#, 16#52#, 16#5f#, 16#4a#,
      16#fa#, 16#94#, 16#df#, 16#c2#, 16#39#, 16#54#, 16#56#, 16#fa#,
      16#54#, 16#eb#, 16#e5#, 16#ef#, 16#0f#, 16#41#, 16#3b#, 16#5a#,
      16#9a#, 16#bf#, 16#e6#, 16#50#, 16#1d#, 16#ab#, 16#b4#, 16#b9#,
      16#a0#, 16#fb#, 16#ca#, 16#16#, 16#4d#, 16#6c#, 16#d8#, 16#0b#,
      16#1e#, 16#79#, 16#db#, 16#be#, 16#d8#, 16#d4#, 16#20#, 16#2e#);

   ------------------------------------------------------------
   --  Test Vector 5: One-byte message
   ------------------------------------------------------------

   --  Input: 0x00 (1 byte)
   --  Tests minimal message
   OneByte_Message : constant Byte_Array (1 .. 1) := (1 => 16#00#);

   OneByte_Expected : constant Hash_Type :=
     (16#2f#, 16#a3#, 16#f6#, 16#86#, 16#df#, 16#87#, 16#69#, 16#95#,
      16#16#, 16#7e#, 16#7c#, 16#2e#, 16#5d#, 16#74#, 16#c4#, 16#c7#,
      16#b6#, 16#e4#, 16#8f#, 16#80#, 16#68#, 16#fe#, 16#0e#, 16#44#,
      16#20#, 16#83#, 16#44#, 16#d4#, 16#80#, 16#f7#, 16#90#, 16#4c#,
      16#36#, 16#96#, 16#3e#, 16#44#, 16#11#, 16#5f#, 16#e3#, 16#eb#,
      16#2a#, 16#3a#, 16#c8#, 16#69#, 16#4c#, 16#28#, 16#bc#, 16#b4#,
      16#f5#, 16#a0#, 16#f3#, 16#27#, 16#6f#, 16#2e#, 16#79#, 16#48#,
      16#7d#, 16#82#, 16#19#, 16#05#, 16#7a#, 16#50#, 16#6e#, 16#4b#);

begin
   Put_Line ("Blake2b-512 Test Vector Validation");
   Put_Line ("RFC 7693 Appendix A + Reference Implementation");
   New_Line;

   --  Run all test cases
   Test_Case ("Empty Message (0 bytes)", Empty_Message, Empty_Expected);
   Test_Case ("ABC Message (3 bytes)", ABC_Message, ABC_Expected);
   Test_Case ("One Byte Message", OneByte_Message, OneByte_Expected);
   Test_Case ("128-Byte Block", Block128_Message, Block128_Expected);
   Test_Case ("256-Byte Message (2 blocks)", Block256_Message, Block256_Expected);

   --  Summary
   Put_Line ("================================================================================");
   Put_Line ("TEST SUMMARY");
   Put_Line ("================================================================================");
   Put_Line ("Total tests: " & Natural'Image (Test_Count));
   Put_Line ("Passed:      " & Natural'Image (Pass_Count));
   Put_Line ("Failed:      " & Natural'Image (Test_Count - Pass_Count));
   New_Line;

   if Pass_Count = Test_Count then
      Put_Line ("*** ALL TESTS PASSED ***");
      Put_Line ("Blake2b implementation is RFC 7693 compliant!");
   else
      Put_Line ("*** SOME TESTS FAILED ***");
      Put_Line ("Blake2b implementation needs debugging.");
   end if;
end Test_Blake2b_Vectors;
