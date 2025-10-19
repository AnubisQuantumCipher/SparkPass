--  Detailed trace of Keccak state for debugging
with Ada.Text_IO; use Ada.Text_IO;
with SparkPass.Types; use SparkPass.Types;
with Interfaces; use Interfaces;

procedure Test_Keccak_Trace is

   type Lane_Index is range 0 .. 4;
   type State_Array is array (Lane_Index, Lane_Index) of U64;

   procedure Print_State (S : State_Array; Label : String) is
   begin
      Put_Line (Label & ":");
      for Y in Lane_Index loop
         Put ("  Y=" & Y'Image & ": ");
         for X in Lane_Index loop
            Put (S(Y, X)'Image & " ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_State;

   procedure Print_Lane_Hex (Lane : U64; Label : String) is
      function Hex (B : U8) return String is
         H : constant String := "0123456789abcdef";
      begin
         return (1 => H(Natural(Shift_Right(B, 4)) + 1),
                 2 => H(Natural(B and 16#0F#) + 1));
      end Hex;
   begin
      Put (Label & ": ");
      for I in reverse 0 .. 7 loop
         Put (Hex(U8(Shift_Right(Lane, 8 * I) and 16#FF#)) & " ");
      end loop;
      New_Line;
   end Print_Lane_Hex;

   State : State_Array := (others => (others => 0));

   --  Input "abc" = 0x61 0x62 0x63
   --  After padding: 0x61 0x62 0x63 0x06 0x00...00 0x80
   --  Rate for SHA3-256 = 136 bytes

   Input_Bytes : Byte_Array(1 .. 136) := (others => 0);

begin
   --  Prepare padded input
   Input_Bytes(1) := 16#61#;  -- 'a'
   Input_Bytes(2) := 16#62#;  -- 'b'
   Input_Bytes(3) := 16#63#;  -- 'c'
   Input_Bytes(4) := 16#06#;  -- SHA3 domain separator
   Input_Bytes(136) := 16#80#;  -- Padding bit

   Put_Line ("===========================================");
   Put_Line ("SHA3-256 Trace for input 'abc'");
   Put_Line ("===========================================");
   New_Line;

   Put_Line ("Input (first 16 bytes):");
   for I in 1 .. 16 loop
      Put (Input_Bytes(I)'Image & " ");
   end loop;
   New_Line;
   Put_Line ("Last byte: " & Input_Bytes(136)'Image);
   New_Line;

   --  Absorb first 8 bytes into lane (0,0)
   declare
      Lane_0_0 : U64 := 0;
   begin
      for I in 0 .. 7 loop
         Lane_0_0 := Lane_0_0 or Shift_Left(U64(Input_Bytes(I + 1)), 8 * I);
      end loop;
      State(0, 0) := Lane_0_0;
      Print_Lane_Hex(Lane_0_0, "Lane (0,0) after absorb");
   end;

   --  Absorb remaining lanes
   for Offset_Lane in 1 .. (136 / 8) - 1 loop
      declare
         X : constant Lane_Index := Lane_Index(Offset_Lane mod 5);
         Y : constant Lane_Index := Lane_Index(Offset_Lane / 5);
         Lane_Val : U64 := 0;
         Byte_Idx : Natural := Offset_Lane * 8 + 1;
      begin
         for I in 0 .. 7 loop
            Lane_Val := Lane_Val or Shift_Left(U64(Input_Bytes(Byte_Idx + I)), 8 * I);
         end loop;
         State(Y, X) := Lane_Val;
      end;
   end loop;

   Print_State (State, "State after absorb (before permutation)");

   Put_Line ("Lane (0,0) should contain bytes 0-7 in little-endian:");
   Put_Line ("Expected: 0x80...06636261 (padding|00..|06|c|b|a)");
   Print_Lane_Hex(State(0, 0), "Actual  ");
   New_Line;

   Put_Line ("===========================================");
   Put_Line ("This is where we need to apply Permute()");
   Put_Line ("Then Squeeze to get the 32-byte hash");
   Put_Line ("===========================================");

end Test_Keccak_Trace;
