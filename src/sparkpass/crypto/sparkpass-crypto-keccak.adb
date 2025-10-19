--  ============================================================================
--  SparkPass - Keccak-f[1600] Permutation Implementation (Pure SPARK)
--  ============================================================================

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.Keccak is

   --  Build verification tag
   pragma Warnings (Off);
   Keccak_Build_Tag : constant String := "CRYPTO-FIXES-2025-10-18: Keccak State(Y,X) indexing + Argon2id Memory init";
   pragma Warnings (On);

   --  =========================================================================
   --  Keccak-f[1600] Round Constants
   --  =========================================================================

   --  FIPS 202 Section 3.2.5, Table 2: Round constants RC[ir] for ι step
   --  These are precomputed outputs of LFSR(8·ir) to break symmetry
   --  Constant time: All data publicly known, no secret-dependent access
   RC : constant Round_Constants_Array := (
      16#0000000000000001#,  -- Round  0
      16#0000000000008082#,  -- Round  1
      16#800000000000808A#,  -- Round  2
      16#8000000080008000#,  -- Round  3
      16#000000000000808B#,  -- Round  4
      16#0000000080000001#,  -- Round  5
      16#8000000080008081#,  -- Round  6
      16#8000000000008009#,  -- Round  7
      16#000000000000008A#,  -- Round  8
      16#0000000000000088#,  -- Round  9
      16#0000000080008009#,  -- Round 10
      16#000000008000000A#,  -- Round 11
      16#000000008000808B#,  -- Round 12
      16#800000000000008B#,  -- Round 13
      16#8000000000008089#,  -- Round 14
      16#8000000000008003#,  -- Round 15
      16#8000000000008002#,  -- Round 16
      16#8000000000000080#,  -- Round 17
      16#000000000000800A#,  -- Round 18
      16#800000008000000A#,  -- Round 19
      16#8000000080008081#,  -- Round 20
      16#8000000000008080#,  -- Round 21
      16#0000000080000001#,  -- Round 22
      16#8000000080008008#   -- Round 23
   );

   --  =========================================================================
   --  Rotation Offsets for ρ (rho) Step
   --  =========================================================================

   --  FIPS 202 Section 3.2.2, Table 1: Rotation offsets r[x,y]
   --  For ρ step: A'[x,y] = ROT(A[x,y], r[x,y])
   --  Constant time: All offsets publicly known
   type Rotation_Offsets_Array is array (Lane_Index, Lane_Index) of Natural;

   Rotation_Offsets : constant Rotation_Offsets_Array := (
      0 => ( 0,  1, 62, 28, 27),  -- x = 0
      1 => (36, 44,  6, 55, 20),  -- x = 1
      2 => ( 3, 10, 43, 25, 39),  -- x = 2
      3 => (41, 45, 15, 21,  8),  -- x = 3
      4 => (18,  2, 61, 56, 14)   -- x = 4
   );

   --  =========================================================================
   --  Bit Rotation (Constant Time)
   --  =========================================================================

   --  **Left rotation of 64-bit word by N positions**
   --  Constant time: Rotation count is publicly known (not secret-dependent)
   function ROL (X : U64; N : Natural) return U64
     with
       Inline,
       Global => null,
       Pre    => N <= 64,
       Post   => True;

   function ROL (X : U64; N : Natural) return U64 is
      Shift_Left  : constant Natural := N mod 64;
      Shift_Right : constant Natural := (64 - Shift_Left) mod 64;
   begin
      if Shift_Left = 0 then
         return X;
      else
         return Interfaces.Shift_Left (X, Shift_Left) or
                Interfaces.Shift_Right (X, Shift_Right);
      end if;
   end ROL;

   --  =========================================================================
   --  Keccak-f[1600] Step Mappings
   --  =========================================================================

   --  **θ (theta) step**: Column parity mixing
   --
   --  FIPS 202 Algorithm 1: θ step
   --  C[x] = A[x,0] ⊕ A[x,1] ⊕ A[x,2] ⊕ A[x,3] ⊕ A[x,4]
   --  D[x] = C[x-1] ⊕ ROT(C[x+1], 1)
   --  A'[x,y] = A[x,y] ⊕ D[x]
   --
   --  Constant time: No secret-dependent branches or memory access
   procedure Theta (State : in out State_Array)
     with
       Inline,
       Global => null;

   procedure Theta (State : in out State_Array) is
      C : array (Lane_Index) of U64;
      D : array (Lane_Index) of U64;
   begin
      --  Step 1: Compute column parities C[x]
      --  NOTE: State(Y, X) stores lane (x, y), so State(0, X) = lane (X, 0)
      for X in Lane_Index loop
         C(X) := State(0, X) xor State(1, X) xor State(2, X) xor
                 State(3, X) xor State(4, X);
      end loop;

      --  Step 2: Compute D[x] = C[x-1] ⊕ ROT(C[x+1], 1)
      for X in Lane_Index loop
         declare
            X_Minus_1 : constant Lane_Index := Lane_Index((Integer(X) + 4) mod 5);
            X_Plus_1  : constant Lane_Index := Lane_Index((Integer(X) + 1) mod 5);
         begin
            D(X) := C(X_Minus_1) xor ROL(C(X_Plus_1), 1);
         end;
      end loop;

      --  Step 3: Update state A'[x,y] = A[x,y] ⊕ D[x]
      --  State(Y, X) stores lane (X, Y), so we XOR with D(X)
      for X in Lane_Index loop
         for Y in Lane_Index loop
            State(Y, X) := State(Y, X) xor D(X);
         end loop;
      end loop;

      --  Zeroize intermediate values (manual for U64 arrays)
      for I in Lane_Index loop
         C(I) := 0;
         D(I) := 0;
      end loop;
   end Theta;

   --  **ρ (rho) step**: Lane rotation
   --
   --  FIPS 202 Algorithm 2: ρ step
   --  A'[x,y] = ROT(A[x,y], r[x,y])
   --
   --  Constant time: Rotation offsets are publicly known
   procedure Rho (State : in out State_Array)
     with
       Inline,
       Global => null;

   procedure Rho (State : in out State_Array) is
   begin
      --  Rotate each lane according to rotation table
      --  State(Y, X) stores lane (X, Y), which needs rotation r[X, Y]
      --  Matrix is transposed: row i contains r[0,i], r[1,i], r[2,i], r[3,i], r[4,i]
      --  So Rotation_Offsets(i, j) gives r[j, i]
      --  For lane (X, Y), use Rotation_Offsets(Y, X) to get r[X, Y] ✓
      for X in Lane_Index loop
         for Y in Lane_Index loop
            State(Y, X) := ROL(State(Y, X), Rotation_Offsets(Y, X));
         end loop;
      end loop;
   end Rho;

   --  **π (pi) step**: Lane permutation
   --
   --  FIPS 202 Algorithm 3: π step
   --  A'[x,y] = A[x',y'] where [x', y'] = [y, (2x + 3y) mod 5]^T
   --
   --  Constant time: Permutation pattern is publicly known
   procedure Pi (State : in out State_Array)
     with
       Inline,
       Global => null;

   procedure Pi (State : in out State_Array) is
      Temp : State_Array;
   begin
      --  Copy current state
      Temp := State;

      --  Apply π permutation INVERSE: A'[x,y] = A[(3y+x) mod 5, x]
      --  FIPS 202 Algorithm 3 defines FORWARD: B[y, (2x+3y) mod 5] = A[x, y]
      --  We need INVERSE to implement as A' = f(A)
      --
      --  State(Y, X) stores lane (X, Y)
      --  For output A'[x, y] at State(y, x):
      --    Need input from A[(3y+x) mod 5, x] at State(x, (3y+x) mod 5)
      for X in Lane_Index loop
         for Y in Lane_Index loop
            declare
               X_Source : constant Lane_Index :=
                 Lane_Index((3 * Integer(Y) + Integer(X)) mod 5);
               Y_Source : constant Lane_Index := X;
            begin
               State(Y, X) := Temp(Y_Source, X_Source);
            end;
         end loop;
      end loop;

      --  Zeroize temporary state
      for X in Lane_Index loop
         for Y in Lane_Index loop
            Temp(Y, X) := 0;
         end loop;
      end loop;
   end Pi;

   --  **χ (chi) step**: Non-linear mixing
   --
   --  FIPS 202 Algorithm 4: χ step
   --  A'[x,y] = A[x,y] ⊕ ((NOT A[x+1,y]) AND A[x+2,y])
   --
   --  This is the only non-linear step in Keccak
   --  Constant time: No secret-dependent branches
   procedure Chi (State : in out State_Array)
     with
       Inline,
       Global => null;

   procedure Chi (State : in out State_Array) is
      Temp : State_Array;
   begin
      --  Copy current state
      Temp := State;

      --  Apply χ non-linear mixing
      --  State(Y, X) stores lane (X, Y)
      --  χ operates on rows: A'[x,y] = A[x,y] ⊕ (¬A[x+1,y] ∧ A[x+2,y])
      for X in Lane_Index loop
         for Y in Lane_Index loop
            declare
               X_Plus_1 : constant Lane_Index := Lane_Index((Integer(X) + 1) mod 5);
               X_Plus_2 : constant Lane_Index := Lane_Index((Integer(X) + 2) mod 5);
            begin
               State(Y, X) := Temp(Y, X) xor
                             ((not Temp(Y, X_Plus_1)) and Temp(Y, X_Plus_2));
            end;
         end loop;
      end loop;

      --  Zeroize temporary state
      for X in Lane_Index loop
         for Y in Lane_Index loop
            Temp(Y, X) := 0;
         end loop;
      end loop;
   end Chi;

   --  **ι (iota) step**: Round constant addition
   --
   --  FIPS 202 Algorithm 5: ι step
   --  A'[0,0] = A[0,0] ⊕ RC[ir]
   --
   --  Constant time: Round constant is publicly known
   procedure Iota (State : in out State_Array; Round_Index : Natural)
     with
       Inline,
       Global => null,
       Pre    => Round_Index < Num_Rounds;

   procedure Iota (State : in out State_Array; Round_Index : Natural) is
   begin
      State(0, 0) := State(0, 0) xor RC(Round_Index);
   end Iota;

   --  =========================================================================
   --  Keccak-f[1600] Permutation (24 Rounds)
   --  =========================================================================

   --  **Main permutation**: 24 rounds of θ, ρ, π, χ, ι
   --
   --  FIPS 202 Algorithm 7: Keccak-f[1600](S)
   --  for ir from 0 to nr-1:
   --    S = Rnd(S, ir)
   --  where Rnd(S, ir) = ι(χ(π(ρ(θ(S)))), ir)
   --
   --  Constant time: Fixed 24 iterations, no data-dependent branches
   procedure Permute (State : in out State_Array) is
   begin
      for Round in 0 .. Num_Rounds - 1 loop
         pragma Loop_Invariant (Round in 0 .. Num_Rounds - 1);

         Theta (State);
         Rho   (State);
         Pi    (State);
         Chi   (State);
         Iota  (State, Round);
      end loop;
   end Permute;

   --  =========================================================================
   --  Byte/Lane Conversion Utilities
   --  =========================================================================

   --  **Convert 8 bytes to 64-bit lane (little-endian)**
   --  Constant time: Fixed 8 iterations
   function Bytes_To_Lane (Bytes : Byte_Array) return U64
     with
       Inline,
       Global => null,
       Pre    => Bytes'Length = 8;

   function Bytes_To_Lane (Bytes : Byte_Array) return U64 is
      Result : U64 := 0;
      Offset : Natural := Bytes'First;
   begin
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         pragma Loop_Invariant (Offset = Bytes'First + I);

         Result := Result or
           Interfaces.Shift_Left (U64(Bytes(Offset)), 8 * I);
         Offset := Offset + 1;
      end loop;
      return Result;
   end Bytes_To_Lane;

   --  **Convert 64-bit lane to 8 bytes (little-endian)**
   --  Constant time: Fixed 8 iterations
   procedure Lane_To_Bytes (Lane : in U64; Bytes : out Byte_Array)
     with
       Inline,
       Global => null,
       Pre    => Bytes'Length = 8;

   procedure Lane_To_Bytes (Lane : in U64; Bytes : out Byte_Array) is
      Offset : Natural := Bytes'First;
   begin
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         pragma Loop_Invariant (Offset = Bytes'First + I);

         Bytes(Offset) := U8(Interfaces.Shift_Right (Lane, 8 * I) and 16#FF#);
         Offset := Offset + 1;
      end loop;
   end Lane_To_Bytes;

   --  =========================================================================
   --  Sponge Construction (Absorb/Squeeze)
   --  =========================================================================

   --  **Absorb a single block into the state**
   --
   --  FIPS 202 Section 4: Sponge absorbing phase
   --  State[0..rate-1] := State[0..rate-1] ⊕ Block
   --  State := Keccak-f[1600](State)
   procedure Absorb_Block (
      State      : in out State_Array;
      Block      : in Byte_Array;
      Rate_Bytes : in Positive
   ) is
      Block_Idx : Natural := Block'First;
   begin
      --  XOR block into state (lane by lane, little-endian)
      --  FIPS 202: Lane A[x,y] is at bit offset w(5y+x) = byte offset 8(5y+x)
      --  Process lanes in OFFSET ORDER (not loop iteration order!)
      --
      --  Key insight: Block bytes must be absorbed in sequential order,
      --  matching the FIPS 202 state string layout.
      Block_Idx := Block'First;

      --  Iterate through offsets 0, 8, 16, ... in order
      --  offset = 8*(5y+x), so we need to find (x,y) for each offset
      for Offset_Lanes in 0 .. (Rate_Bytes / 8) - 1 loop
         declare
            --  Convert lane index to (x,y) coordinates
            --  FIPS 202: offset_lanes = 5*y + x, so: x = offset_lanes mod 5, y = offset_lanes / 5
            X : constant Lane_Index := Lane_Index(Offset_Lanes mod 5);
            Y : constant Lane_Index := Lane_Index(Offset_Lanes / 5);
         begin
            if Block_Idx + 7 <= Block'Last then
               --  Full lane available
               --  Use State(Y, X) to match Ada column-major with FIPS 202 row-major
               State(Y, X) := State(Y, X) xor
                 Bytes_To_Lane(Block(Block_Idx .. Block_Idx + 7));
               Block_Idx := Block_Idx + 8;
            elsif Block_Idx <= Block'Last then
               --  Partial lane (pad with zeros)
               declare
                  Temp_Bytes : Byte_Array(1 .. 8) := (others => 0);
                  Copy_Count : constant Natural := Block'Last - Block_Idx + 1;
               begin
                  Temp_Bytes(1 .. Copy_Count) :=
                    Block(Block_Idx .. Block'Last);
                  State(Y, X) := State(Y, X) xor Bytes_To_Lane(Temp_Bytes);
                  Block_Idx := Block_Idx + Copy_Count;
               end;
            end if;
         end;
      end loop;

      --  Apply Keccak-f[1600] permutation
      Permute (State);
   end Absorb_Block;

   --  **Squeeze a single block from the state**
   --
   --  FIPS 202 Section 4: Sponge squeezing phase
   --  Output := State[0..output_length-1]
   procedure Squeeze_Block (
      State      : in State_Array;
      Output     : out Byte_Array;
      Rate_Bytes : in Positive
   ) is
      Output_Idx : Natural := Output'First;
   begin
      --  Extract bytes from state (lane by lane, little-endian)
      --  FIPS 202: Lane A[x,y] is at byte offset 8(5y+x)
      --  Extract lanes in OFFSET ORDER matching state string layout
      Output_Idx := Output'First;

      --  Iterate through lane offsets 0, 8, 16, ... in order
      for Offset_Lanes in 0 .. (Rate_Bytes / 8) - 1 loop
         exit when Output_Idx > Output'Last;

         declare
            --  Convert lane index to (x,y) coordinates
            --  FIPS 202: offset_lanes = 5*y + x, so: x = offset_lanes mod 5, y = offset_lanes / 5
            X : constant Lane_Index := Lane_Index(Offset_Lanes mod 5);
            Y : constant Lane_Index := Lane_Index(Offset_Lanes / 5);
            Temp_Bytes : Byte_Array(1 .. 8);
            Bytes_To_Copy : Natural;
         begin
            --  Use State(Y, X) to match Ada column-major with FIPS 202 row-major
            Lane_To_Bytes (State(Y, X), Temp_Bytes);

            --  Copy as many bytes as needed
            Bytes_To_Copy := Natural'Min(8, Output'Last - Output_Idx + 1);
            Output(Output_Idx .. Output_Idx + Bytes_To_Copy - 1) :=
              Temp_Bytes(1 .. Bytes_To_Copy);
            Output_Idx := Output_Idx + Bytes_To_Copy;
         end;
      end loop;
   end Squeeze_Block;

   --  =========================================================================
   --  SHA3-512 (FIPS 202 Algorithm 8)
   --  =========================================================================

   procedure SHA3_512_Hash (
      Input  : in Byte_Array;
      Output : out SHA3_512_Digest
   ) is
      State : State_Array := (others => (others => 0));
      Rate  : constant Positive := SHA3_512_Rate_Bytes;

      --  SHA3 padding: append 0x06 || 0x00...00 || 0x80
      --  (domain separation byte 0x06 for SHA3)
      Padded_Block : Byte_Array(1 .. Rate) := (others => 0);
      Input_Idx    : Natural := Input'First;
   begin
      --  Absorb full blocks
      while Input_Idx + Rate <= Input'Last + 1 loop
         pragma Loop_Invariant (Input_Idx >= Input'First);
         pragma Loop_Invariant (Input_Idx <= Input'Last + 1);

         Absorb_Block (State, Input(Input_Idx .. Input_Idx + Rate - 1), Rate);
         Input_Idx := Input_Idx + Rate;
      end loop;

      --  Absorb final partial block with padding
      declare
         Remaining : constant Natural := Input'Last - Input_Idx + 1;
      begin
         if Remaining > 0 then
            Padded_Block(1 .. Remaining) := Input(Input_Idx .. Input'Last);
         end if;

         --  Apply SHA3 padding
         Padded_Block(Remaining + 1) := 16#06#;  -- SHA3 domain separation
         Padded_Block(Rate) := Padded_Block(Rate) or 16#80#;  -- Padding bit

         Absorb_Block (State, Padded_Block, Rate);
      end;

      --  Squeeze output
      Squeeze_Block (State, Output, Rate);

      --  Zeroize state
      for X in Lane_Index loop
         for Y in Lane_Index loop
            State(Y, X) := 0;
         end loop;
      end loop;
      Zeroize.Wipe (Padded_Block);
   end SHA3_512_Hash;

   --  =========================================================================
   --  SHA3-256 (FIPS 202 Algorithm 8)
   --  =========================================================================

   procedure SHA3_256_Hash (
      Input  : in Byte_Array;
      Output : out SHA3_256_Digest
   ) is
      State : State_Array := (others => (others => 0));
      Rate  : constant Positive := SHA3_256_Rate_Bytes;

      Padded_Block : Byte_Array(1 .. Rate) := (others => 0);
      Input_Idx    : Natural := Input'First;
   begin
      --  Absorb full blocks
      while Input_Idx + Rate <= Input'Last + 1 loop
         pragma Loop_Invariant (Input_Idx >= Input'First);
         pragma Loop_Invariant (Input_Idx <= Input'Last + 1);

         Absorb_Block (State, Input(Input_Idx .. Input_Idx + Rate - 1), Rate);
         Input_Idx := Input_Idx + Rate;
      end loop;

      --  Absorb final partial block with padding
      declare
         Remaining : constant Natural := Input'Last - Input_Idx + 1;
      begin
         if Remaining > 0 then
            Padded_Block(1 .. Remaining) := Input(Input_Idx .. Input'Last);
         end if;

         --  Apply SHA3 padding
         Padded_Block(Remaining + 1) := 16#06#;  -- SHA3 domain separation
         Padded_Block(Rate) := Padded_Block(Rate) or 16#80#;  -- Padding bit

         Absorb_Block (State, Padded_Block, Rate);
      end;

      --  Squeeze output
      Squeeze_Block (State, Output, Rate);

      --  Zeroize state
      for X in Lane_Index loop
         for Y in Lane_Index loop
            State(Y, X) := 0;
         end loop;
      end loop;
      Zeroize.Wipe (Padded_Block);
   end SHA3_256_Hash;

   --  =========================================================================
   --  SHAKE-256 (FIPS 202 Algorithm 9)
   --  =========================================================================

   procedure SHAKE_256 (
      Input  : in Byte_Array;
      Output : out Byte_Array
   ) is
      State : State_Array := (others => (others => 0));
      Rate  : constant Positive := SHAKE_256_Rate_Bytes;

      --  SHAKE padding: append 0x1F || 0x00...00 || 0x80
      --  (domain separation byte 0x1F for SHAKE)
      Padded_Block : Byte_Array(1 .. Rate) := (others => 0);
      Input_Idx    : Natural := Input'First;
      Output_Idx   : Natural := Output'First;
   begin
      --  Absorb phase: Process full input blocks
      while Input_Idx + Rate <= Input'Last + 1 loop
         pragma Loop_Invariant (Input_Idx >= Input'First);
         pragma Loop_Invariant (Input_Idx <= Input'Last + 1);

         Absorb_Block (State, Input(Input_Idx .. Input_Idx + Rate - 1), Rate);
         Input_Idx := Input_Idx + Rate;
      end loop;

      --  Absorb final partial block with padding
      declare
         Remaining : constant Natural := Input'Last - Input_Idx + 1;
      begin
         if Remaining > 0 then
            Padded_Block(1 .. Remaining) := Input(Input_Idx .. Input'Last);
         end if;

         --  Apply SHAKE padding
         Padded_Block(Remaining + 1) := 16#1F#;  -- SHAKE domain separation
         Padded_Block(Rate) := Padded_Block(Rate) or 16#80#;  -- Padding bit

         Absorb_Block (State, Padded_Block, Rate);
      end;

      --  Squeeze phase: Extract output blocks
      while Output_Idx <= Output'Last loop
         pragma Loop_Invariant (Output_Idx >= Output'First);
         pragma Loop_Invariant (Output_Idx <= Output'Last + 1);

         declare
            Bytes_Remaining : constant Natural := Output'Last - Output_Idx + 1;
            Bytes_To_Squeeze : constant Positive :=
              Positive'Min(Rate, Bytes_Remaining);
         begin
            Squeeze_Block (State, Output(Output_Idx .. Output_Idx + Bytes_To_Squeeze - 1), Rate);
            Output_Idx := Output_Idx + Bytes_To_Squeeze;

            --  If more output needed, permute again
            if Output_Idx <= Output'Last then
               Permute (State);
            end if;
         end;
      end loop;

      --  Zeroize state
      for X in Lane_Index loop
         for Y in Lane_Index loop
            State(Y, X) := 0;
         end loop;
      end loop;
      Zeroize.Wipe (Padded_Block);
   end SHAKE_256;

   --  =========================================================================
   --  SHAKE-128 (FIPS 202 Algorithm 9)
   --  =========================================================================

   procedure SHAKE_128 (
      Input  : in Byte_Array;
      Output : out Byte_Array
   ) is
      State : State_Array := (others => (others => 0));
      Rate  : constant Positive := SHAKE_128_Rate_Bytes;

      Padded_Block : Byte_Array(1 .. Rate) := (others => 0);
      Input_Idx    : Natural := Input'First;
      Output_Idx   : Natural := Output'First;
   begin
      --  Absorb phase: Process full input blocks
      while Input_Idx + Rate <= Input'Last + 1 loop
         pragma Loop_Invariant (Input_Idx >= Input'First);
         pragma Loop_Invariant (Input_Idx <= Input'Last + 1);

         Absorb_Block (State, Input(Input_Idx .. Input_Idx + Rate - 1), Rate);
         Input_Idx := Input_Idx + Rate;
      end loop;

      --  Absorb final partial block with padding
      declare
         Remaining : constant Natural := Input'Last - Input_Idx + 1;
      begin
         if Remaining > 0 then
            Padded_Block(1 .. Remaining) := Input(Input_Idx .. Input'Last);
         end if;

         --  Apply SHAKE padding
         Padded_Block(Remaining + 1) := 16#1F#;  -- SHAKE domain separation
         Padded_Block(Rate) := Padded_Block(Rate) or 16#80#;  -- Padding bit

         Absorb_Block (State, Padded_Block, Rate);
      end;

      --  Squeeze phase: Extract output blocks
      while Output_Idx <= Output'Last loop
         pragma Loop_Invariant (Output_Idx >= Output'First);
         pragma Loop_Invariant (Output_Idx <= Output'Last + 1);

         declare
            Bytes_Remaining : constant Natural := Output'Last - Output_Idx + 1;
            Bytes_To_Squeeze : constant Positive :=
              Positive'Min(Rate, Bytes_Remaining);
         begin
            Squeeze_Block (State, Output(Output_Idx .. Output_Idx + Bytes_To_Squeeze - 1), Rate);
            Output_Idx := Output_Idx + Bytes_To_Squeeze;

            --  If more output needed, permute again
            if Output_Idx <= Output'Last then
               Permute (State);
            end if;
         end;
      end loop;

      --  Zeroize state
      for X in Lane_Index loop
         for Y in Lane_Index loop
            State(Y, X) := 0;
         end loop;
      end loop;
      Zeroize.Wipe (Padded_Block);
   end SHAKE_128;

end SparkPass.Crypto.Keccak;
