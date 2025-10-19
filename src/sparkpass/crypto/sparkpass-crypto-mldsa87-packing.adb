--  ============================================================================
--  ML-DSA-87 Packing/Unpacking Implementation
--  ============================================================================

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with SparkPass.Crypto.MLDSA87.ZQ_Ops;

package body SparkPass.Crypto.MLDSA87.Packing is

   --  =========================================================================
   --  Helper: Bit Packing
   --  =========================================================================

   procedure Write_Bits (
      Output : in out Byte_Array;
      Offset : in out Natural;
      Value  : in Unsigned_32;
      Bits   : in Natural
   ) is
      Byte_Offset : Natural := Output'First + Offset / 8;
      Bit_Offset  : Natural := Offset mod 8;
      V : Unsigned_32 := Value;
      Bits_Written : Natural := 0;
   begin
      while Bits_Written < Bits loop
         declare
            Bits_In_Byte : Natural := Natural'Min (8 - Bit_Offset, Bits - Bits_Written);
            Mask : Unsigned_8 := Unsigned_8 (Shift_Left (Unsigned_8'(1), Bits_In_Byte) - 1);
            Chunk : Unsigned_8 := Unsigned_8 (V and Unsigned_32 (Mask));
         begin
            Output (Byte_Offset) := Output (Byte_Offset) or Shift_Left (Chunk, Bit_Offset);
            V := Shift_Right (V, Bits_In_Byte);
            Bits_Written := Bits_Written + Bits_In_Byte;
            Bit_Offset := (Bit_Offset + Bits_In_Byte) mod 8;
            if Bit_Offset = 0 then
               Byte_Offset := Byte_Offset + 1;
            end if;
         end;
      end loop;
      Offset := Offset + Bits;
   end Write_Bits;

   procedure Read_Bits (
      Input  : in Byte_Array;
      Offset : in out Natural;
      Bits   : in Natural;
      Result : out Unsigned_32
   ) is
      Byte_Offset : Natural := Input'First + Offset / 8;
      Bit_Offset  : Natural := Offset mod 8;
      Value : Unsigned_32 := 0;
      Bits_Read : Natural := 0;
   begin
      while Bits_Read < Bits loop
         declare
            Bits_In_Byte : Natural := Natural'Min (8 - Bit_Offset, Bits - Bits_Read);
            Mask : Unsigned_8 := Unsigned_8 (Shift_Left (Unsigned_8'(1), Bits_In_Byte) - 1);
            Chunk : Unsigned_8 := Shift_Right (Input (Byte_Offset), Bit_Offset) and Mask;
         begin
            Value := Value or Shift_Left (Unsigned_32 (Chunk), Bits_Read);
            Bits_Read := Bits_Read + Bits_In_Byte;
            Bit_Offset := (Bit_Offset + Bits_In_Byte) mod 8;
            if Bit_Offset = 0 then
               Byte_Offset := Byte_Offset + 1;
            end if;
         end;
      end loop;
      Offset := Offset + Bits;
      Result := Value;
   end Read_Bits;

   --  =========================================================================
   --  t1 Packing (10 bits per coefficient)
   --  =========================================================================

   procedure Pack_t1 (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) is
      Offset : Natural := 0;
   begin
      --  Zero output
      for I in Packed'Range loop
         Packed (I) := 0;
      end loop;

      --  Pack each coefficient (10 bits)
      for I in Poly_Index loop
         Write_Bits (Packed, Offset, Unsigned_32 (P (I)), 10);
      end loop;
   end Pack_t1;

   procedure Unpack_t1 (
      Packed : in  Byte_Array;
      P      : out Polynomial
   ) is
      Offset : Natural := 0;
      Val : Unsigned_32;
   begin
      for I in Poly_Index loop
         Read_Bits (Packed, Offset, 10, Val);
         P (I) := Zq (Val);
      end loop;
   end Unpack_t1;

   procedure Pack_t1_Vec_K (
      V      : in  PolyVec_K;
      Packed : out Byte_Array
   ) is
      Offset : Natural := Packed'First;  -- Use actual first index, not hardcoded 1
   begin
      for I in Vec_K_Index loop
         Pack_t1 (V (I), Packed (Offset .. Offset + Poly_T1_Bytes - 1));
         Offset := Offset + Poly_T1_Bytes;
      end loop;
   end Pack_t1_Vec_K;

   procedure Unpack_t1_Vec_K (
      Packed : in  Byte_Array;
      V      : out PolyVec_K
   ) is
      Offset : Natural := Packed'First;
   begin
      for I in Vec_K_Index loop
         Unpack_t1 (Packed (Offset .. Offset + Poly_T1_Bytes - 1), V (I));
         Offset := Offset + Poly_T1_Bytes;
      end loop;
   end Unpack_t1_Vec_K;

   --  =========================================================================
   --  t0 Packing (13 bits per coefficient)
   --  =========================================================================

   procedure Pack_t0 (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) is
      Offset : Natural := 0;
   begin
      for I in Packed'Range loop
         Packed (I) := 0;
      end loop;

      for I in Poly_Index loop
         --  Convert to centered, then add 2^12 offset
         declare
            C : Integer_32 := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Centered (P (I));
            Val : Unsigned_32 := Unsigned_32 (2 ** 12 + C);
         begin
            Write_Bits (Packed, Offset, Val, 13);
         end;
      end loop;
   end Pack_t0;

   procedure Unpack_t0 (
      Packed : in  Byte_Array;
      P      : out Polynomial
   ) is
      Offset : Natural := 0;
   begin
      for I in Poly_Index loop
         declare
            Val : Unsigned_32;
            C : Integer_32;
         begin
            Read_Bits (Packed, Offset, 13, Val);
            C := Integer_32 (Val) - 2 ** 12;
            P (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (C);
         end;
      end loop;
   end Unpack_t0;

   --  =========================================================================
   --  s Packing (3 bits per coefficient, for η=2)
   --  =========================================================================

   procedure Pack_s (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) is
      Offset : Natural := 0;
   begin
      for I in Packed'Range loop
         Packed (I) := 0;
      end loop;

      for I in Poly_Index loop
         --  Map [-η, η] to [0, 2η]
         declare
            C : Integer_32 := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Centered (P (I));
            Val : Unsigned_32 := Unsigned_32 (Eta + C);
         begin
            Write_Bits (Packed, Offset, Val, 3);
         end;
      end loop;
   end Pack_s;

   procedure Unpack_s (
      Packed : in  Byte_Array;
      P      : out Polynomial
   ) is
      Offset : Natural := 0;
   begin
      for I in Poly_Index loop
         declare
            Val : Unsigned_32;
            C : Integer_32;
         begin
            Read_Bits (Packed, Offset, 3, Val);
            C := Integer_32 (Val) - Eta;
            P (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (C);
         end;
      end loop;
   end Unpack_s;

   procedure Pack_s_Vec_L (
      V      : in  PolyVec_L;
      Packed : out Byte_Array
   ) is
      Offset : Natural := Packed'First;
   begin
      for I in Vec_L_Index loop
         Pack_s (V (I), Packed (Offset .. Offset + Poly_S_Bytes - 1));
         Offset := Offset + Poly_S_Bytes;
      end loop;
   end Pack_s_Vec_L;

   procedure Pack_s_Vec_K (
      V      : in  PolyVec_K;
      Packed : out Byte_Array
   ) is
      Offset : Natural := Packed'First;
   begin
      for I in Vec_K_Index loop
         Pack_s (V (I), Packed (Offset .. Offset + Poly_S_Bytes - 1));
         Offset := Offset + Poly_S_Bytes;
      end loop;
   end Pack_s_Vec_K;

   --  =========================================================================
   --  z Packing (20 bits per coefficient)
   --  =========================================================================

   procedure Pack_z (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) is
      Offset : Natural := 0;
   begin
      for I in Packed'Range loop
         Packed (I) := 0;
      end loop;

      for I in Poly_Index loop
         --  Center at γ1
         declare
            C : Integer_32 := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Centered (P (I));
            Val : Unsigned_32 := Unsigned_32 (Gamma1 + C);
         begin
            Write_Bits (Packed, Offset, Val, 20);
         end;
      end loop;
   end Pack_z;

   procedure Unpack_z (
      Packed : in  Byte_Array;
      P      : out Polynomial
   ) is
      Offset : Natural := 0;
   begin
      for I in Poly_Index loop
         declare
            Val : Unsigned_32;
            C : Integer_32;
         begin
            Read_Bits (Packed, Offset, 20, Val);
            C := Integer_32 (Val) - Gamma1;
            P (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (C);
         end;
      end loop;
   end Unpack_z;

   procedure Pack_z_Vec_L (
      V      : in  PolyVec_L;
      Packed : out Byte_Array
   ) is
      Offset : Natural := Packed'First;
   begin
      for I in Vec_L_Index loop
         Pack_z (V (I), Packed (Offset .. Offset + Poly_Z_Bytes - 1));
         Offset := Offset + Poly_Z_Bytes;
      end loop;
   end Pack_z_Vec_L;

   procedure Unpack_z_Vec_L (
      Packed : in  Byte_Array;
      V      : out PolyVec_L
   ) is
      Offset : Natural := Packed'First;
   begin
      for I in Vec_L_Index loop
         Unpack_z (Packed (Offset .. Offset + Poly_Z_Bytes - 1), V (I));
         Offset := Offset + Poly_Z_Bytes;
      end loop;
   end Unpack_z_Vec_L;

   --  =========================================================================
   --  w1 Packing (4 bits per coefficient)
   --  =========================================================================

   procedure Pack_w1 (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) is
      Offset : Natural := 0;
   begin
      for I in Packed'Range loop
         Packed (I) := 0;
      end loop;

      for I in Poly_Index loop
         Write_Bits (Packed, Offset, Unsigned_32 (P (I)), 4);
      end loop;
   end Pack_w1;

   --  =========================================================================
   --  Hint Packing (FIPS 204 Algorithm 40/41)
   --  =========================================================================

   procedure Pack_Hint (
      H      : in  PolyVec_K;
      Packed : out Byte_Array;
      Success : out Boolean
   ) is
      Pos : Natural := Packed'First;  -- FIX: Use array's actual start index
      Count : Natural := 0;
   begin
      --  Zero output
      for I in Packed'Range loop
         Packed (I) := 0;
      end loop;

      --  Encode positions of 1-bits
      for I in Vec_K_Index loop
         for J in Poly_Index loop
            if H (I)(J) /= 0 then
               --  Check bounds relative to array start
               if Count >= Omega then
                  Success := False;
                  return;
               end if;
               Packed (Pos) := Unsigned_8 (J);
               Pos := Pos + 1;
               Count := Count + 1;
            end if;
         end loop;
         --  Mark end of this polynomial's hints
         --  FIX: Compute offset from array start, not from 1
         Packed (Packed'First + Omega + Natural (I)) := Unsigned_8 (Count);
      end loop;

      Success := (Count <= Omega);
   end Pack_Hint;

   procedure Unpack_Hint (
      Packed  : in  Byte_Array;
      H       : out PolyVec_K;
      Success : out Boolean
   ) is
      Pos : Natural := Packed'First;  -- FIX: Use array's actual start index
      Prev : Natural := 0;
      Curr : Natural;
   begin
      --  Zero output
      for I in Vec_K_Index loop
         for J in Poly_Index loop
            H (I)(J) := 0;
         end loop;
      end loop;

      --  Decode positions
      for I in Vec_K_Index loop
         --  FIX: Compute offset from array start, not from 1
         Curr := Natural (Packed (Packed'First + Omega + Natural (I)));

         if Curr < Prev or Curr > Omega then
            Success := False;
            return;
         end if;

         while Pos < (Packed'First + Curr) loop  -- FIX: Compare with absolute position
            if (Pos - Packed'First) >= Omega then  -- FIX: Check relative offset
               Success := False;
               return;
            end if;

            declare
               Idx : Natural := Natural (Packed (Pos));
            begin
               if Idx >= N then
                  Success := False;
                  return;
               end if;
               H (I)(Idx) := 1;
            end;
            Pos := Pos + 1;
         end loop;

         Prev := Curr;
      end loop;

      Success := True;
   end Unpack_Hint;

end SparkPass.Crypto.MLDSA87.Packing;
