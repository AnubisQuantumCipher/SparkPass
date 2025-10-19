--  ============================================================================
--  ML-DSA-87 Polynomial Vector Operations Implementation
--  ============================================================================

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body SparkPass.Crypto.MLDSA87.PolyVec is

   --  =========================================================================
   --  NTT Operations on Vectors
   --  =========================================================================

   procedure NTT_Vec_K (V : in out PolyVec_K) is
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         NTT (V (I));
      end loop;
   end NTT_Vec_K;

   procedure NTT_Inv_Vec_K (V : in out PolyVec_K) is
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         NTT_Inv (V (I));
      end loop;
   end NTT_Inv_Vec_K;

   procedure NTT_Vec_L (V : in out PolyVec_L) is
   begin
      for I in Vec_L_Index loop
         pragma Loop_Invariant (I in Vec_L_Index);
         NTT (V (I));
      end loop;
   end NTT_Vec_L;

   procedure NTT_Inv_Vec_L (V : in out PolyVec_L) is
   begin
      for I in Vec_L_Index loop
         pragma Loop_Invariant (I in Vec_L_Index);
         NTT_Inv (V (I));
      end loop;
   end NTT_Inv_Vec_L;

   --  =========================================================================
   --  Vector Arithmetic
   --  =========================================================================

   procedure Add_Vec_K (
      Result : out PolyVec_K;
      A      : in  PolyVec_K;
      B      : in  PolyVec_K
   ) is
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         Add (Result (I), A (I), B (I));
      end loop;
   end Add_Vec_K;

   procedure Sub_Vec_K (
      Result : out PolyVec_K;
      A      : in  PolyVec_K;
      B      : in  PolyVec_K
   ) is
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         Sub (Result (I), A (I), B (I));
      end loop;
   end Sub_Vec_K;

   procedure Add_Vec_L (
      Result : out PolyVec_L;
      A      : in  PolyVec_L;
      B      : in  PolyVec_L
   ) is
   begin
      for I in Vec_L_Index loop
         pragma Loop_Invariant (I in Vec_L_Index);
         Add (Result (I), A (I), B (I));
      end loop;
   end Add_Vec_L;

   procedure Sub_Vec_L (
      Result : out PolyVec_L;
      A      : in  PolyVec_L;
      B      : in  PolyVec_L
   ) is
   begin
      for I in Vec_L_Index loop
         pragma Loop_Invariant (I in Vec_L_Index);
         Sub (Result (I), A (I), B (I));
      end loop;
   end Sub_Vec_L;

   --  =========================================================================
   --  Rounding Operations on Vectors
   --  =========================================================================

   procedure Power2Round_Vec_K (
      V  : in  PolyVec_K;
      V1 : out PolyVec_K;
      V0 : out PolyVec_K
   ) is
      R1, R0 : Zq;
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         for J in Poly_Index loop
            pragma Loop_Invariant (J in Poly_Index);
            Power2Round (V (I)(J), R1, R0);
            V1 (I)(J) := R1;
            V0 (I)(J) := R0;
         end loop;
      end loop;
   end Power2Round_Vec_K;

   procedure Decompose_Vec_K (
      V  : in  PolyVec_K;
      V1 : out PolyVec_K;
      V0 : out PolyVec_K
   ) is
      R1, R0 : Zq;
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         for J in Poly_Index loop
            pragma Loop_Invariant (J in Poly_Index);
            Decompose (V (I)(J), R1, R0);
            V1 (I)(J) := R1;
            V0 (I)(J) := R0;
         end loop;
      end loop;
   end Decompose_Vec_K;

   procedure HighBits_Vec_K (
      V  : in  PolyVec_K;
      V1 : out PolyVec_K
   ) is
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         for J in Poly_Index loop
            pragma Loop_Invariant (J in Poly_Index);
            V1 (I)(J) := HighBits (V (I)(J));
         end loop;
      end loop;
   end HighBits_Vec_K;

   procedure LowBits_Vec_K (
      V  : in  PolyVec_K;
      V0 : out PolyVec_K
   ) is
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         for J in Poly_Index loop
            pragma Loop_Invariant (J in Poly_Index);
            V0 (I)(J) := LowBits (V (I)(J));
         end loop;
      end loop;
   end LowBits_Vec_K;

   --  =========================================================================
   --  Norm Operations
   --  =========================================================================

   function Infinity_Norm_Vec_L (V : PolyVec_L) return Natural is
      Max : Natural := 0;
      Poly_Norm : Natural;
   begin
      for I in Vec_L_Index loop
         pragma Loop_Invariant (I in Vec_L_Index);
         Poly_Norm := Infinity_Norm (V (I));
         if Poly_Norm > Max then
            Max := Poly_Norm;
         end if;
      end loop;
      return Max;
   end Infinity_Norm_Vec_L;

   function Check_Norm_Bound_Vec_L (
      V     : PolyVec_L;
      Bound : Natural
   ) return Boolean is
   begin
      return Infinity_Norm_Vec_L (V) < Bound;
   end Check_Norm_Bound_Vec_L;

   function Infinity_Norm_Vec_K (V : PolyVec_K) return Natural is
      Max : Natural := 0;
      Poly_Norm : Natural;
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         Poly_Norm := Infinity_Norm (V (I));
         if Poly_Norm > Max then
            Max := Poly_Norm;
         end if;
      end loop;
      return Max;
   end Infinity_Norm_Vec_K;

   --  =========================================================================
   --  Hint Operations
   --  =========================================================================

   procedure MakeHint_Vec_K (
      H : out PolyVec_K;
      Z : in  PolyVec_K;
      R : in  PolyVec_K;
      Ones_Count : out Natural
   ) is
      Hint : Boolean;
   begin
      Ones_Count := 0;

      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         pragma Loop_Invariant (Ones_Count <= Natural (I) * N);

         for J in Poly_Index loop
            pragma Loop_Invariant (J in Poly_Index);
            pragma Loop_Invariant (Ones_Count <= Natural (I) * N + Natural (J));

            Hint := MakeHint (Z (I)(J), R (I)(J));

            if Hint then
               H (I)(J) := 1;
               Ones_Count := Ones_Count + 1;
            else
               H (I)(J) := 0;
            end if;
         end loop;
      end loop;
   end MakeHint_Vec_K;

   procedure UseHint_Vec_K (
      Result : out PolyVec_K;
      H      : in  PolyVec_K;
      R      : in  PolyVec_K
   ) is
      Hint : Boolean;
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         for J in Poly_Index loop
            pragma Loop_Invariant (J in Poly_Index);

            Hint := (H (I)(J) /= 0);
            Result (I)(J) := UseHint (Hint, R (I)(J));
         end loop;
      end loop;
   end UseHint_Vec_K;

   --  =========================================================================
   --  Utility Operations
   --  =========================================================================

   procedure Zeroize_Vec_K (V : in out PolyVec_K) is
   begin
      for I in Vec_K_Index loop
         pragma Loop_Invariant (I in Vec_K_Index);
         Zeroize (V (I));
      end loop;
   end Zeroize_Vec_K;

   procedure Zeroize_Vec_L (V : in out PolyVec_L) is
   begin
      for I in Vec_L_Index loop
         pragma Loop_Invariant (I in Vec_L_Index);
         Zeroize (V (I));
      end loop;
   end Zeroize_Vec_L;

end SparkPass.Crypto.MLDSA87.PolyVec;
