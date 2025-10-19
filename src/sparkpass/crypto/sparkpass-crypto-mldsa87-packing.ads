--  ============================================================================
--  ML-DSA-87 Packing/Unpacking (FIPS 204)
--  ============================================================================
--
--  **Purpose**: Encode/decode polynomials and vectors to/from byte arrays
--
--  **Formats** (FIPS 204 Table 2):
--    - t1: 10 bits per coefficient (320 bytes per poly)
--    - t0: 13 bits per coefficient (416 bytes per poly)
--    - s:   3 bits per coefficient (96 bytes per poly, η=2)
--    - z:  20 bits per coefficient (640 bytes per poly)
--    - w1:  4 bits per coefficient (128 bytes per poly)
--    - h:  Hint bits encoded with positions
--
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Crypto.MLDSA87.Params;  use SparkPass.Crypto.MLDSA87.Params;
with SparkPass.Crypto.MLDSA87.Poly;    use SparkPass.Crypto.MLDSA87.Poly;
with SparkPass.Crypto.MLDSA87.PolyVec; use SparkPass.Crypto.MLDSA87.PolyVec;
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.MLDSA87.Packing is

   --  =========================================================================
   --  t1 Packing (10 bits per coefficient)
   --  =========================================================================

   procedure Pack_t1 (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) with
      Global => null,
      Pre    => Packed'Length = Poly_T1_Bytes and Packed'First = 1;

   procedure Unpack_t1 (
      Packed : in  Byte_Array;
      P      : out Polynomial
   ) with
      Global => null,
      Pre    => Packed'Length = Poly_T1_Bytes and Packed'First = 1;

   procedure Pack_t1_Vec_K (
      V      : in  PolyVec_K;
      Packed : out Byte_Array
   ) with
      Global => null,
      Pre    => Packed'Length = T1_Bytes and Packed'First = 1;

   procedure Unpack_t1_Vec_K (
      Packed : in  Byte_Array;
      V      : out PolyVec_K
   ) with
      Global => null,
      Pre    => Packed'Length = T1_Bytes and Packed'First = 1;

   --  =========================================================================
   --  t0 Packing (13 bits per coefficient)
   --  =========================================================================

   procedure Pack_t0 (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) with
      Global => null,
      Pre    => Packed'Length = Poly_T0_Bytes and Packed'First = 1;

   procedure Unpack_t0 (
      Packed : in  Byte_Array;
      P      : out Polynomial
   ) with
      Global => null,
      Pre    => Packed'Length = Poly_T0_Bytes and Packed'First = 1;

   --  =========================================================================
   --  s Packing (3 bits per coefficient, for η=2)
   --  =========================================================================

   procedure Pack_s (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) with
      Global => null,
      Pre    => Packed'Length = Poly_S_Bytes and Packed'First = 1;

   procedure Unpack_s (
      Packed : in  Byte_Array;
      P      : out Polynomial
   ) with
      Global => null,
      Pre    => Packed'Length = Poly_S_Bytes and Packed'First = 1;

   procedure Pack_s_Vec_L (
      V      : in  PolyVec_L;
      Packed : out Byte_Array
   ) with
      Global => null,
      Pre    => Packed'Length = S1_Bytes and Packed'First = 1;

   procedure Pack_s_Vec_K (
      V      : in  PolyVec_K;
      Packed : out Byte_Array
   ) with
      Global => null,
      Pre    => Packed'Length = S2_Bytes and Packed'First = 1;

   --  =========================================================================
   --  z Packing (20 bits per coefficient)
   --  =========================================================================

   procedure Pack_z (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) with
      Global => null,
      Pre    => Packed'Length = Poly_Z_Bytes and Packed'First = 1;

   procedure Unpack_z (
      Packed : in  Byte_Array;
      P      : out Polynomial
   ) with
      Global => null,
      Pre    => Packed'Length = Poly_Z_Bytes and Packed'First = 1;

   procedure Pack_z_Vec_L (
      V      : in  PolyVec_L;
      Packed : out Byte_Array
   ) with
      Global => null,
      Pre    => Packed'Length = Z_Bytes and Packed'First = 1;

   procedure Unpack_z_Vec_L (
      Packed : in  Byte_Array;
      V      : out PolyVec_L
   ) with
      Global => null,
      Pre    => Packed'Length = Z_Bytes and Packed'First = 1;

   --  =========================================================================
   --  w1 Packing (4 bits per coefficient)
   --  =========================================================================

   procedure Pack_w1 (
      P      : in  Polynomial;
      Packed : out Byte_Array
   ) with
      Global => null,
      Pre    => Packed'Length = Poly_W1_Bytes and Packed'First = 1;

   --  =========================================================================
   --  Hint Packing (FIPS 204 Algorithm 40/41)
   --  =========================================================================

   --  Encode hint polynomial h (positions of 1-bits)
   procedure Pack_Hint (
      H      : in  PolyVec_K;
      Packed : out Byte_Array;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Packed'Length = H_Bytes and Packed'First = 1;

   --  Decode hint polynomial h
   procedure Unpack_Hint (
      Packed  : in  Byte_Array;
      H       : out PolyVec_K;
      Success : out Boolean
   ) with
      Global => null,
      Pre    => Packed'Length = H_Bytes and Packed'First = 1;

end SparkPass.Crypto.MLDSA87.Packing;
