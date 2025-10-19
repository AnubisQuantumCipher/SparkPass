--  ============================================================================
--  ML-DSA-87 Polynomial Arithmetic Implementation
--  ============================================================================
--
--  **Migrated to Poly_Zq**: All operations use non-centered Zq [0, Q-1]
--  Conversions to Coeff_Centered only at edges (norms, rounding)
--
--  ============================================================================

pragma SPARK_Mode (On);

with SparkPass.Crypto.MLDSA87.ZQ_Ops;
with Interfaces; use Interfaces;

package body SparkPass.Crypto.MLDSA87.Poly is

   --  =========================================================================
   --  Precomputed NTT Constants
   --  =========================================================================

   --  Zetas: Powers of primitive 512-th root of unity (ζ = 1753)
   --  Precomputed in Montgomery form for efficient NTT
   --  Generated from ζ^bitrev(k) mod q in Montgomery form
   --  Index 0 is placeholder; actual zetas at indices 1-255 (standard FIPS 204)
   --  Forward NTT uses k=1..255 with pre-increment
   --  Inverse NTT uses k=255..1 with pre-decrement
   type Zeta_Array is array (Natural range 0 .. 254) of Zq;

   Zetas : constant Zeta_Array := (
        0 =>    25847,   1 =>  5771523,   2 =>  7861508,   3 =>   237124,
        4 =>  7602457,   5 =>  7504169,   6 =>   466468,   7 =>  1826347,
        8 =>  2353451,   9 =>  8021166,  10 =>  6288512,  11 =>  3119733,
       12 =>  5495562,  13 =>  3111497,  14 =>  2680103,  15 =>  2725464,
       16 =>  1024112,  17 =>  7300517,  18 =>  3585928,  19 =>  7830929,
       20 =>  7260833,  21 =>  2619752,  22 =>  6271868,  23 =>  6262231,
       24 =>  4520680,  25 =>  6980856,  26 =>  5102745,  27 =>  1757237,
       28 =>  8360995,  29 =>  4010497,  30 =>   280005,  31 =>  2706023,
       32 =>    95776,  33 =>  3077325,  34 =>  3530437,  35 =>  6718724,
       36 =>  4788269,  37 =>  5842901,  38 =>  3915439,  39 =>  4519302,
       40 =>  5336701,  41 =>  3574422,  42 =>  5512770,  43 =>  3539968,
       44 =>  8079950,  45 =>  2348700,  46 =>  7841118,  47 =>  6681150,
       48 =>  6736599,  49 =>  3505694,  50 =>  4558682,  51 =>  3507263,
       52 =>  6239768,  53 =>  6779997,  54 =>  3699596,  55 =>   811944,
       56 =>   531354,  57 =>   954230,  58 =>  3881043,  59 =>  3900724,
       60 =>  5823537,  61 =>  2071892,  62 =>  5582638,  63 =>  4450022,
       64 =>  6851714,  65 =>  4702672,  66 =>  5339162,  67 =>  6927966,
       68 =>  3475950,  69 =>  2176455,  70 =>  6795196,  71 =>  7122806,
       72 =>  1939314,  73 =>  4296819,  74 =>  7380215,  75 =>  5190273,
       76 =>  5223087,  77 =>  4747489,  78 =>   126922,  79 =>  3412210,
       80 =>  7396998,  81 =>  2147896,  82 =>  2715295,  83 =>  5412772,
       84 =>  4686924,  85 =>  7969390,  86 =>  5903370,  87 =>  7709315,
       88 =>  7151892,  89 =>  8357436,  90 =>  7072248,  91 =>  7998430,
       92 =>  1349076,  93 =>  1852771,  94 =>  6949987,  95 =>  5037034,
       96 =>   264944,  97 =>   508951,  98 =>  3097992,  99 =>    44288,
      100 =>  7280319, 101 =>   904516, 102 =>  3958618, 103 =>  4656075,
      104 =>  8371839, 105 =>  1653064, 106 =>  5130689, 107 =>  2389356,
      108 =>  8169440, 109 =>   759969, 110 =>  7063561, 111 =>   189548,
      112 =>  4827145, 113 =>  3159746, 114 =>  6529015, 115 =>  5971092,
      116 =>  8202977, 117 =>  1315589, 118 =>  1341330, 119 =>  1285669,
      120 =>  6795489, 121 =>  7567685, 122 =>  6940675, 123 =>  5361315,
      124 =>  4499357, 125 =>  4751448, 126 =>  3839961, 127 =>  2091667,
      128 =>  3407706, 129 =>  2316500, 130 =>  3817976, 131 =>  5037939,
      132 =>  2244091, 133 =>  5933984, 134 =>  4817955, 135 =>   266997,
      136 =>  2434439, 137 =>  7144689, 138 =>  3513181, 139 =>  4860065,
      140 =>  4621053, 141 =>  7183191, 142 =>  5187039, 143 =>   900702,
      144 =>  1859098, 145 =>   909542, 146 =>   819034, 147 =>   495491,
      148 =>  6767243, 149 =>  8337157, 150 =>  7857917, 151 =>  7725090,
      152 =>  5257975, 153 =>  2031748, 154 =>  3207046, 155 =>  4823422,
      156 =>  7855319, 157 =>  7611795, 158 =>  4784579, 159 =>   342297,
      160 =>   286988, 161 =>  5942594, 162 =>  4108315, 163 =>  3437287,
      164 =>  5038140, 165 =>  1735879, 166 =>   203044, 167 =>  2842341,
      168 =>  2691481, 169 =>  5790267, 170 =>  1265009, 171 =>  4055324,
      172 =>  1247620, 173 =>  2486353, 174 =>  1595974, 175 =>  4613401,
      176 =>  1250494, 177 =>  2635921, 178 =>  4832145, 179 =>  5386378,
      180 =>  1869119, 181 =>  1903435, 182 =>  7329447, 183 =>  7047359,
      184 =>  1237275, 185 =>  5062207, 186 =>  6950192, 187 =>  7929317,
      188 =>  1312455, 189 =>  3306115, 190 =>  6417775, 191 =>  7100756,
      192 =>  1917081, 193 =>  5834105, 194 =>  7005614, 195 =>  1500165,
      196 =>   777191, 197 =>  2235880, 198 =>  3406031, 199 =>  7838005,
      200 =>  5548557, 201 =>  6709241, 202 =>  6533464, 203 =>  5796124,
      204 =>  4656147, 205 =>   594136, 206 =>  4603424, 207 =>  6366809,
      208 =>  2432395, 209 =>  2454455, 210 =>  8215696, 211 =>  1957272,
      212 =>  3369112, 213 =>   185531, 214 =>  7173032, 215 =>  5196991,
      216 =>   162844, 217 =>  1616392, 218 =>  3014001, 219 =>   810149,
      220 =>  1652634, 221 =>  4686184, 222 =>  6581310, 223 =>  5341501,
      224 =>  3523897, 225 =>  3866901, 226 =>   269760, 227 =>  2213111,
      228 =>  7404533, 229 =>  1717735, 230 =>   472078, 231 =>  7953734,
      232 =>  1723600, 233 =>  6577327, 234 =>  1910376, 235 =>  6712985,
      236 =>  7276084, 237 =>  8119771, 238 =>  4546524, 239 =>  5441381,
      240 =>  6144432, 241 =>  7959518, 242 =>  6094090, 243 =>   183443,
      244 =>  7403526, 245 =>  1612842, 246 =>  4834730, 247 =>  7826001,
      248 =>  3919660, 249 =>  8332111, 250 =>  7018208, 251 =>  3937738,
      252 =>  1400424, 253 =>  7534263, 254 =>  1976782
   );
   pragma Assert (Zetas'Length = 255);

   --  =========================================================================
   --  NTT Operations (FIPS 204 Algorithm 42/43)
   --  =========================================================================

   procedure NTT (P : in out Polynomial) is
      --  Cooley-Tukey NTT in Montgomery domain
      Len : Natural := 128;
      K   : Natural;
      J   : Natural;
      T   : Zq;
      Zeta : Zq;
   begin
      K := 0;
      while Len >= 1 loop
         J := 0;
         while J < N loop
            Zeta := Zetas (K);             -- Post-increment: read then increment
            K := K + 1;

            for I in J .. J + Len - 1 loop
               pragma Loop_Invariant (I >= J and I < J + Len);
               pragma Loop_Invariant (K >= 1 and K <= 255);

               --  DIF butterfly: save a0, compute t, then update both
               declare
                  A0 : constant Zq := P (I);
               begin
                  T := SparkPass.Crypto.MLDSA87.ZQ_Ops.MontMul (Zeta, P (I + Len));
                  P (I)       := SparkPass.Crypto.MLDSA87.ZQ_Ops.AddQ (A0, T);
                  P (I + Len) := SparkPass.Crypto.MLDSA87.ZQ_Ops.SubQ (A0, T);
               end;
            end loop;

            J := J + 2 * Len;
         end loop;

         Len := Len / 2;
      end loop;
      pragma Assert (K = 255);  -- Verify all 255 zetas consumed
   end NTT;

   procedure NTT_Inv (P : in out Polynomial) is
      --  Gentleman-Sande inverse NTT (DIT) - FIPS 204 Algorithm 42
      --  Scale by n^{-1} mod q (for n=256, q=8380417)
      --  n^{-1} = 8347681 (plain), converted to Montgomery form for MontMul
      F : constant Zq := 16382;  -- (8347681 * R) mod q where R = 2^32 mod q

      Len : Natural := 1;
      K   : Integer := 254;  -- Reverse: start at last index (254 down to 0)
      J   : Natural;
      A0, A1 : Zq;
      Zeta : Zq;
   begin
      while Len < N loop
         J := 0;
         while J < N loop
            --  Use forward zetas in reverse order (post-decrement)
            --  FIPS 204 Algorithm 42: inverse uses NEGATED zetas
            Zeta := Zetas (K);
            K := K - 1;
            Zeta := SparkPass.Crypto.MLDSA87.ZQ_Ops.SubQ (0, Zeta);  -- Negate: -Zeta mod q

            for I in J .. J + Len - 1 loop
               pragma Loop_Invariant (I >= J and I < J + Len);
               pragma Loop_Invariant (K >= -1 and K <= 253);

               --  DIT butterfly: save inputs, compute difference, then update
               declare
                  A0 : constant Zq := P (I);
                  A1 : constant Zq := P (I + Len);
                  T  : constant Zq := SparkPass.Crypto.MLDSA87.ZQ_Ops.SubQ (A0, A1);
               begin
                  P (I)       := SparkPass.Crypto.MLDSA87.ZQ_Ops.AddQ (A0, A1);
                  P (I + Len) := SparkPass.Crypto.MLDSA87.ZQ_Ops.MontMul (T, Zeta);
               end;
            end loop;

            J := J + 2 * Len;
         end loop;

         Len := Len * 2;
      end loop;
      pragma Assert (K = -1);  -- Verify all 255 zetas consumed in reverse

      --  Final scaling by n^{-1} (Montgomery form)
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         P (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.MontMul (P (I), F);
      end loop;
   end NTT_Inv;

   --  =========================================================================
   --  Arithmetic Operations
   --  =========================================================================

   procedure Add (
      Result : out Polynomial;
      A      : in  Polynomial;
      B      : in  Polynomial
   ) is
   begin
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         Result (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.AddQ (A (I), B (I));
      end loop;
   end Add;

   procedure Sub (
      Result : out Polynomial;
      A      : in  Polynomial;
      B      : in  Polynomial
   ) is
   begin
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         Result (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.SubQ (A (I), B (I));
      end loop;
   end Sub;

   procedure Pointwise_Montgomery (
      Result : out Polynomial;
      A      : in  Polynomial;
      B      : in  Polynomial
   ) is
   begin
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         Result (I) := SparkPass.Crypto.MLDSA87.ZQ_Ops.MontMul (A (I), B (I));
      end loop;
   end Pointwise_Montgomery;

   --  =========================================================================
   --  Rounding Operations (FIPS 204 Section 3.6)
   --  =========================================================================
   --  Convert to centered for arithmetic, then back to Zq

   procedure Power2Round (
      R  : in  Zq;
      R1 : out Zq;
      R0 : out Zq
   ) is
      --  FIPS 204 Algorithm 27: Power2Round(r, d)
      --  Input: r ∈ [0, Q-1]
      --  Output: (r1, r0) such that r = r1*2^d + r0, r0 ∈ [-2^(d-1), 2^(d-1))

      R_Val  : constant Unsigned_32 := R;
      R0_Temp, R1_Temp : Integer_32;
   begin
      --  r0 = r mod 2^d (centered)
      R0_Temp := Integer_32 (R_Val mod (2 ** D));
      if R0_Temp >= 2 ** (D - 1) then
         R0_Temp := R0_Temp - 2 ** D;
      end if;

      --  r1 = (r - r0) / 2^d
      R1_Temp := (Integer_32 (R_Val) - R0_Temp) / (2 ** D);

      --  Convert back to Zq
      R0 := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (R0_Temp);
      R1 := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (R1_Temp);
   end Power2Round;

   procedure Decompose (
      R  : in  Zq;
      R1 : out Zq;
      R0 : out Zq
   ) is
      --  FIPS 204 Algorithm 30: Decompose(r, γ2)
      Alpha : constant Unsigned_32 := 2 * Gamma2;

      R_Val  : constant Unsigned_32 := R;
      R0_Temp, R1_Temp : Integer_32;
   begin
      --  r0 = r mod 2γ2 (centered)
      R0_Temp := Integer_32 (R_Val mod Alpha);
      if R0_Temp >= Integer_32 (Gamma2) then
         R0_Temp := R0_Temp - Integer_32 (Alpha);
      end if;

      --  Special case for boundary
      if Integer_32 (R_Val) - R0_Temp = Integer_32 (Q - 1) then
         R1_Temp := 0;
         R0_Temp := R0_Temp - 1;
      else
         R1_Temp := (Integer_32 (R_Val) - R0_Temp) / Integer_32 (Alpha);
      end if;

      --  Convert back to Zq
      R0 := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (R0_Temp);
      R1 := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Zq (R1_Temp);
   end Decompose;

   function HighBits (R : Zq) return Zq is
      R1, R0 : Zq;
   begin
      Decompose (R, R1, R0);
      return R1;
   end HighBits;

   function LowBits (R : Zq) return Zq is
      R1, R0 : Zq;
   begin
      Decompose (R, R1, R0);
      return R0;
   end LowBits;

   function MakeHint (Z : Zq; R : Zq) return Boolean is
      --  FIPS 204 Algorithm 33: MakeHint(z, r, γ2)
      R1 : constant Zq := HighBits (R);
      V1 : constant Zq := HighBits (SparkPass.Crypto.MLDSA87.ZQ_Ops.AddQ (R, Z));
   begin
      return R1 /= V1;
   end MakeHint;

   function UseHint (H : Boolean; R : Zq) return Zq is
      --  FIPS 204 Algorithm 34: UseHint(h, r, γ2)
      M  : constant Unsigned_32 := (Q - 1) / (2 * Gamma2);
      R1, R0 : Zq;
      R0_Centered : Coeff_Centered;
   begin
      Decompose (R, R1, R0);

      if not H then
         return R1;
      end if;

      --  Convert R0 to centered to check sign
      R0_Centered := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Centered (R0);

      --  Apply hint based on sign of R0
      if R0_Centered > 0 then
         return Zq ((R1 + 1) mod M);
      else
         return Zq ((R1 + M - 1) mod M);  -- Equivalent to (R1 - 1) mod M
      end if;
   end UseHint;

   --  =========================================================================
   --  Norm Operations
   --  =========================================================================

   function Infinity_Norm (P : Polynomial) return Natural is
      --  Compute ||p||_∞ in centered representation
      Max : Natural := 0;
      Abs_Val : Natural;
      C : Coeff_Centered;
   begin
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         pragma Loop_Invariant (Max <= (Q - 1) / 2);

         --  Convert to centered to get proper absolute value
         C := SparkPass.Crypto.MLDSA87.ZQ_Ops.To_Centered (P (I));

         if C >= 0 then
            Abs_Val := Natural (C);
         else
            Abs_Val := Natural (-C);
         end if;

         if Abs_Val > Max then
            Max := Abs_Val;
         end if;
      end loop;

      return Max;
   end Infinity_Norm;

   function Check_Norm_Bound (
      P     : Polynomial;
      Bound : Natural
   ) return Boolean is
   begin
      return Infinity_Norm (P) < Bound;
   end Check_Norm_Bound;

   --  =========================================================================
   --  Utility Operations
   --  =========================================================================

   procedure Zeroize (P : in out Polynomial) is
   begin
      for I in Poly_Index loop
         pragma Loop_Invariant (I in Poly_Index);
         pragma Loop_Invariant (for all J in 0 .. I - 1 => P (J) = 0);
         P (I) := 0;
      end loop;
   end Zeroize;

end SparkPass.Crypto.MLDSA87.Poly;
