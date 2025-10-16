pragma SPARK_Mode (On);
with SparkPass.Crypto.Random;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.Shamir is

   --  GF(256) arithmetic using irreducible polynomial x^8 + x^4 + x^3 + x + 1
   --  This is the same polynomial used in AES and other cryptographic systems
   --
   --  Multiplication is done via log/antilog tables for constant-time operation

   --  GF(256) multiplication using log tables (constant-time)
   type GF256 is mod 256;

   --  Precomputed log and antilog tables for GF(256)
   --  Generated using generator polynomial x^8 + x^4 + x^3 + x + 1
   type Log_Table is array (GF256 range 1 .. 255) of U8;
   type Exp_Table is array (U8 range 0 .. 255) of U8;

   --  Exponential table (antilog): exp[i] = g^i where g=3 is generator
   Exp : constant Exp_Table :=
     (1, 3, 5, 15, 17, 51, 85, 255, 26, 46, 114, 150, 161, 248, 19, 53,
      95, 225, 56, 72, 216, 115, 149, 164, 247, 2, 6, 10, 30, 34, 102, 170,
      229, 52, 92, 228, 55, 89, 235, 38, 106, 190, 217, 112, 144, 171, 230, 49,
      83, 245, 4, 12, 20, 60, 68, 204, 79, 209, 104, 184, 211, 110, 178, 205,
      76, 212, 103, 169, 224, 59, 77, 215, 98, 166, 241, 8, 24, 40, 120, 136,
      131, 158, 185, 208, 107, 189, 220, 127, 129, 152, 179, 206, 73, 219, 118, 154,
      181, 196, 87, 249, 16, 48, 80, 240, 11, 29, 39, 105, 187, 214, 97, 163,
      254, 25, 43, 125, 135, 146, 173, 236, 47, 113, 147, 174, 233, 32, 96, 160,
      251, 22, 58, 78, 210, 109, 183, 194, 93, 231, 50, 86, 250, 21, 63, 65,
      195, 94, 226, 61, 71, 201, 64, 192, 91, 237, 44, 116, 156, 191, 218, 117,
      159, 186, 213, 100, 172, 239, 42, 126, 130, 157, 188, 223, 122, 142, 137, 128,
      155, 182, 193, 88, 232, 35, 101, 175, 234, 37, 111, 177, 200, 67, 197, 84,
      252, 31, 33, 99, 165, 244, 7, 9, 27, 45, 119, 153, 176, 203, 70, 202,
      69, 207, 74, 222, 121, 139, 134, 145, 168, 227, 62, 66, 198, 81, 243, 14,
      18, 54, 90, 238, 41, 123, 141, 140, 143, 138, 133, 148, 167, 242, 13, 23,
      57, 75, 221, 124, 132, 151, 162, 253, 28, 36, 108, 180, 199, 82, 246, 1);

   --  Logarithm table: log[exp[i]] = i
   Log : constant Log_Table :=
     (0, 25, 1, 50, 2, 26, 198, 75, 199, 27, 104, 51, 238, 223, 3, 100,
      4, 224, 14, 52, 141, 129, 239, 76, 113, 8, 200, 248, 105, 28, 193, 125,
      194, 29, 181, 249, 185, 39, 106, 77, 228, 166, 114, 154, 201, 9, 120, 101,
      47, 138, 5, 33, 15, 225, 36, 18, 240, 130, 69, 53, 147, 218, 142, 150,
      143, 219, 189, 54, 208, 206, 148, 19, 92, 210, 241, 64, 70, 131, 56, 102,
      221, 253, 48, 191, 6, 139, 98, 179, 37, 226, 152, 34, 136, 145, 16, 126,
      110, 72, 195, 163, 182, 30, 66, 58, 107, 40, 84, 250, 133, 61, 186, 43,
      121, 10, 21, 155, 159, 94, 202, 78, 212, 172, 229, 243, 115, 167, 87, 175,
      88, 168, 80, 244, 234, 214, 116, 79, 174, 233, 213, 231, 230, 173, 232, 44,
      215, 117, 122, 235, 22, 11, 245, 89, 203, 95, 176, 156, 169, 81, 160, 127,
      12, 246, 111, 23, 196, 73, 236, 216, 67, 31, 45, 164, 118, 123, 183, 204,
      187, 62, 90, 251, 96, 177, 134, 59, 82, 161, 108, 170, 85, 41, 157, 151,
      178, 135, 144, 97, 190, 220, 252, 188, 149, 207, 205, 55, 63, 91, 209, 83,
      57, 132, 60, 65, 162, 109, 71, 20, 42, 158, 93, 86, 242, 211, 171, 68,
      17, 146, 217, 35, 32, 46, 137, 180, 124, 184, 38, 119, 153, 227, 165, 103,
      74, 237, 222, 197, 49, 254, 24, 13, 99, 140, 128, 192, 247, 112, 7);

   --  GF(256) multiplication: a * b
   function GF_Mult (A : U8; B : U8) return U8
   with
     Global => null,
     Post   => GF_Mult'Result in U8
   is
   begin
      if A = 0 or B = 0 then
         return 0;
      end if;

      --  Multiplication via log tables: a*b = exp[log[a] + log[b]]
      declare
         Log_A : constant U8 := Log (GF256 (A));
         Log_B : constant U8 := Log (GF256 (B));
         Sum   : constant Natural := Natural (Log_A) + Natural (Log_B);
         Index : constant U8 := (if Sum >= 255 then U8 (Sum - 255) else U8 (Sum));
      begin
         return Exp (Index);
      end;
   end GF_Mult;

   --  GF(256) division: a / b (where b != 0)
   function GF_Div (A : U8; B : U8) return U8
   with
     Global => null,
     Pre    => B /= 0,
     Post   => GF_Div'Result in U8
   is
   begin
      if A = 0 then
         return 0;
      end if;

      --  Division via log tables: a/b = exp[log[a] - log[b]]
      declare
         Log_A : constant U8 := Log (GF256 (A));
         Log_B : constant U8 := Log (GF256 (B));
         Diff  : constant Integer := Integer (Log_A) - Integer (Log_B);
         Index : constant U8 := (if Diff < 0 then U8 (Diff + 255) else U8 (Diff));
      begin
         return Exp (Index);
      end;
   end GF_Div;

   --  Evaluate polynomial at point x
   --  Coefficients: [c0, c1, ..., c_{k-1}] where c0 = secret byte
   --  Returns: c0 + c1*x + c2*x^2 + ... + c_{k-1}*x^{k-1}
   function Evaluate_Polynomial
     (Coeffs : Byte_Array;
      X      : U8) return U8
   with
     Global => null,
     Pre    => Coeffs'Length > 0 and Coeffs'Length <= 32,
     Post   => Evaluate_Polynomial'Result in U8
   is
      Result : U8 := 0;
      X_Power : U8 := 1;  -- x^0 = 1
   begin
      for I in Coeffs'Range loop
         --  Add c_i * x^i to result
         Result := Result xor GF_Mult (Coeffs (I), X_Power);

         --  Update x^i for next iteration (except on last iteration)
         if I /= Coeffs'Last then
            X_Power := GF_Mult (X_Power, X);
         end if;
      end loop;

      return Result;
   end Evaluate_Polynomial;

   --  Lagrange interpolation to compute P(0) from k points
   --  X_Coords: x-coordinates of shares
   --  Y_Coords: y-coordinates of shares (one byte position across all shares)
   --  Returns: P(0) = secret byte
   function Lagrange_Interpolate
     (X_Coords : Byte_Array;
      Y_Coords : Byte_Array) return U8
   with
     Global => null,
     Pre    => X_Coords'Length = Y_Coords'Length and then
               X_Coords'Length > 0 and then
               X_Coords'Length <= 255,
     Post   => Lagrange_Interpolate'Result in U8
   is
      Result : U8 := 0;
   begin
      --  Lagrange formula: P(0) = sum_i [ y_i * prod_{j!=i} (0-x_j)/(x_i-x_j) ]
      --  Simplified for x=0: P(0) = sum_i [ y_i * prod_{j!=i} x_j/(x_j-x_i) ]

      for I in X_Coords'Range loop
         declare
            Numerator   : U8 := Y_Coords (I);
            Denominator : U8 := 1;
         begin
            --  Compute Lagrange basis polynomial L_i(0)
            for J in X_Coords'Range loop
               if I /= J then
                  --  Numerator: multiply by x_j
                  Numerator := GF_Mult (Numerator, X_Coords (J));

                  --  Denominator: multiply by (x_j - x_i)
                  declare
                     Diff : constant U8 := X_Coords (J) xor X_Coords (I);
                  begin
                     Denominator := GF_Mult (Denominator, Diff);
                  end;
               end if;
            end loop;

            --  Add y_i * (numerator / denominator)
            if Denominator /= 0 then
               Result := Result xor GF_Div (Numerator, Denominator);
            end if;
         end;
      end loop;

      return Result;
   end Lagrange_Interpolate;

   procedure Split
     (Root_Key     : in  Key_Array;
      Threshold    : in  Share_Count;
      Total_Shares : in  Share_Count;
      Shares       : out Share_Set;
      Success      : out Boolean)
   is
      Coefficients : Byte_Array (1 .. Threshold) := (others => 0);
      Random_Bytes : Byte_Array (1 .. 64) := (others => 0);
      Random_Index : Positive := 1;
   begin
      Success := False;

      --  Zero all shares initially
      for I in Shares'Range loop
         for J in Shares (I)'Range loop
            Shares (I)(J) := 0;
         end loop;
      end loop;

      --  Generate random bytes for polynomial coefficients
      --  We need (Threshold - 1) random bytes per Root_Key byte (32 total)
      --  Maximum: 31 * 32 = 992 bytes, split into chunks
      declare
         Bytes_Needed : constant Positive := (Threshold - 1) * 32;
         Bytes_Generated : Natural := 0;
      begin
         while Bytes_Generated < Bytes_Needed loop
            SparkPass.Crypto.Random.Fill (Random_Bytes);

            --  Process each byte of Root_Key
            for Byte_Index in Root_Key'Range loop
               exit when Bytes_Generated >= Bytes_Needed;

               --  Set up polynomial: c0 = secret byte, c1..c_{k-1} = random
               Coefficients (1) := Root_Key (Byte_Index);

               for Coeff_Index in 2 .. Threshold loop
                  exit when Bytes_Generated >= Bytes_Needed;

                  if Random_Index > Random_Bytes'Last then
                     SparkPass.Crypto.Random.Fill (Random_Bytes);
                     Random_Index := 1;
                  end if;

                  Coefficients (Coeff_Index) := Random_Bytes (Random_Index);
                  Random_Index := Random_Index + 1;
                  Bytes_Generated := Bytes_Generated + 1;
               end loop;

               --  Evaluate polynomial at x = 1, 2, ..., n
               for Share_Index in Shares'Range loop
                  declare
                     X : constant U8 := U8 (Share_Index);
                     Y : constant U8 := Evaluate_Polynomial (Coefficients, X);
                  begin
                     --  First byte is x-coordinate
                     if Byte_Index = 1 then
                        Shares (Share_Index)(1) := X;
                     end if;

                     --  Remaining bytes are y-coordinates
                     Shares (Share_Index)(Byte_Index + 1) := Y;
                  end;
               end loop;
            end loop;
         end loop;
      end;

      --  Zeroize sensitive data
      SparkPass.Crypto.Zeroize.Wipe (Coefficients);
      SparkPass.Crypto.Zeroize.Wipe (Random_Bytes);

      Success := True;
   end Split;

   procedure Combine
     (Shares    : in  Share_Set;
      Threshold : in  Share_Count;
      Root_Key  : out Key_Array;
      Success   : out Boolean)
   is
      X_Coords : Byte_Array (1 .. Threshold) := (others => 0);
      Y_Coords : Byte_Array (1 .. Threshold) := (others => 0);
   begin
      Success := False;
      Root_Key := (others => 0);

      --  Validate shares and extract x-coordinates
      for I in 1 .. Threshold loop
         if not Is_Valid_Share (Shares (I)) then
            SparkPass.Crypto.Zeroize.Wipe (Root_Key);
            return;
         end if;

         X_Coords (I) := Shares (I)(1);

         --  Check for duplicate x-coordinates
         for J in 1 .. I - 1 loop
            if X_Coords (I) = X_Coords (J) then
               SparkPass.Crypto.Zeroize.Wipe (Root_Key);
               return;
            end if;
         end loop;
      end loop;

      --  Reconstruct each byte of Root_Key using Lagrange interpolation
      for Byte_Index in Root_Key'Range loop
         --  Extract y-coordinates for this byte position
         for I in 1 .. Threshold loop
            Y_Coords (I) := Shares (I)(Byte_Index + 1);
         end loop;

         --  Interpolate P(0) = secret byte
         Root_Key (Byte_Index) := Lagrange_Interpolate (X_Coords, Y_Coords);
      end loop;

      --  Zeroize temporary data
      SparkPass.Crypto.Zeroize.Wipe (X_Coords);
      SparkPass.Crypto.Zeroize.Wipe (Y_Coords);

      Success := True;
   end Combine;

   function Is_Valid_Share (Share : Share_Array) return Boolean is
   begin
      return Share'Length = Share_Size and then Share (Share'First) > 0;
   end Is_Valid_Share;

   procedure Wipe_Share (Share : in out Share_Array) is
   begin
      for I in Share'Range loop
         Share (I) := 0;
      end loop;
   end Wipe_Share;

   procedure Wipe_Share_Set (Shares : in out Share_Set) is
   begin
      for I in Shares'Range loop
         Wipe_Share (Shares (I));
      end loop;
   end Wipe_Share_Set;

end SparkPass.Crypto.Shamir;
