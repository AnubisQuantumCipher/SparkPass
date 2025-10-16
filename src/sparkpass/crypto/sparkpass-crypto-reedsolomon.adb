pragma SPARK_Mode (On);
with Interfaces; use Interfaces;

package body SparkPass.Crypto.ReedSolomon is

   --  ========================================================================
   --  GF(256) Table Generation (Compile-Time)
   --  ========================================================================

   --  Initialize log/antilog tables
   procedure Initialize_Antilog_Table is
      Value : Unsigned_16 := 1;
   begin
      GF_Antilog (0) := 1;

      for I in 1 .. 254 loop
         Value := Value * 2;
         if Value >= 256 then
            Value := Value xor 16#1D#;  -- GF_Poly with bit 8 removed
         end if;
         --  Value is guaranteed to be < 256 after XOR operation
         GF_Antilog (I) := GF256_Element (Value and 16#FF#);
      end loop;

      for I in 255 .. 510 loop
         GF_Antilog (I) := GF_Antilog (I - 255);
      end loop;
   end Initialize_Antilog_Table;

   procedure Initialize_Log_Table is
   begin
      for I in 0 .. 254 loop
         if GF_Antilog (I) /= 0 then
            GF_Log (GF_Antilog (I)) := I;
         end if;
      end loop;
   end Initialize_Log_Table;

   --  ========================================================================
   --  GF(256) Arithmetic Operations
   --  ========================================================================

   function GF_Multiply (A, B : GF256_Element) return GF256_Element is
   begin
      --  Special case: multiplication by zero
      if A = 0 or B = 0 then
         return 0;
      end if;

      --  Fast multiplication using log tables:
      --  a * b = antilog(log(a) + log(b) mod 255)
      --  Extended antilog table [0..510] eliminates modulo operation
      declare
         Log_Sum : constant Natural := GF_Log (A) + GF_Log (B);
      begin
         return GF_Antilog (Log_Sum);
      end;
   end GF_Multiply;

   function GF_Divide (A, B : GF256_Element) return GF256_Element is
   begin
      --  Division by zero or zero dividend
      if A = 0 or B = 0 then
         return 0;
      end if;

      --  Division using log tables:
      --  a / b = antilog(log(a) - log(b) mod 255)
      declare
         Log_A   : constant Natural := GF_Log (A);
         Log_B   : constant Natural := GF_Log (B);
         Log_Diff : Natural;
      begin
         --  Compute log(a) - log(b) with wraparound
         if Log_A >= Log_B then
            Log_Diff := Log_A - Log_B;
         else
            Log_Diff := (Log_A + 255) - Log_B;
         end if;

         return GF_Antilog (Log_Diff);
      end;
   end GF_Divide;

   function GF_Power (Base : GF256_Element; Exponent : Natural) return GF256_Element is
   begin
      --  Special cases
      if Exponent = 0 then
         return 1;  -- x^0 = 1 for any x
      end if;

      if Base = 0 then
         return 0;  -- 0^n = 0 for n > 0
      end if;

      --  Exponentiation using log tables:
      --  a^n = antilog(log(a) * n mod 255)
      declare
         Log_Base : constant Natural := GF_Log (Base);
         Log_Result : constant Natural := (Log_Base * Exponent) mod 255;
      begin
         return GF_Antilog (Log_Result);
      end;
   end GF_Power;

   --  ========================================================================
   --  Reed-Solomon Generator Polynomial
   --  ========================================================================

   --  Initialize generator polynomial
   procedure Initialize_Generator_Polynomial is
      Temp : Generator_Polynomial := (others => 0);
      Root : GF256_Element;
   begin
      RS_Generator := (others => 0);
      RS_Generator (0) := 1;

      for I in 0 .. Parity_Symbols - 1 loop
         Root := GF_Antilog (I);
         Temp := (others => 0);

         for J in 0 .. I loop
            Temp (J + 1) := RS_Generator (J);
         end loop;

         for J in 0 .. I loop
            Temp (J) := GF_Add (Temp (J), GF_Multiply (Root, RS_Generator (J)));
         end loop;

         RS_Generator (0 .. I + 1) := Temp (0 .. I + 1);
      end loop;
   end Initialize_Generator_Polynomial;

   --  Initialize all tables
   procedure Initialize_Tables is
   begin
      Initialize_Antilog_Table;
      Initialize_Log_Table;
      Initialize_Generator_Polynomial;
   end Initialize_Tables;

   --  ========================================================================
   --  Polynomial Evaluation (Horner's Method)
   --  ========================================================================

   function Poly_Eval
     (Coeffs : Byte_Array;
      Point  : GF256_Element) return GF256_Element
   is
      Result : GF256_Element := 0;
   begin
      --  Horner's method: evaluate polynomial from highest degree down
      --  p(x) = ((...(a_n * x + a_{n-1}) * x + ...) * x + a_0)
      for I in reverse Coeffs'Range loop
         Result := GF_Add (GF_Multiply (Result, Point), Coeffs (I));
      end loop;

      return Result;
   end Poly_Eval;

   --  ========================================================================
   --  Reed-Solomon Encoding (Systematic)
   --  ========================================================================

   procedure Encode
     (Data    : in  Data_Block;
      Parity  : out Parity_Block;
      Success : out Boolean)
   is
      --  Remainder polynomial from division by generator
      Remainder : Parity_Block := (others => 0);
      Feedback  : GF256_Element;
   begin
      --  Initialize output
      Parity := (others => 0);
      Success := False;

      --  Systematic encoding via polynomial long division:
      --  1. Treat data as polynomial: m(x) = Σ data[i] * x^i
      --  2. Compute x^(n-k) * m(x) (shift data by parity_symbols positions)
      --  3. Divide by generator g(x), remainder is parity
      --
      --  Algorithm: Synthetic division (similar to CRC computation)

      --  Process each data symbol
      for I in Data'Range loop
         --  Feedback term: MSB of current remainder + next data symbol
         Feedback := Data (I) xor Remainder (Parity_Symbols);

         --  Shift remainder left by 1 position
         for J in reverse 2 .. Parity_Symbols loop
            Remainder (J) := Remainder (J - 1);
         end loop;
         Remainder (1) := 0;

         --  Add feedback * generator polynomial to remainder
         --  This is equivalent to polynomial division step
         if Feedback /= 0 then
            for J in 1 .. Parity_Symbols loop
               Remainder (J) := GF_Add (
                  Remainder (J),
                  GF_Multiply (RS_Generator (J), Feedback)
               );
            end loop;
         end if;
      end loop;

      --  The remainder is the parity (negation is identity in GF(2^8))
      Parity := Remainder;
      Success := True;

   end Encode;

   --  ========================================================================
   --  Reed-Solomon Decoding: Syndrome Computation
   --  ========================================================================

   procedure Compute_Syndromes
     (Codeword   : in  Codeword_Block;
      Has_Errors : out Boolean)
   is
      Syndrome : GF256_Element;
      Root     : GF256_Element;
   begin
      Has_Errors := False;

      --  Compute syndromes S_i = r(α^i) for i = 0..2t-1
      --  If all syndromes = 0, no errors detected
      for I in 0 .. 2 * Correction_Capacity - 1 loop
         --  Root = α^i
         Root := GF_Antilog (I);

         --  Evaluate received polynomial at root
         Syndrome := Poly_Eval (Byte_Array (Codeword), Root);

         --  If any syndrome ≠ 0, errors present
         if Syndrome /= 0 then
            Has_Errors := True;
            return;  -- Early exit for performance
         end if;
      end loop;

   end Compute_Syndromes;

   --  ========================================================================
   --  Reed-Solomon Decoding: Berlekamp-Massey Algorithm
   --  ========================================================================

   --  Berlekamp-Massey: Find error locator polynomial Λ(x)
   --
   --  Given syndromes S_0, S_1, ..., S_{2t-1}, find minimal polynomial Λ(x)
   --  such that: Σ_{i=0}^{L} Λ_i * S_{j-i} = 0 for j = L..2t-1
   --
   --  Output: Error locator polynomial coefficients (Λ_0 = 1 always)
   --  Returns: Degree of error locator polynomial (number of errors)
   procedure Berlekamp_Massey
     (Syndromes : in  Byte_Array;
      Lambda    : out Byte_Array;
      Degree    : out Natural)
   with
     Pre => Syndromes'Length = 2 * Correction_Capacity and
            Lambda'Length >= Correction_Capacity + 1
   is
      L : Natural := 0;  -- Current degree of Λ(x)
      M : Natural := 1;  -- Distance since last length change
      B : array (0 .. Correction_Capacity) of U8 := (others => 0);  -- B(x) polynomial
      T : array (0 .. Correction_Capacity) of U8 := (others => 0);  -- Temporary
      Discrepancy : GF256_Element;
      Scale : GF256_Element;
      B_Discrepancy : GF256_Element := 1;
   begin
      --  Initialize Λ(x) = 1, B(x) = 1
      Lambda (Lambda'First) := 1;
      for I in Lambda'First + 1 .. Lambda'Last loop
         Lambda (I) := 0;
      end loop;
      B (0) := 1;

      --  Iterative algorithm: process each syndrome
      for N in 0 .. 2 * Correction_Capacity - 1 loop
         --  Compute discrepancy: Δ = S_n + Σ_{i=1}^{L} Λ_i * S_{n-i}
         Discrepancy := Syndromes (Syndromes'First + N);

         for I in 1 .. L loop
            Discrepancy := GF_Add (
               Discrepancy,
               GF_Multiply (
                  Lambda (Lambda'First + I),
                  Syndromes (Syndromes'First + N - I)
               )
            );
         end loop;

         if Discrepancy = 0 then
            --  No correction needed this iteration
            M := M + 1;
         else
            --  Update Λ(x) using B(x)
            for I in 0 .. L loop
               T (I) := Lambda (Lambda'First + I);
            end loop;

            --  Scale = discrepancy / B_discrepancy
            Scale := GF_Divide (Discrepancy, B_Discrepancy);

            --  Λ(x) := Λ(x) - (Δ/d) * x^m * B(x)
            for I in 0 .. Correction_Capacity - M loop
               Lambda (Lambda'First + I + M) := GF_Add (
                  Lambda (Lambda'First + I + M),
                  GF_Multiply (Scale, B (I))
               );
            end loop;

            --  Update B(x) and degree if necessary
            if 2 * L <= N then
               L := N + 1 - L;
               for I in T'Range loop
                  B (I) := T (I);
               end loop;
               B_Discrepancy := Discrepancy;
               M := 1;
            else
               M := M + 1;
            end if;
         end if;
      end loop;

      Degree := L;
   end Berlekamp_Massey;

   --  ========================================================================
   --  Reed-Solomon Decoding: Chien Search
   --  ========================================================================

   --  Chien Search: Find roots of error locator polynomial Λ(x)
   --
   --  Evaluate Λ(α^(-i)) for i = 0..n-1 to find error locations.
   --  If Λ(α^(-i)) = 0, then error is at position i in codeword.
   --
   --  Output: Array of error positions (0-indexed)
   --  Returns: Number of error locations found
   procedure Chien_Search
     (Lambda    : in  Byte_Array;
      Degree    : in  Natural;
      Locations : out Error_Locations;
      Count     : out Natural)
   with
     Pre => Lambda'Length >= Degree + 1 and Degree <= Correction_Capacity
   is
      Eval : GF256_Element;
      Powers : array (1 .. Degree) of GF256_Element;
   begin
      Count := 0;
      Locations := (others => 0);

      --  Initialize powers: Powers[i] = α^(-i) for i = 1..Degree
      for I in 1 .. Degree loop
         Powers (I) := GF_Antilog ((255 - I) mod 255);
      end loop;

      --  Test each codeword position
      for I in 0 .. Total_Symbols - 1 loop
         --  Evaluate Λ(α^(-i)) using current powers
         Eval := Lambda (Lambda'First);  -- Λ_0 term

         for J in 1 .. Degree loop
            Eval := GF_Add (Eval, GF_Multiply (Lambda (Lambda'First + J), Powers (J)));
         end loop;

         --  If Λ(α^(-i)) = 0, found an error at position i
         if Eval = 0 then
            if Count < Correction_Capacity then
               Count := Count + 1;
               Locations (Count) := I;
            end if;
         end if;

         --  Update powers: Powers[j] := Powers[j] * α^(-j)
         for J in 1 .. Degree loop
            Powers (J) := GF_Multiply (Powers (J), GF_Antilog ((255 - J) mod 255));
         end loop;
      end loop;

   end Chien_Search;

   --  ========================================================================
   --  Reed-Solomon Decoding: Forney Algorithm
   --  ========================================================================

   --  Forney Algorithm: Compute error magnitudes at known locations
   --
   --  Given syndromes S and error locator Λ(x), compute error evaluator Ω(x):
   --  Ω(x) = S(x) * Λ(x) mod x^(2t)
   --
   --  Error magnitude at location X_i: e_i = -Ω(X_i^(-1)) / Λ'(X_i^(-1))
   --  where Λ'(x) is the formal derivative of Λ(x)
   procedure Forney_Algorithm
     (Syndromes  : in  Byte_Array;
      Lambda     : in  Byte_Array;
      Degree     : in  Natural;
      Locations  : in  Error_Locations;
      Loc_Count  : in  Natural;
      Magnitudes : out Byte_Array)
   with
     Pre => Syndromes'Length = 2 * Correction_Capacity and
            Lambda'Length >= Degree + 1 and
            Loc_Count <= Correction_Capacity and
            Magnitudes'Length >= Loc_Count
   is
      Omega : array (0 .. 2 * Correction_Capacity) of U8 := (others => 0);
      X_Inv : GF256_Element;
      Omega_Eval : GF256_Element;
      Lambda_Deriv : GF256_Element;
      Magnitude : GF256_Element;
   begin
      Magnitudes := (others => 0);

      --  Compute error evaluator Ω(x) = S(x) * Λ(x) mod x^(2t)
      --  This is polynomial multiplication truncated to degree 2t-1
      for I in 0 .. Degree loop
         for J in 0 .. 2 * Correction_Capacity - 1 loop
            if I + J < 2 * Correction_Capacity then
               Omega (I + J) := GF_Add (
                  Omega (I + J),
                  GF_Multiply (
                     Lambda (Lambda'First + I),
                     Syndromes (Syndromes'First + J)
                  )
               );
            end if;
         end loop;
      end loop;

      --  Compute error magnitude at each location
      for I in 1 .. Loc_Count loop
         --  X_i^(-1) = α^(n-1-loc) (inverse of error location)
         X_Inv := GF_Antilog ((Total_Symbols - 1 - Locations (I)) mod 255);

         --  Evaluate Ω(X_i^(-1))
         Omega_Eval := 0;
         for J in 0 .. 2 * Correction_Capacity - 1 loop
            Omega_Eval := GF_Add (Omega_Eval, GF_Multiply (Omega (J), GF_Power (X_Inv, J)));
         end loop;

         --  Compute Λ'(X_i^(-1)) (formal derivative, only odd powers in GF(2^8))
         Lambda_Deriv := 0;
         for J in 1 .. Degree loop
            if J mod 2 = 1 then  -- Only odd powers contribute in characteristic 2
               Lambda_Deriv := GF_Add (
                  Lambda_Deriv,
                  GF_Multiply (Lambda (Lambda'First + J), GF_Power (X_Inv, J))
               );
            end if;
         end loop;

         --  Error magnitude: e_i = -Ω(X_i^(-1)) / Λ'(X_i^(-1))
         --  Negation is identity in GF(2^8)
         if Lambda_Deriv /= 0 then
            Magnitude := GF_Divide (Omega_Eval, Lambda_Deriv);
            Magnitudes (Magnitudes'First + I - 1) := Magnitude;
         else
            --  Should not happen if error locator is correct
            Magnitudes (Magnitudes'First + I - 1) := 0;
         end if;
      end loop;

   end Forney_Algorithm;

   --  ========================================================================
   --  Reed-Solomon Decoding (Full Pipeline)
   --  ========================================================================

   procedure Decode
     (Codeword        : in out Codeword_Block;
      Corrected_Count : out Natural;
      Status          : out Decode_Status)
   is
      Syndromes : array (1 .. 2 * Correction_Capacity) of U8;
      Lambda : array (1 .. Correction_Capacity + 1) of U8 := (others => 0);
      Locations : Error_Locations := (others => 0);
      Magnitudes : array (1 .. Correction_Capacity) of U8 := (others => 0);
      Lambda_Degree : Natural;
      Location_Count : Natural;
      Has_Errors : Boolean;
   begin
      --  Initialize outputs
      Corrected_Count := 0;
      Status := Invalid_Input;

      --  Step 1: Compute syndromes
      Compute_Syndromes (Codeword, Has_Errors);

      if not Has_Errors then
         --  No errors detected, success
         Status := Success;
         return;
      end if;

      --  Step 2: Compute syndromes explicitly for Berlekamp-Massey
      for I in 0 .. 2 * Correction_Capacity - 1 loop
         Syndromes (I + 1) := Poly_Eval (Byte_Array (Codeword), GF_Antilog (I));
      end loop;

      --  Step 3: Berlekamp-Massey algorithm (find error locator polynomial)
      declare
         Syndromes_View : Byte_Array (1 .. 2 * Correction_Capacity);
         for Syndromes_View'Address use Syndromes'Address;
         Lambda_View : Byte_Array (1 .. Correction_Capacity + 1);
         for Lambda_View'Address use Lambda'Address;
      begin
         Berlekamp_Massey (Syndromes_View, Lambda_View, Lambda_Degree);
      end;

      --  Check if too many errors
      if Lambda_Degree > Correction_Capacity then
         Status := Uncorrectable_Errors;
         return;
      end if;

      --  Step 4: Chien search (find error locations)
      declare
         Lambda_View : Byte_Array (1 .. Correction_Capacity + 1);
         for Lambda_View'Address use Lambda'Address;
      begin
         Chien_Search (Lambda_View, Lambda_Degree, Locations, Location_Count);
      end;

      --  Verify location count matches polynomial degree
      if Location_Count /= Lambda_Degree then
         Status := Uncorrectable_Errors;
         return;
      end if;

      --  Step 5: Forney algorithm (compute error magnitudes)
      declare
         Syndromes_View : Byte_Array (1 .. 2 * Correction_Capacity);
         for Syndromes_View'Address use Syndromes'Address;
         Lambda_View : Byte_Array (1 .. Correction_Capacity + 1);
         for Lambda_View'Address use Lambda'Address;
         Magnitudes_View : Byte_Array (1 .. Correction_Capacity);
         for Magnitudes_View'Address use Magnitudes'Address;
      begin
         Forney_Algorithm (Syndromes_View, Lambda_View, Lambda_Degree, Locations, Location_Count, Magnitudes_View);
      end;

      --  Step 6: Correct errors
      for I in 1 .. Location_Count loop
         if Locations (I) < Total_Symbols then
            Codeword (Codeword'First + Locations (I)) :=
              GF_Add (Codeword (Codeword'First + Locations (I)),
                      Magnitudes (I));
         end if;
      end loop;

      --  Success
      Corrected_Count := Location_Count;
      Status := Success;

   end Decode;

begin
   --  Initialize tables at package elaboration
   Initialize_Tables;

end SparkPass.Crypto.ReedSolomon;
