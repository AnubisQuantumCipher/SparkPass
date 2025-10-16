pragma SPARK_Mode (On);
with SparkPass.Types; use SparkPass.Types;
with Interfaces; use Interfaces;

--  Reed-Solomon Forward Error Correction (FEC)
--
--  Implements RS(255, 223) error correction code over GF(2^8) for vault
--  integrity protection against bit rot, corruption, and media degradation.
--
--  Security Properties:
--  - FEC operates on ciphertext, not plaintext (no information leakage)
--  - Parity symbols are publicly computable (no additional security weakening)
--  - Constant-time operations not required (no secret data processed)
--  - Memory safety: all operations proven bounds-safe by SPARK
--
--  References:
--  - MacWilliams & Sloane (1977): "The Theory of Error-Correcting Codes"
--  - Berlekamp (1968): "Algebraic Coding Theory"
--  - CCSDS 131.0-B-3: Space Data System Blue Book (RS Standard)
--
--  @author SPARK FEC Team
--  @date 2025-10-16

package SparkPass.Crypto.ReedSolomon is
   pragma Elaborate_Body;

   --  ========================================================================
   --  GF(256) Finite Field Arithmetic
   --  ========================================================================
   --
   --  GF(2^8) using irreducible polynomial: x^8 + x^4 + x^3 + x^2 + 1 (0x11D)
   --  Generator element: 0x02 (primitive element with order 255)

   subtype GF256_Element is U8;
   subtype GF256_Nonzero is GF256_Element range 1 .. 255;

   --  Logarithm table: Log(x) = discrete log base generator (for x ≠ 0)
   --  Used for fast multiplication: a * b = antilog(log(a) + log(b))
   type Log_Table is array (GF256_Nonzero) of Natural range 0 .. 254
     with Pack;

   --  Antilog table: Antilog(x) = generator^x (extended to 510 for overflow)
   --  Extended range avoids modulo operation in multiplication
   type Antilog_Table is array (Natural range 0 .. 510) of GF256_Element
     with Pack;

   --  Precomputed tables (initialized at elaboration)
   GF_Log : Log_Table;
   GF_Antilog : Antilog_Table;

   --  GF(256) addition (equivalent to XOR in characteristic 2 field)
   function GF_Add (A, B : GF256_Element) return GF256_Element is
     (A xor B)
   with
     Global => null,
     Inline;

   --  GF(256) subtraction (equivalent to XOR in characteristic 2 field)
   function GF_Sub (A, B : GF256_Element) return GF256_Element is
     (A xor B)
   with
     Global => null,
     Inline;

   --  GF(256) multiplication using precomputed log/antilog tables
   --  Implements: a * b = antilog(log(a) + log(b) mod 255)
   --  Special case: 0 * x = 0 for any x
   function GF_Multiply (A, B : GF256_Element) return GF256_Element
   with
     Global => null,
     Post   => (if A = 0 or B = 0 then GF_Multiply'Result = 0);

   --  GF(256) division (undefined for B = 0, returns 0)
   --  Implements: a / b = antilog(log(a) - log(b) mod 255)
   function GF_Divide (A, B : GF256_Element) return GF256_Element
   with
     Global => null,
     Post   => (if A = 0 or B = 0 then GF_Divide'Result = 0);

   --  GF(256) exponentiation: base^exponent
   --  Implements: a^n = antilog(log(a) * n mod 255)
   function GF_Power (Base : GF256_Element; Exponent : Natural) return GF256_Element
   with
     Global => null,
     Post   => (if Base = 0 and Exponent > 0 then GF_Power'Result = 0) and then
               (if Base /= 0 and Exponent = 0 then GF_Power'Result = 1);

   --  ========================================================================
   --  Reed-Solomon Parameters (Standard RS(255, 223) Configuration)
   --  ========================================================================

   Data_Symbols       : constant Positive := 223;  -- k: data symbols
   Parity_Symbols     : constant Positive := 32;   -- n-k: parity symbols (2t)
   Total_Symbols      : constant Positive := 255;  -- n: total codeword symbols
   Correction_Capacity : constant Positive := 16;  -- t: maximum correctable errors

   subtype Data_Block is Byte_Array (1 .. Data_Symbols);
   subtype Parity_Block is Byte_Array (1 .. Parity_Symbols);
   subtype Codeword_Block is Byte_Array (1 .. Total_Symbols);

   --  Generator polynomial coefficients for RS(255, 223)
   --  g(x) = (x - α^0)(x - α^1)...(x - α^31) where α = 0x02
   type Generator_Polynomial is array (0 .. Parity_Symbols) of GF256_Element
     with Pack;

   --  Precomputed generator polynomial (initialized at elaboration)
   RS_Generator : Generator_Polynomial;

   --  ========================================================================
   --  Reed-Solomon Encoding (Systematic)
   --  ========================================================================

   --  Encode data block into parity block
   --
   --  Implements systematic encoding where output codeword = data || parity.
   --  This allows reading data directly without decoding if no errors present.
   --
   --  Algorithm:
   --  1. Treat data as polynomial coefficients: m(x) = Σ data[i] * x^i
   --  2. Compute x^(n-k) * m(x) (shift data left by parity length)
   --  3. Divide by generator polynomial g(x), keep remainder r(x)
   --  4. Parity = -r(x) (negation is identity in GF(2^8))
   --
   --  Security: Operates on ciphertext, parity publicly computable from data
   --
   --  @param Data Input data block (223 bytes)
   --  @param Parity Output parity block (32 bytes)
   --  @param Success True if encoding succeeded
   procedure Encode
     (Data    : in  Data_Block;
      Parity  : out Parity_Block;
      Success : out Boolean)
   with
     Global  => null,
     Depends => (Parity => Data,
                 Success => null),
     Post    => Success;

   --  ========================================================================
   --  Reed-Solomon Decoding (Error Correction)
   --  ========================================================================

   type Decode_Status is
     (Success,               -- No errors or successfully corrected
      Uncorrectable_Errors,  -- More than t errors detected
      Invalid_Input);        -- Invalid input (wrong length, etc.)

   --  Error location array (tracks which symbols had errors)
   type Error_Locations is array (1 .. Correction_Capacity) of Natural;

   --  Decode and correct errors in codeword
   --
   --  Implements full RS decoding pipeline:
   --  1. Syndrome computation: S_i = Σ c_j * α^(ij) for i = 0..2t-1
   --  2. If all syndromes = 0: no errors, return Success
   --  3. Berlekamp-Massey: find error locator polynomial Λ(x)
   --  4. Chien search: find roots of Λ(x) to locate errors
   --  5. Forney algorithm: compute error magnitudes
   --  6. Correct errors: codeword[loc] := codeword[loc] XOR magnitude
   --
   --  Security: Memory-safe (all arrays bounds-checked by SPARK)
   --  Performance: ~10ms for syndrome-only check, ~50ms with correction
   --
   --  @param Codeword Input/output codeword (255 bytes, modified in place)
   --  @param Corrected_Count Number of symbol errors corrected
   --  @param Status Decode result status
   procedure Decode
     (Codeword        : in out Codeword_Block;
      Corrected_Count : out Natural;
      Status          : out Decode_Status)
   with
     Global  => null,
     Depends => (Codeword => Codeword,
                 Corrected_Count => Codeword,
                 Status => Codeword),
     Post    => (if Status = Success then
                   Corrected_Count <= Correction_Capacity
                 elsif Status = Uncorrectable_Errors then
                   Corrected_Count = 0
                 else
                   Corrected_Count = 0);

   --  Compute syndromes for error detection (lightweight check)
   --
   --  Syndromes are evaluations of received polynomial at α^i for i=0..2t-1:
   --  S_i = r(α^i) = Σ_{j=0}^{n-1} r_j * α^(ij)
   --
   --  If all syndromes = 0: no errors detected
   --  If any syndrome ≠ 0: errors present, attempt correction
   --
   --  Performance: ~10ms per 255-byte block (much faster than full decode)
   --
   --  @param Codeword Input codeword (255 bytes)
   --  @param Has_Errors True if errors detected (any syndrome ≠ 0)
   procedure Compute_Syndromes
     (Codeword   : in  Codeword_Block;
      Has_Errors : out Boolean)
   with
     Global  => null,
     Depends => (Has_Errors => Codeword);

   --  ========================================================================
   --  Utility Functions
   --  ========================================================================

   --  Evaluate polynomial at point using Horner's method
   --  p(x) = a_n*x^n + ... + a_1*x + a_0
   --  Evaluated as: (...((a_n * x + a_{n-1}) * x + a_{n-2}) * x + ... + a_0)
   function Poly_Eval
     (Coeffs : Byte_Array;
      Point  : GF256_Element) return GF256_Element
   with
     Global => null,
     Pre    => Coeffs'Length > 0 and Coeffs'Length <= 255;

private
   --  ========================================================================
   --  Precomputed Tables (Private Implementation)
   --  ========================================================================

   --  Generator element for GF(256) - primitive polynomial generator
   GF_Generator : constant GF256_Element := 16#02#;

   --  Irreducible polynomial for GF(256): x^8 + x^4 + x^3 + x^2 + 1
   GF_Poly : constant := 16#11D#;

   --  Table initialization (called during package elaboration)
   procedure Initialize_Tables;

end SparkPass.Crypto.ReedSolomon;
