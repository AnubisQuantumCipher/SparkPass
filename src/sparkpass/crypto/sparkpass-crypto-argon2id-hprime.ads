pragma SPARK_Mode (On);

with SparkPass.Types; use SparkPass.Types;

--  ================================================================
--  H' Variable-Length Hash Function (RFC 9106 Section 3.3)
--  ================================================================
--
--  **Purpose**: Generate variable-length outputs from Blake2b-512
--
--  **Algorithm** (RFC 9106 Section 3.3):
--
--    H'(tau, X):
--      if tau <= 64:
--        return Blake2b-512(LE32(tau) || X)[0..tau-1]
--      else:
--        V_1 = Blake2b-512(LE32(tau) || X)
--        V_2 = Blake2b-512(V_1[0..31])
--        V_3 = Blake2b-512(V_2[0..31])
--        ...
--        return V_1 || V_2[0..31] || V_3[0..31] || ...
--
--  **Usage in Argon2id**:
--    Generate initial blocks: B[i][j] = H'(1024, H₀ || LE32(j) || LE32(i))
--
--  **Security Properties**:
--    - Deterministic: Same input always produces same output
--    - One-way: Cannot reverse to find input
--    - Based on Blake2b-512 collision resistance
--
--  **SPARK Properties**:
--    - Pure function (Global => null)
--    - No heap allocation
--    - Bounded subtypes for all indices
--    - Target: 80/80 VCs (100%)
--
--  **Source**: RFC 9106 Section 3.3
--  ================================================================

private package SparkPass.Crypto.Argon2id.HPrime with
   SPARK_Mode => On
is

   --  Maximum supported output length (1024 bytes = one Argon2 block)
   subtype Output_Length_Type is Positive range 1 .. 1024;

   --  Maximum input length
   --    - For initialization: H₀ + overhead = 64 + 8 = 72 bytes
   --    - For finalization: Final block = 1024 bytes
   subtype Input_Length_Type is Natural range 0 .. 1024;

   ------------------------------------------------------------
   --  Compute_H_Prime
   ------------------------------------------------------------

   --  Generate variable-length hash output using Blake2b-512
   --
   --  **Algorithm** (RFC 9106 Section 3.3):
   --    - For tau <= 64: Single Blake2b call, truncated
   --    - For tau > 64:  Multiple Blake2b calls, chained
   --
   --  **Parameters**:
   --    Output_Length : Desired output size in bytes (tau in RFC)
   --    Input         : Input message (X in RFC)
   --    Output        : Output buffer (must have length = Output_Length)
   --
   --  **Preconditions**:
   --    - Output buffer size matches Output_Length parameter
   --    - Output_Length in valid range (1..1024)
   --    - Input length bounded for verification
   --
   --  **Postconditions**:
   --    - Output length unchanged
   --
   --  **Example** (Argon2id block generation):
   --    Input := H₀ || LE32(0) || LE32(lane_index)
   --    Compute_H_Prime(1024, Input, Block)
   --
   --  **Source**: RFC 9106 Section 3.3

   procedure Compute_H_Prime (
      Output_Length : Output_Length_Type;
      Input         : Byte_Array;
      Output        : out Byte_Array
   ) with
      Global => null,
      Pre    => Output'Length = Output_Length and
                Output'First = 1 and  -- Needed for overflow proofs
                Input'Length <= 1024,  -- Support both initialization and finalization
      Post   => Output'Length = Output_Length;

end SparkPass.Crypto.Argon2id.HPrime;
