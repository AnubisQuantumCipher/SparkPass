pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_32; use type Interfaces.Unsigned_8;

package Spark_Argon2id is
   -- Basic unsigned types (aliases for clarity)
   subtype U8  is Interfaces.Unsigned_8;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;

   -- Byte arrays and fixed-size crypto arrays
   type Byte_Array is array (Positive range <>) of U8;
   subtype Salt_Array is Byte_Array (1 .. 32);
   subtype Key_Array  is Byte_Array (1 .. 32);

   -- Algorithm parameters (RFC 9106 Section 3.2)
   Argon2_Memory_KiB  : constant Interfaces.Unsigned_32 := 1_048_576; -- 1 GiB
   Argon2_Iterations  : constant Interfaces.Unsigned_32 := 4;         -- passes
   Argon2_Parallelism : constant Interfaces.Unsigned_32 := 2;         -- lanes

   -- Verification preset selection (for SPARK proof bounds)
   type Argon2_Verification_Preset is (Test_Small, Test_Medium, Production);
   Argon2_Verification_Mode : constant Argon2_Verification_Preset := Test_Small;

   -- Public parameter set for KDF
   type Parameters is record
      Memory_Cost : Interfaces.Unsigned_32 := Argon2_Memory_KiB;
      Iterations  : Interfaces.Unsigned_32 := Argon2_Iterations;
      Parallelism : Interfaces.Unsigned_32 := Argon2_Parallelism;
      Salt        : Salt_Array := [others => 0];
   end record;

   procedure Derive
     (Password : Byte_Array;
      Params   : Parameters;
      Output   : out Key_Array;
      Success  : out Boolean)
     with
       Global  => null,
       Pre     => Password'Length > 0 and then
                  Password'Length <= 128 and then  -- Reasonable maximum
                  Params.Parallelism > 0 and then
                  Params.Parallelism <= 255 and then
                  Params.Iterations > 0 and then
                  Params.Iterations <= 255 and then
                  Params.Memory_Cost > 0 and then
                  Params.Memory_Cost <= Interfaces.Unsigned_32 (Positive'Last),
       Depends => (Output => (Password, Params),
                   Success => null),  -- Always True (no error conditions in current impl)
       Post    => (if not Success then (for all I in Output'Range => Output (I) = 0));

   -- Extended API with K, X, variable salt and output length
   procedure Derive_Ex
     (Password        : Byte_Array;
      Salt            : Byte_Array;
      Key             : Byte_Array;        -- Secret parameter K (may be empty)
      Associated_Data : Byte_Array;        -- Associated data X (may be empty)
      Output          : out Byte_Array;    -- Caller-provided length (1..1024)
      Memory_Cost     : Interfaces.Unsigned_32;
      Iterations      : Interfaces.Unsigned_32;
      Parallelism_Requested     : Interfaces.Unsigned_32;
      Success         : out Boolean)
     with
       Global  => null,
       Pre     => Password'Length > 0 and Password'Length <= 128 and
                  Salt'Length in 8 .. 64 and
                  Key'Length <= 64 and
                  Associated_Data'Length <= 4096 and
                  Output'Length in 1 .. 4096 and Output'First = 1 and
                  Parallelism_Requested in 1 .. 255 and
                  Iterations in 1 .. 255 and
                  Memory_Cost > 0,
       Depends => (Output => (Output, Password, Salt, Key, Associated_Data, Memory_Cost, Iterations, Parallelism_Requested),
                   Success => null),
       Post    => (if not Success then (for all I in Output'Range => Output (I) = 0));

end Spark_Argon2id;
