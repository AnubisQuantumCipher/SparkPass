pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_32;
with SparkPass.Config;
with SparkPass.Types; use SparkPass.Types; use type SparkPass.Types.U8;

package SparkPass.Crypto.Argon2id is
   type Parameters is record
      Memory_Cost : Interfaces.Unsigned_32 := SparkPass.Config.Argon2_Memory_KiB;
      Iterations  : Interfaces.Unsigned_32 := SparkPass.Config.Argon2_Iterations;
      Parallelism : Interfaces.Unsigned_32 := SparkPass.Config.Argon2_Parallelism;
      Salt        : Salt_Array := (others => 0);
   end record;

   procedure Derive
     (Password : Byte_Array;
      Params   : Parameters;
      Output   : out Key_Array;
      Success  : out Boolean)
     with
       Global  => null,
       Pre     => Password'Length > 0 and then
                  Password'Length <= 128,  -- Reasonable maximum
       Depends => (Output => (Password, Params), Success => (Password, Params)),
       Post    => (if not Success then (for all I in Output'Range => Output (I) = 0));

   procedure Zeroize (Value : in out Key_Array)
     with Global => null;

end SparkPass.Crypto.Argon2id;
