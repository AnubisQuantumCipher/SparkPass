pragma SPARK_Mode (On);

with SparkPass.Crypto.Zeroize;
with Spark_Argon2id;  -- Upgraded external Argon2id

package body SparkPass.Crypto.Argon2id is

   ------------------------------------------------------------
   --  Derive (Complete Implementation - Phase 2.8)
   ------------------------------------------------------------

   --  Adapter: Delegate to external Spark_Argon2id implementation

   procedure Derive
     (Password : Byte_Array;
      Params   : Parameters;
      Output   : out Key_Array;
      Success  : out Boolean)
   is
      -- Map SparkPass types to Spark_Argon2id types
      SP_Params : Spark_Argon2id.Parameters :=
        (Memory_Cost => Params.Memory_Cost,
         Iterations  => Params.Iterations,
         Parallelism => Params.Parallelism,
         Salt        => (others => 0));
      PW_Copy   : Spark_Argon2id.Byte_Array (1 .. Password'Length);
      Out_Copy  : Spark_Argon2id.Key_Array;
      Ok        : Boolean := False;
   begin
      Output := (others => 0);
      Success := False;

      -- Copy salt into external param (both 32 bytes)
      for I in SP_Params.Salt'Range loop
         SP_Params.Salt(I) := Params.Salt(I);
      end loop;

      -- Copy password bytes
      for I in Password'Range loop
         PW_Copy(I) := Password(I);
      end loop;

      -- Delegate to upgraded implementation
      Spark_Argon2id.Derive(
         Password => PW_Copy,
         Params   => SP_Params,
         Output   => Out_Copy,
         Success  => Ok);

      if Ok then
         for I in Output'Range loop
            Output(I) := Out_Copy(I);
         end loop;
         Success := True;
      end if;
   end Derive;

   ------------------------------------------------------------
   --  Zeroize
   ------------------------------------------------------------

   --  Securely erase key material from memory
   --
   --  Uses cryptographic zeroization (not compiler-optimizable)
   --  via SparkPass.Crypto.Zeroize.Wipe, which calls libsodium's
   --  sodium_memzero with memory barrier.
   --
   --  **Security Property**: Key material is overwritten with zeros
   --  **Side-Channel Resistance**: No data-dependent branches

   procedure Zeroize (Value : in out Key_Array) is
   begin
      SparkPass.Crypto.Zeroize.Wipe (Value);
   end Zeroize;

end SparkPass.Crypto.Argon2id;
