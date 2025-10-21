pragma SPARK_Mode (On);

with Spark_Argon2id.Zeroize;
with Spark_Argon2id.Internal_Types;     use Spark_Argon2id.Internal_Types;
with Spark_Argon2id.H0;        use Spark_Argon2id.H0;
with Spark_Argon2id.Init;      use Spark_Argon2id.Init;
with Spark_Argon2id.Fill;      use Spark_Argon2id.Fill;
with Spark_Argon2id.Finalize;

package body Spark_Argon2id is

   ------------------------------------------------------------
   --  Derive (Complete Implementation - Phase 2.8)
   ------------------------------------------------------------

   --  Argon2id key derivation function (RFC 9106)
   --
   --  **Algorithm** (RFC 9106 Section 3):
   --    1. H₀ ← Initial hash from password, salt, and parameters
   --    2. Initialize first two blocks per lane from H₀
   --    3. Fill remaining memory using compression function G
   --    4. Iterate over memory t times (4 passes)
   --    5. Extract final hash from last block
   --
   --  Notes:
   --    - Multi‑lane: runtime p (Active_Lanes) ≤ compiled Argon2_Parallelism
   --    - Iterations t = Argon2_Iterations (default 4)
   --    - Memory size follows verification preset for array bounds; H₀ encodes
   --      the requested memory cost, but arrays are sized by preset.
   --    - Default output = 32 bytes for Derive; Derive_Ex supports 1..4096.
   --
   --  **Security Properties**:
   --    - Memory-hard: Resistant to GPU/ASIC attacks
   --    - Side-channel resistant: Data-independent mode in first half of first pass
   --    - Deterministic: Same inputs always produce same output
   --
   --  **Source**: RFC 9106 Section 3

   procedure Derive
     (Password : Byte_Array;
      Params   : Parameters;
      Output   : out Key_Array;
      Success  : out Boolean)
   is
      -- H₀
      H0 : Byte_Array (1 .. 64) := [others => 0];
      -- Memory state
      Memory : Memory_State := [others => [others => Zero_Block]];
      -- Final output buffer (32 bytes)
      Final_Output : Byte_Array (1 .. 32);
   begin
      Output := [others => 0];
      Success := False;

      -- Accept requested lanes but require requested <= configured Parallelism
      declare
         Requested_Lanes : constant Positive := Positive (Integer (Params.Parallelism));
         Empty : constant Byte_Array := [];
      begin
         pragma Assert (Requested_Lanes in 1 .. Parallelism);
         Compute_H0 (
           Password        => Password,
           Salt            => Params.Salt,
           Key             => Empty,
           Associated_Data => Empty,
           Parallelism     => Requested_Lanes,
           Tag_Length      => 32,
           Memory_KiB      => Positive (Params.Memory_Cost),
           Iterations      => Positive (Params.Iterations),
           H0_Out          => H0);
      end;

      -- Initialize first two blocks for each lane
      for L_Int in 0 .. Positive (Integer (Params.Parallelism)) - 1 loop
         declare
            L : constant Lane_Index := Lane_Index (L_Int);
            Init_Blocks : Initial_Blocks;
         begin
            Generate_Initial_Blocks (H0 => H0, Lane => L, Output => Init_Blocks);
            Memory (L, 0) := Init_Blocks.Block_0;
            Memory (L, 1) := Init_Blocks.Block_1;
         end;
      end loop;

      -- Compute effective runtime memory geometry (Active ≤ Capacity)
      declare
         P_Lanes  : constant Positive := Positive (Integer (Params.Parallelism));
         -- Desired blocks per lane from requested memory (1 KiB per block)
         Requested_Total_Blocks : constant Natural := Natural (Params.Memory_Cost);
         Requested_Q : Natural := (if P_Lanes = 0 then 0 else Requested_Total_Blocks / P_Lanes);
         -- Round down to multiple of 4 blocks per lane (segments)
         Requested_Q_Rounded : constant Natural := (Requested_Q / 4) * 4;
         -- Capacity from preset (compile-time)
         Capacity_Q : constant Natural := Active_Blocks_Per_Lane;
         Effective_Q : constant Positive := Positive (Integer (Natural'Min (Requested_Q_Rounded, Capacity_Q)));
         Effective_S : constant Positive := Positive (Integer (Effective_Q / 4));
      begin
         Fill_Memory (
           Memory                      => Memory,
           Active_Lanes                => P_Lanes,
           Effective_Blocks_Per_Lane   => Effective_Q,
           Effective_Blocks_Per_Segment=> Effective_S);
      end;

      -- Finalize
      declare
         P_Lanes  : constant Positive := Positive (Integer (Params.Parallelism));
         Requested_Total_Blocks : constant Natural := Natural (Params.Memory_Cost);
         Requested_Q : Natural := (if P_Lanes = 0 then 0 else Requested_Total_Blocks / P_Lanes);
         Requested_Q_Rounded : constant Natural := (Requested_Q / 4) * 4;
         Capacity_Q : constant Natural := Active_Blocks_Per_Lane;
         Effective_Q : constant Positive := Positive (Integer (Natural'Min (Requested_Q_Rounded, Capacity_Q)));
      begin
         Spark_Argon2id.Finalize.Finalize (
           Memory                    => Memory,
           Active_Lanes              => P_Lanes,
           Effective_Blocks_Per_Lane => Effective_Q,
           Output_Length             => 32,
           Output                    => Final_Output);
      end;

      Output := Final_Output;
      Success := True;

      Spark_Argon2id.Zeroize.Wipe (H0);
      Spark_Argon2id.Zeroize.Wipe (Final_Output);
   exception
      when others =>
         Output := [others => 0];
         Spark_Argon2id.Zeroize.Wipe (H0);
         Spark_Argon2id.Zeroize.Wipe (Final_Output);
         Success := False;
         raise;
   end Derive;

   -- Extended API with K, X and variable output
   procedure Derive_Ex
     (Password        : Byte_Array;
      Salt            : Byte_Array;
      Key             : Byte_Array;
      Associated_Data : Byte_Array;
      Output          : out Byte_Array;
      Memory_Cost     : Interfaces.Unsigned_32;
      Iterations      : Interfaces.Unsigned_32;
      Parallelism_Requested     : Interfaces.Unsigned_32;
      Success         : out Boolean)
   is
      H0 : Byte_Array (1 .. 64) := [others => 0];
      Memory : Memory_State := [others => [others => Zero_Block]];
   begin
      Output := [others => 0];
      Success := False;

      declare
         Requested_Lanes : constant Positive := Positive (Integer (Parallelism_Requested));
      begin
         pragma Assert (Requested_Lanes in 1 .. Parallelism);
         Compute_H0 (
           Password        => Password,
           Salt            => Salt,
           Key             => Key,
           Associated_Data => Associated_Data,
           Parallelism     => Requested_Lanes,
           Tag_Length      => Output'Length,
           Memory_KiB      => Positive (Memory_Cost),
           Iterations      => Positive (Iterations),
           H0_Out          => H0);
      end;

      for L_Int in 0 .. Positive (Integer (Parallelism_Requested)) - 1 loop
         declare
            L : constant Lane_Index := Lane_Index (L_Int);
            Init_Blocks : Initial_Blocks;
         begin
            Generate_Initial_Blocks (H0 => H0, Lane => L, Output => Init_Blocks);
            Memory (L, 0) := Init_Blocks.Block_0;
            Memory (L, 1) := Init_Blocks.Block_1;
         end;
      end loop;

      declare
         P_Lanes  : constant Positive := Positive (Integer (Parallelism_Requested));
         Requested_Total_Blocks : constant Natural := Natural (Memory_Cost);
         Requested_Q : Natural := (if P_Lanes = 0 then 0 else Requested_Total_Blocks / P_Lanes);
         Requested_Q_Rounded : constant Natural := (Requested_Q / 4) * 4;
         Capacity_Q : constant Natural := Active_Blocks_Per_Lane;
         Effective_Q : constant Positive := Positive (Integer (Natural'Min (Requested_Q_Rounded, Capacity_Q)));
         Effective_S : constant Positive := Positive (Integer (Effective_Q / 4));
      begin
         Fill_Memory (
           Memory                      => Memory,
           Active_Lanes                => P_Lanes,
           Effective_Blocks_Per_Lane   => Effective_Q,
           Effective_Blocks_Per_Segment=> Effective_S);
      end;

      declare
         P_Lanes  : constant Positive := Positive (Integer (Parallelism_Requested));
         Requested_Total_Blocks : constant Natural := Natural (Memory_Cost);
         Requested_Q : Natural := (if P_Lanes = 0 then 0 else Requested_Total_Blocks / P_Lanes);
         Requested_Q_Rounded : constant Natural := (Requested_Q / 4) * 4;
         Capacity_Q : constant Natural := Active_Blocks_Per_Lane;
         Effective_Q : constant Positive := Positive (Integer (Natural'Min (Requested_Q_Rounded, Capacity_Q)));
      begin
         Spark_Argon2id.Finalize.Finalize (
           Memory                    => Memory,
           Active_Lanes              => P_Lanes,
           Effective_Blocks_Per_Lane => Effective_Q,
           Output_Length             => Output'Length,
           Output                    => Output);
      end;

      Success := True;

      Spark_Argon2id.Zeroize.Wipe (H0);
   exception
      when others =>
         Output := [others => 0];
         Spark_Argon2id.Zeroize.Wipe (H0);
         Success := False;
         raise;
   end Derive_Ex;

end Spark_Argon2id;
