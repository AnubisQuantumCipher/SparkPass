--  ========================================================================
--  SparkPass HMAC Implementation - Pure SPARK
--  ========================================================================

pragma SPARK_Mode (On);

with Interfaces; use type Interfaces.Unsigned_8;
with SparkPass.Crypto.Keccak;

package body SparkPass.Crypto.HMAC is

   --  SHA3-512 block size (rate) = 136 bytes (1088 bits / 8)
   --  SHA3-256 block size (rate) = 136 bytes (1088 bits / 8)
   Block_Size : constant := 136;

   --  HMAC padding constants (RFC 2104)
   Ipad_Byte : constant U8 := 16#36#;  -- Inner padding
   Opad_Byte : constant U8 := 16#5C#;  -- Outer padding

   subtype Block_Array is Byte_Array(1 .. Block_Size);

   --  =====================================================================
   --  HMAC-SHA512 Implementation
   --  =====================================================================

   procedure HMAC_SHA512 (
      Key     : in Byte_Array;
      Message : in Byte_Array;
      Output  : out HMAC_Output
   ) is
      --  Padded key (hash if > block_size, zero-pad if < block_size)
      Padded_Key : Block_Array := (others => 0);

      --  XORed keys for inner and outer hashing
      Inner_Key  : Block_Array;
      Outer_Key  : Block_Array;

      --  Inner hash result
      Inner_Hash : HMAC_Output;

      --  Buffers for concatenation
      Inner_Message : Byte_Array(1 .. Block_Size + Message'Length);
      Outer_Message : Byte_Array(1 .. Block_Size + 64);
   begin
      --  Step 1: Prepare padded key
      if Key'Length > Block_Size then
         --  Key too long: hash it first
         declare
            Key_Hash : HMAC_Output;
         begin
            Keccak.SHA3_512_Hash(Key, Key_Hash);
            Padded_Key(1 .. 64) := Key_Hash;
            --  Rest already zero-initialized
         end;
      else
         --  Key short enough: copy and zero-pad
         Padded_Key(1 .. Key'Length) := Key;
         --  Rest already zero-initialized
      end if;

      --  Step 2: Create inner and outer keys (XOR with pads)
      for I in Block_Array'Range loop
         pragma Loop_Invariant (I in Block_Array'Range);
         Inner_Key(I) := Padded_Key(I) xor Ipad_Byte;
         Outer_Key(I) := Padded_Key(I) xor Opad_Byte;
      end loop;

      --  Step 3: Compute inner hash: H((K ⊕ ipad) || message)
      Inner_Message(1 .. Block_Size) := Inner_Key;
      Inner_Message(Block_Size + 1 .. Inner_Message'Last) := Message;
      Keccak.SHA3_512_Hash(Inner_Message, Inner_Hash);

      --  Step 4: Compute outer hash: H((K ⊕ opad) || inner_hash)
      Outer_Message(1 .. Block_Size) := Outer_Key;
      Outer_Message(Block_Size + 1 .. Outer_Message'Last) := Inner_Hash;
      Keccak.SHA3_512_Hash(Outer_Message, Output);

      --  Zeroize sensitive data
      Padded_Key := (others => 0);
      Inner_Key  := (others => 0);
      Outer_Key  := (others => 0);
      Inner_Hash := (others => 0);
   end HMAC_SHA512;

   --  =====================================================================
   --  HMAC-SHA256 Implementation
   --  =====================================================================

   procedure HMAC_SHA256 (
      Key     : in Byte_Array;
      Message : in Byte_Array;
      Output  : out HMAC256_Output
   ) is
      --  Padded key (hash if > block_size, zero-pad if < block_size)
      Padded_Key : Block_Array := (others => 0);

      --  XORed keys for inner and outer hashing
      Inner_Key  : Block_Array;
      Outer_Key  : Block_Array;

      --  Inner hash result
      Inner_Hash : HMAC256_Output;

      --  Buffers for concatenation
      Inner_Message : Byte_Array(1 .. Block_Size + Message'Length);
      Outer_Message : Byte_Array(1 .. Block_Size + 32);
   begin
      --  Step 1: Prepare padded key
      if Key'Length > Block_Size then
         --  Key too long: hash it first
         declare
            Key_Hash : HMAC256_Output;
         begin
            Keccak.SHA3_256_Hash(Key, Key_Hash);
            Padded_Key(1 .. 32) := Key_Hash;
            --  Rest already zero-initialized
         end;
      else
         --  Key short enough: copy and zero-pad
         Padded_Key(1 .. Key'Length) := Key;
         --  Rest already zero-initialized
      end if;

      --  Step 2: Create inner and outer keys (XOR with pads)
      for I in Block_Array'Range loop
         pragma Loop_Invariant (I in Block_Array'Range);
         Inner_Key(I) := Padded_Key(I) xor Ipad_Byte;
         Outer_Key(I) := Padded_Key(I) xor Opad_Byte;
      end loop;

      --  Step 3: Compute inner hash: H((K ⊕ ipad) || message)
      Inner_Message(1 .. Block_Size) := Inner_Key;
      Inner_Message(Block_Size + 1 .. Inner_Message'Last) := Message;
      Keccak.SHA3_256_Hash(Inner_Message, Inner_Hash);

      --  Step 4: Compute outer hash: H((K ⊕ opad) || inner_hash)
      Outer_Message(1 .. Block_Size) := Outer_Key;
      Outer_Message(Block_Size + 1 .. Outer_Message'Last) := Inner_Hash;
      Keccak.SHA3_256_Hash(Outer_Message, Output);

      --  Zeroize sensitive data
      Padded_Key := (others => 0);
      Inner_Key  := (others => 0);
      Outer_Key  := (others => 0);
      Inner_Hash := (others => 0);
   end HMAC_SHA256;

end SparkPass.Crypto.HMAC;
