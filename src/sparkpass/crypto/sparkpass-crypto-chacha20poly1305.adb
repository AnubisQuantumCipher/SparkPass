pragma SPARK_Mode (On);
with Interfaces; use Interfaces;
with SPARKNaCl;
with SPARKNaCl.Core;
with SPARKNaCl.Secretbox;
with SparkPass.Crypto.Zeroize;

package body SparkPass.Crypto.ChaCha20Poly1305 is

   --  Convert SparkPass Key_Array (1-indexed) to SPARKNaCl Bytes_32 (0-indexed)
   function To_SPARKNaCl_Bytes_32 (Key : Key_Array) return SPARKNaCl.Bytes_32
     with Global => null
   is
      Result : SPARKNaCl.Bytes_32 := (others => 0);
   begin
      for I in Key'Range loop
         pragma Loop_Invariant (I in Key'Range);
         Result (SPARKNaCl.N32 (I - Key'First)) := Key (I);
      end loop;
      return Result;
   end To_SPARKNaCl_Bytes_32;

   --  Convert SparkPass Nonce_Array (1-indexed) to SPARKNaCl Bytes_12 (0-indexed)
   function To_SPARKNaCl_Bytes_12 (Nonce : Nonce_Array) return SPARKNaCl.Bytes_12
     with Global => null
   is
      Result : SPARKNaCl.Bytes_12 := (others => 0);
   begin
      for I in Nonce'Range loop
         pragma Loop_Invariant (I in Nonce'Range);
         Result (SPARKNaCl.N32 (I - Nonce'First)) := Nonce (I);
      end loop;
      return Result;
   end To_SPARKNaCl_Bytes_12;

   --  Convert SparkPass Byte_Array (flexible index) to SPARKNaCl Byte_Seq (0-indexed)
   function To_SPARKNaCl_Byte_Seq (Data : Byte_Array) return SPARKNaCl.Byte_Seq
     with Global => null,
          Pre  => Data'Length <= SparkPass.Config.Max_Data_Length,
          Post => To_SPARKNaCl_Byte_Seq'Result'First = 0 and then
                  To_SPARKNaCl_Byte_Seq'Result'Length = Data'Length
   is
   begin
      if Data'Length = 0 then
         --  Return empty array for empty input (avoiding invalid range 0..-1)
         declare
            Empty : constant SPARKNaCl.Byte_Seq (1 .. 0) := (others => 0);
         begin
            return Empty;
         end;
      else
         declare
            Result : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (Data'Length - 1)) := (others => 0);
         begin
            for I in Data'Range loop
               pragma Loop_Invariant (I in Data'Range);
               pragma Loop_Invariant (SPARKNaCl.N32 (I - Data'First) in Result'Range);
               Result (SPARKNaCl.N32 (I - Data'First)) := Data (I);
            end loop;
            return Result;
         end;
      end if;
   end To_SPARKNaCl_Byte_Seq;

   --  Convert SPARKNaCl Bytes_16 (0-indexed) to SparkPass Tag_Array (1-indexed)
   procedure From_SPARKNaCl_Bytes_16 (Source : SPARKNaCl.Bytes_16; Target : out Tag_Array)
     with Global => null,
          Pre => Source'First = 0 and then Source'Last = 15
   is
   begin
      for I in Target'Range loop
         pragma Loop_Invariant (I in Target'Range);
         Target (I) := Source (SPARKNaCl.N32 (I - Target'First));
      end loop;
   end From_SPARKNaCl_Bytes_16;

   --  Convert SPARKNaCl Byte_Seq (0-indexed) to SparkPass Byte_Array (flexible index)
   procedure From_SPARKNaCl_Byte_Seq (Source : SPARKNaCl.Byte_Seq; Target : out Byte_Array)
     with Global => null,
          Pre => Source'First = 0 and then
                 Source'Length = Target'Length and then
                 Target'Length <= SparkPass.Config.Max_Data_Length
   is
   begin
      for I in Target'Range loop
         pragma Loop_Invariant (I in Target'Range);
         pragma Loop_Invariant (SPARKNaCl.N32 (I - Target'First) in Source'Range);
         Target (I) := Source (SPARKNaCl.N32 (I - Target'First));
      end loop;
   end From_SPARKNaCl_Byte_Seq;

   procedure Seal
     (Key        : in  Key_Array;
      Nonce      : in  Nonce_Array;
      Plaintext  : in  Byte_Array;
      AAD        : in  Byte_Array;
      Ciphertext : out Byte_Array;
      Tag        : out Tag_Array)
   is
      --  Convert to SPARKNaCl types (0-indexed)
      SNaCl_Key     : constant SPARKNaCl.Bytes_32 := To_SPARKNaCl_Bytes_32 (Key);
      SNaCl_Nonce   : constant SPARKNaCl.Bytes_12 := To_SPARKNaCl_Bytes_12 (Nonce);
      SNaCl_Plain   : constant SPARKNaCl.Byte_Seq := To_SPARKNaCl_Byte_Seq (Plaintext);
      SNaCl_AAD     : constant SPARKNaCl.Byte_Seq := To_SPARKNaCl_Byte_Seq (AAD);

      --  ChaCha20 key (limited private type)
      ChaCha_Key    : SPARKNaCl.Core.ChaCha20_Key;

      --  Output buffers (0-indexed, handle empty plaintext)
      SNaCl_Cipher  : SPARKNaCl.Byte_Seq (0 .. (if Plaintext'Length > 0 then SPARKNaCl.N32 (Plaintext'Length - 1) else 0));
      SNaCl_Tag     : SPARKNaCl.Bytes_16;
   begin
      --  Construct ChaCha20 key from raw bytes
      SPARKNaCl.Core.Construct (K => ChaCha_Key, X => SNaCl_Key);

      --  Call SPARKNaCl AEAD encryption (RFC 8439)
      SPARKNaCl.Secretbox.Create
        (C   => SNaCl_Cipher,
         Tag => SNaCl_Tag,
         M   => SNaCl_Plain,
         N   => SNaCl_Nonce,
         K   => ChaCha_Key,
         AAD => SNaCl_AAD);

      --  Convert outputs back to SparkPass types (1-indexed)
      From_SPARKNaCl_Byte_Seq (SNaCl_Cipher, Ciphertext);
      From_SPARKNaCl_Bytes_16 (SNaCl_Tag, Tag);

      --  Sanitize sensitive data
      SPARKNaCl.Core.Sanitize (ChaCha_Key);
      SPARKNaCl.Sanitize (SNaCl_Cipher);
      SPARKNaCl.Sanitize (SNaCl_Tag);
   end Seal;

   procedure Open
     (Key        : in  Key_Array;
      Nonce      : in  Nonce_Array;
      Ciphertext : in  Byte_Array;
      AAD        : in  Byte_Array;
      Tag        : in  Tag_Array;
      Plaintext  : out Byte_Array;
      Success    : out Boolean)
   is
      --  Convert to SPARKNaCl types (0-indexed)
      SNaCl_Key     : constant SPARKNaCl.Bytes_32 := To_SPARKNaCl_Bytes_32 (Key);
      SNaCl_Nonce   : constant SPARKNaCl.Bytes_12 := To_SPARKNaCl_Bytes_12 (Nonce);
      SNaCl_Cipher  : constant SPARKNaCl.Byte_Seq := To_SPARKNaCl_Byte_Seq (Ciphertext);
      SNaCl_AAD     : constant SPARKNaCl.Byte_Seq := To_SPARKNaCl_Byte_Seq (AAD);
      SNaCl_Tag     : SPARKNaCl.Bytes_16 := (others => 0);

      --  ChaCha20 key (limited private type)
      ChaCha_Key    : SPARKNaCl.Core.ChaCha20_Key;

      --  Output buffer (0-indexed, handle empty ciphertext)
      SNaCl_Plain   : SPARKNaCl.Byte_Seq (0 .. (if Ciphertext'Length > 0 then SPARKNaCl.N32 (Ciphertext'Length - 1) else 0));
   begin
      --  Convert Tag to SPARKNaCl format
      for I in Tag'Range loop
         pragma Loop_Invariant (I in Tag'Range);
         SNaCl_Tag (SPARKNaCl.N32 (I - Tag'First)) := Tag (I);
      end loop;

      --  Construct ChaCha20 key from raw bytes
      SPARKNaCl.Core.Construct (K => ChaCha_Key, X => SNaCl_Key);

      --  Call SPARKNaCl AEAD decryption (RFC 8439)
      SPARKNaCl.Secretbox.Open
        (M      => SNaCl_Plain,
         Status => Success,
         Tag    => SNaCl_Tag,
         C      => SNaCl_Cipher,
         N      => SNaCl_Nonce,
         K      => ChaCha_Key,
         AAD    => SNaCl_AAD);

      if Success then
         --  Convert output back to SparkPass type (1-indexed)
         From_SPARKNaCl_Byte_Seq (SNaCl_Plain, Plaintext);
      else
         --  Zero plaintext on authentication failure
         Zeroize.Wipe (Plaintext);
      end if;

      --  Sanitize sensitive data
      SPARKNaCl.Core.Sanitize (ChaCha_Key);
      SPARKNaCl.Sanitize (SNaCl_Plain);
   end Open;

end SparkPass.Crypto.ChaCha20Poly1305;
