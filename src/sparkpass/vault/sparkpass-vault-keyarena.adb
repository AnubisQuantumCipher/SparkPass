pragma SPARK_Mode (Off);  -- Calls Wrapping module which uses FFI
with SparkPass.Crypto.Zeroize;

package body SparkPass.Vault.KeyArena is

   --  Helper: Write wrapped key to buffer
   procedure Write_Wrapped_Key
     (Wrapped : in     SparkPass.Crypto.Wrapping.Wrapped_Key;
      Present : in     Boolean;
      Buffer  : in out Byte_Array;
      Offset  : in out Positive)
   with
     Global => null,
     Pre    => Offset >= Buffer'First and then
               Offset + 60 <= Buffer'Last + 1
   is
      Serialized : SparkPass.Crypto.Wrapping.Wrapped_Key_Array := (others => 0);
      Serialize_Success : Boolean := False;
   begin
      --  Write present flag
      Buffer (Offset) := (if Present then 1 else 0);
      Offset := Offset + 1;

      if Present then
         --  Serialize wrapped key
         SparkPass.Crypto.Wrapping.Serialize_Wrapped_Key
           (Wrapped => Wrapped,
            Buffer  => Serialized,
            Success => Serialize_Success);

         if Serialize_Success then
            for I in Serialized'Range loop
               Buffer (Offset) := Serialized (I);
               Offset := Offset + 1;
            end loop;
         else
            --  Failed to serialize, write zeros
            for I in 1 .. 60 loop
               Buffer (Offset) := 0;
               Offset := Offset + 1;
            end loop;
         end if;
      else
         --  Not present, write padding
         for I in 1 .. 60 loop
            Buffer (Offset) := 0;
            Offset := Offset + 1;
         end loop;
      end if;

      SparkPass.Crypto.Zeroize.Wipe (Serialized);
   end Write_Wrapped_Key;

   --  Helper: Read wrapped key from buffer
   procedure Read_Wrapped_Key
     (Buffer  : in     Byte_Array;
      Offset  : in out Positive;
      Present : out    Boolean;
      Wrapped : out    SparkPass.Crypto.Wrapping.Wrapped_Key;
      Success : out    Boolean)
   with
     Global => null,
     Pre    => Offset >= Buffer'First and then
               Offset + 60 <= Buffer'Last + 1
   is
      Serialized : SparkPass.Crypto.Wrapping.Wrapped_Key_Array := (others => 0);
      Deserialize_Success : Boolean := False;
   begin
      Present := False;
      Success := False;
      Wrapped.Present := False;

      --  Read present flag
      Present := (Buffer (Offset) /= 0);
      Offset := Offset + 1;

      if Present then
         --  Read serialized wrapped key
         for I in Serialized'Range loop
            Serialized (I) := Buffer (Offset);
            Offset := Offset + 1;
         end loop;

         --  Deserialize
         SparkPass.Crypto.Wrapping.Deserialize_Wrapped_Key
           (Buffer  => Serialized,
            Wrapped => Wrapped,
            Success => Deserialize_Success);

         if not Deserialize_Success then
            SparkPass.Crypto.Zeroize.Wipe (Serialized);
            return;
         end if;
      else
         --  Skip padding
         for I in 1 .. 60 loop
            Offset := Offset + 1;
         end loop;
      end if;

      SparkPass.Crypto.Zeroize.Wipe (Serialized);
      Success := True;
   end Read_Wrapped_Key;

   procedure Serialize
     (Arena       : in     Key_Arena;
      Buffer      : out    Byte_Array;
      Actual_Size : out    Natural;
      Status      : out    Parse_Status)
   is
      Offset : Positive := Buffer'First;
   begin
      Status := Ok;
      Actual_Size := 0;
      Buffer := (others => 0);

      --  Validate policy before serialization
      if not Is_Valid_Policy (Arena) then
         Status := Policy_Violation;
         return;
      end if;

      --  Write magic
      for I in KeyArena_Magic'Range loop
         Buffer (Offset) := U8 (Character'Pos (KeyArena_Magic (I)));
         Offset := Offset + 1;
      end loop;

      --  Write Wrap A (required)
      Write_Wrapped_Key (Arena.Wrap_A, Arena.Wrap_A_Present, Buffer, Offset);

      --  Write Wrap B (optional)
      Write_Wrapped_Key (Arena.Wrap_B, Arena.Wrap_B_Present, Buffer, Offset);

      --  Write Shamir header
      Buffer (Offset) := U8 (Arena.Shamir_Total_Shares);
      Offset := Offset + 1;
      Buffer (Offset) := U8 (Arena.Shamir_Threshold);
      Offset := Offset + 1;

      --  Write Shamir shares (if present)
      if Arena.Shamir_Total_Shares > 0 then
         for I in 1 .. Arena.Shamir_Total_Shares loop
            declare
               Share : constant SparkPass.Crypto.Wrapping.Sealed_Share :=
                 Arena.Shamir_Shares (I);
            begin
               --  Write share data (33 bytes)
               for J in Share.Share_Data'Range loop
                  Buffer (Offset) := Share.Share_Data (J);
                  Offset := Offset + 1;
               end loop;

               --  Write nonce (12 bytes)
               for J in Share.Nonce'Range loop
                  Buffer (Offset) := Share.Nonce (J);
                  Offset := Offset + 1;
               end loop;

               --  Write tag (16 bytes)
               for J in Share.Tag'Range loop
                  Buffer (Offset) := Share.Tag (J);
                  Offset := Offset + 1;
               end loop;
            end;
         end loop;
      end if;

      --  Write Wrap D (optional)
      Write_Wrapped_Key (Arena.Wrap_D, Arena.Wrap_D_Present, Buffer, Offset);

      Actual_Size := Offset - Buffer'First;
   end Serialize;

   procedure Deserialize
     (Buffer : in     Byte_Array;
      Arena  : out    Key_Arena;
      Status : out    Parse_Status)
   is
      Offset : Positive := Buffer'First;
      Magic_Buffer : String (1 .. 4) := "    ";
      Read_Success : Boolean := False;
   begin
      Status := Ok;
      Arena.Wrap_A_Present := False;
      Arena.Wrap_B_Present := False;
      Arena.Wrap_D_Present := False;
      Arena.Shamir_Threshold := 0;
      Arena.Shamir_Total_Shares := 0;

      --  Minimum size check: magic + 3 wraps + shamir header = 4 + 61*3 + 2 = 189 bytes
      if Buffer'Length < 189 then
         Status := Invalid_Size;
         return;
      end if;

      --  Read and validate magic
      for I in Magic_Buffer'Range loop
         Magic_Buffer (I) := Character'Val (Integer (Buffer (Offset)));
         Offset := Offset + 1;
      end loop;

      if Magic_Buffer /= KeyArena_Magic then
         Status := Invalid_Magic;
         return;
      end if;

      --  Read Wrap A (required)
      Read_Wrapped_Key (Buffer, Offset, Arena.Wrap_A_Present, Arena.Wrap_A, Read_Success);
      if not Read_Success then
         Status := Invalid_Wrap_Data;
         Wipe_Arena (Arena);
         return;
      end if;

      if not Arena.Wrap_A_Present then
         Status := Missing_Required_Wrap;
         Wipe_Arena (Arena);
         return;
      end if;

      --  Read Wrap B (optional)
      Read_Wrapped_Key (Buffer, Offset, Arena.Wrap_B_Present, Arena.Wrap_B, Read_Success);
      if not Read_Success then
         Status := Invalid_Wrap_Data;
         Wipe_Arena (Arena);
         return;
      end if;

      --  Read Shamir header
      Arena.Shamir_Total_Shares := Natural (Buffer (Offset));
      Offset := Offset + 1;
      Arena.Shamir_Threshold := Natural (Buffer (Offset));
      Offset := Offset + 1;

      --  Validate Shamir configuration
      if Arena.Shamir_Total_Shares > 10 or else
         (Arena.Shamir_Total_Shares > 0 and then
          (Arena.Shamir_Threshold = 0 or else
           Arena.Shamir_Threshold > Arena.Shamir_Total_Shares))
      then
         Status := Invalid_Wrap_Data;
         Wipe_Arena (Arena);
         return;
      end if;

      --  Read Shamir shares (if present)
      if Arena.Shamir_Total_Shares > 0 then
         --  Check buffer size for shares
         if Offset + (Arena.Shamir_Total_Shares * 61) > Buffer'Last then
            Status := Invalid_Size;
            Wipe_Arena (Arena);
            return;
         end if;

         for I in 1 .. Arena.Shamir_Total_Shares loop
            --  Read share data (33 bytes)
            for J in Arena.Shamir_Shares (I).Share_Data'Range loop
               Arena.Shamir_Shares (I).Share_Data (J) := Buffer (Offset);
               Offset := Offset + 1;
            end loop;

            --  Read nonce (12 bytes)
            for J in Arena.Shamir_Shares (I).Nonce'Range loop
               Arena.Shamir_Shares (I).Nonce (J) := Buffer (Offset);
               Offset := Offset + 1;
            end loop;

            --  Read tag (16 bytes)
            for J in Arena.Shamir_Shares (I).Tag'Range loop
               Arena.Shamir_Shares (I).Tag (J) := Buffer (Offset);
               Offset := Offset + 1;
            end loop;
         end loop;
      end if;

      --  Read Wrap D (optional)
      if Offset + 60 > Buffer'Last then
         Status := Invalid_Size;
         Wipe_Arena (Arena);
         return;
      end if;

      Read_Wrapped_Key (Buffer, Offset, Arena.Wrap_D_Present, Arena.Wrap_D, Read_Success);
      if not Read_Success then
         Status := Invalid_Wrap_Data;
         Wipe_Arena (Arena);
         return;
      end if;

      --  Validate policy
      if not Is_Valid_Policy (Arena) then
         Status := Policy_Violation;
         Wipe_Arena (Arena);
         return;
      end if;

      Status := Ok;
   end Deserialize;

   function Is_Valid_Policy (Arena : Key_Arena) return Boolean is
   begin
      --  Rule 1: Wrap A (passphrase) is always required
      if not Arena.Wrap_A_Present then
         return False;
      end if;

      --  Rule 2: Wrap D (Touch ID) requires Wrap A (always true since Wrap A is required)
      --  This check is redundant but explicit for clarity
      if Arena.Wrap_D_Present and then not Arena.Wrap_A_Present then
         return False;
      end if;

      --  Rule 3: Shamir configuration must be valid
      if Arena.Shamir_Total_Shares > 0 then
         if Arena.Shamir_Threshold = 0 or else
            Arena.Shamir_Threshold > Arena.Shamir_Total_Shares or else
            Arena.Shamir_Total_Shares > 10
         then
            return False;
         end if;
      end if;

      return True;
   end Is_Valid_Policy;

   procedure Wipe_Arena (Arena : in out Key_Arena) is
   begin
      Arena.Wrap_A_Present := False;
      SparkPass.Crypto.Wrapping.Wipe_Wrapped_Key (Arena.Wrap_A);

      Arena.Wrap_B_Present := False;
      SparkPass.Crypto.Wrapping.Wipe_Wrapped_Key (Arena.Wrap_B);

      Arena.Shamir_Threshold := 0;
      Arena.Shamir_Total_Shares := 0;
      for I in Arena.Shamir_Shares'Range loop
         SparkPass.Crypto.Wrapping.Wipe_Sealed_Share (Arena.Shamir_Shares (I));
      end loop;

      Arena.Wrap_D_Present := False;
      SparkPass.Crypto.Wrapping.Wipe_Wrapped_Key (Arena.Wrap_D);
   end Wipe_Arena;

end SparkPass.Vault.KeyArena;
