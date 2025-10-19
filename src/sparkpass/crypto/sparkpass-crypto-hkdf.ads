pragma SPARK_Mode (On);
with SparkPass.Types; use SparkPass.Types;

package SparkPass.Crypto.HKDF is
   --  SHA-512 produces 64-byte output
   Hash_Output_Length : constant Positive := 64;
   subtype Hash64_Array is Byte_Array (1 .. Hash_Output_Length);

   --  HMAC-SHA512 function for general use
   --  Computes HMAC-SHA512 using pure SPARK implementation (RFC 2104)
   --  Key can be any length (will be hashed if > 136 bytes per SHA3-512 block size)
   --  Data can be any length
   function HMAC (Key : Byte_Array; Data : Byte_Array) return Hash64_Array
     with
       Global => null,
       Pre    => Key'Length > 0 and then
                 Key'Length <= 65536 and then
                 Data'Length <= 1048576,  -- 1 MB max data size
       Post   => HMAC'Result'Length = Hash_Output_Length;

   --  HKDF-SHA384 Key Derivation Function (RFC 5869)
   --  Derives cryptographic key material from input keying material (IKM)
   --  Uses SHA-384 (48-byte) internally, truncating SHA-512 output
   --  IKM: Input keying material (master secret)
   --  Salt: Optional salt value (use empty array if none)
   --  Info: Optional context/application-specific information
   --  Length: Desired output length in bytes
   function Derive
     (IKM    : Byte_Array;
      Salt   : Byte_Array;
      Info   : Byte_Array;
      Length : Positive) return Byte_Array
     with
       Global => null,
       Pre    => IKM'Length > 0 and then
                 IKM'Length <= 8192 and then
                 Salt'Length > 0 and then  -- Salt must not be empty for security
                 Salt'Length <= 256 and then
                 Info'Length <= 256 and then
                 Length > 0 and then
                 Length <= 8192 and then
                 Length <= 255 * 48,  -- HKDF-SHA384 max output: 255 * hash_length
       Post   => Derive'Result'Length = Length and then
                 Derive'Result'First = 1 and then
                 Derive'Result'Last = Length;
end SparkPass.Crypto.HKDF;
