with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SparkPass.Crypto.ReedSolomon;
with SparkPass.Crypto.Zeroize;
procedure Test_Rs_Smoke is
   package RS renames SparkPass.Crypto.ReedSolomon;
   Data      : RS.Data_Block;
   Parity    : RS.Parity_Block;
   Codeword  : RS.Codeword_Block;
   Corrupted : RS.Codeword_Block;
   Success   : Boolean := False;
   Has_Err   : Boolean := False;
   Count     : Natural := 0;
   Status    : RS.Decode_Status;
begin
   for I in Data'Range loop
      Data(I) := U8 ((I * 13) mod 256);
   end loop;
   RS.Encode (Data, Parity, Success);
   Put_Line ("encode success=" & Boolean'Image(Success));
   for I in Data'Range loop
      Codeword(I) := Data(I);
   end loop;
   for I in Parity'Range loop
      Codeword(Data'Length + I) := Parity(I);
   end loop;
   RS.Compute_Syndromes(Codeword, Has_Err);
   Put_Line ("syndrome errors?=" & Boolean'Image(Has_Err));
   Corrupted := Codeword;
   for I in 1 .. 8 loop
      Corrupted (I * 10) := Corrupted (I * 10) xor 16#FF#;
   end loop;
   RS.Decode (Corrupted, Count, Status);
   Put_Line ("decode status=" & RS.Decode_Status'Image(Status) & ", corrected=" & Natural'Image(Count));
   Put_Line ("roundtrip OK?=" & Boolean'Image(SparkPass.Crypto.Zeroize.Equal (Corrupted, Codeword)));
end Test_Rs_Smoke;
