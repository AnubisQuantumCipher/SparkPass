pragma SPARK_Mode (On);
with Interfaces; use type Interfaces.Unsigned_8; use type Interfaces.Unsigned_32;
with SparkPass.Types; use SparkPass.Types;

--  Shamir Secret Sharing Scheme (k-of-n threshold)
--
--  Security properties:
--    1. Root Key (32 bytes) is split into n shares
--    2. Any k shares can reconstruct the Root Key
--    3. Fewer than k shares reveal no information about the Root Key
--    4. GF(256) arithmetic ensures perfect secrecy
--    5. Constant-time operations prevent side-channel attacks
--
--  Implementation:
--    - Polynomial evaluation in GF(256) with irreducible polynomial
--    - Each share is (x, P(x)) where P is random polynomial of degree k-1
--    - P(0) = Root Key byte, P(x_i) = share_i byte
--    - Reconstruction uses Lagrange interpolation
--
--  Usage:
--    Split:  Root_Key -> (Share_1, Share_2, ..., Share_N)
--    Combine: Any k shares -> Root_Key
--
--  Supported configurations: 2-of-3, 3-of-5, 2-of-2, etc.
package SparkPass.Crypto.Shamir is

   --  Maximum number of shares supported (limited by GF(256) field size)
   Max_Shares : constant Positive := 255;

   --  Share structure: x-coordinate + y-coordinate (32 bytes each byte split)
   --  Each share is 33 bytes: 1 byte x-coordinate + 32 bytes y-coordinates
   Share_Size : constant Positive := 33;
   subtype Share_Array is Byte_Array (1 .. Share_Size);

   --  Threshold and total share counts
   subtype Share_Count is Positive range 1 .. Max_Shares;

   --  Array of shares for storage
   type Share_Set is array (Share_Count range <>) of Share_Array;

   --  Split Root Key into k-of-n shares
   --
   --  Threshold: minimum number of shares required to reconstruct (k)
   --  Total_Shares: total number of shares to generate (n)
   --  Root_Key: 32-byte secret to split
   --  Shares: output array of n shares, each 33 bytes
   --
   --  Pre: k <= n, Root_Key is 32 bytes
   --  Post: Success -> Shares contains n valid shares
   --        Failure -> Shares is zeroed
   procedure Split
     (Root_Key     : in  Key_Array;
      Threshold    : in  Share_Count;
      Total_Shares : in  Share_Count;
      Shares       : out Share_Set;
      Success      : out Boolean)
   with
     Global  => null,
     Pre     => Root_Key'Length = 32 and then
                Threshold <= Total_Shares and then
                Shares'Length = Total_Shares and then
                Shares'First = 1,
     Post    => (if Success then
                   (for all I in Shares'Range =>
                      (Shares (I)'Length = Share_Size and then
                       Shares (I)(1) = U8 (I)))
                 else
                   (for all I in Shares'Range =>
                      (for all J in Shares (I)'Range =>
                         Shares (I)(J) = 0)));

   --  Reconstruct Root Key from k shares
   --
   --  Shares: array of k shares (exactly Threshold count)
   --  Threshold: minimum number of shares required (k)
   --  Root_Key: reconstructed 32-byte secret
   --
   --  Pre: Shares'Length >= Threshold, each share is 33 bytes
   --  Post: Success -> Root_Key is reconstructed secret
   --        Failure -> Root_Key is zeroed
   procedure Combine
     (Shares    : in  Share_Set;
      Threshold : in  Share_Count;
      Root_Key  : out Key_Array;
      Success   : out Boolean)
   with
     Global  => null,
     Pre     => Shares'Length >= Threshold and then
                Root_Key'Length = 32 and then
                Shares'First = 1 and then
                (for all I in Shares'Range =>
                   Shares (I)'Length = Share_Size),
     Post    => (if not Success then
                   (for all I in Root_Key'Range =>
                      Root_Key (I) = 0));

   --  Validate share structure
   --
   --  Checks that share has valid x-coordinate (1..255) and correct size
   function Is_Valid_Share (Share : Share_Array) return Boolean
   with
     Global  => null,
     Pre     => Share'Length = Share_Size,
     Post    => Is_Valid_Share'Result =
                  (Share'Length = Share_Size and then
                   Share (Share'First) > 0);

   --  Zeroize share data
   procedure Wipe_Share (Share : in out Share_Array)
   with
     Global  => null,
     Post    => (for all I in Share'Range => Share (I) = 0);

   --  Zeroize all shares in set
   procedure Wipe_Share_Set (Shares : in out Share_Set)
   with
     Global  => null,
     Post    => (for all I in Shares'Range =>
                   (for all J in Shares (I)'Range =>
                      Shares (I)(J) = 0));

end SparkPass.Crypto.Shamir;
