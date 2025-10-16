pragma SPARK_Mode (Off);
--  NIST KAT Test Vectors for ML-KEM-1024
--  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLKEM/kat_MLKEM_1024.rsp

package NIST_MLKEM_Vectors is

   --  Test Vector 1 (count=0) - using first 100 bytes of each for brevity
   --  Full vectors would be too large to embed directly
   PK_1_First_100 : constant String :=
      "a8122f376b3f5d355263eba522c43995044beca78b9ba9924b87035490ae78a9" &
      "4d523c7ba588a386eb49bf652fdf12716373187a1ca047cc6138627aaeab8f6c" &
      "ebbf87a176b86c9f7f093c8f5b0bdc14";

   SK_1_First_100 : constant String :=
      "4b8ac400218a249150aff2339367484a6bcbf637646cf750156a7b35a936d05a" &
      "25647a3368e4891a030beba99ff9d550ca830ebf9b4aae231fc2dc4073aa9841" &
      "1a188bc2442c5349f20140c5a3780376";

   CT_1_First_100 : constant String :=
      "707d18cabcf89670c80003b47d2b8678ad0da4aaad781a3351e82ac3e447e701" &
      "9af5cb86020fb2ac727e79654155835b45de9b8af5e5c7efa01295da8988131d" &
      "c8d6edb0645c3e4116aab080c5a571e7";

   SS_1 : constant String :=
      "23f211b84a6ee20c8c29f6e5314c91b414e940513d380add17bd724ab3a13a52";

   --  Test Vector 2 (count=2)
   SS_2 : constant String :=
      "cf62e302e088285819ba2797f48af57db3a1c5f656a0f8de826e0f088e57c4e7";

   --  Test Vector 3 (count=4)
   SS_3 : constant String :=
      "a415b8dc7061fb5f1ba8aebbe3b3a32c14e9bfc4f8e2f079f893e685b44ea95c";

end NIST_MLKEM_Vectors;