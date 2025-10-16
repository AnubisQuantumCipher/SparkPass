
   --  NIST KAT Test Vector #1 from kat_MLDSA_87_det_pure.rsp (count=0)
   procedure Test_NIST_Vector_1 is
      --  Test vector from official NIST post-quantum-cryptography/KAT repository
      --  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLDSA/kat_MLDSA_87_det_pure.rsp

      PK_Hex : constant String := "bc89b367d4288f47c71a74679d0fcffbe041de41b5da2f5fc66d8e28c5899494046873a6e1dcaf1666faf26b09137934ba22d82bb1423d544c7c1951241f6f6997ad5996798926eb8f4840edb92127bd78293d741a356827911cafc45dba6aa74243fe8f..."  -- truncated for readability
         & "83597630428a86fe2a21e02c36bf9c09f8af40fa2149a2cb09868e275235751cec6e66cfd0cce26b302820c77053731e6e65";
      SK_Hex : constant String := "bc89b367d4288f47c71a74679d0fcffbe041de41b5da2f5fc66d8e28c5899494c1f189ce692781ab4fc4fde69fedac0bd4bdc2720d806aa5d0fdd9c100afcdd13f949bc58e8fbe98580f187d82e0a3333cacb9beab3967e4ec827f12145c0a436483fb81..."  -- truncated for readability
         & "7bae50b0be3dcf4eb984471c9e8101a12788eea4a3e7c91f64119a91398f4f454045e2d843c7a610c895ac48da583593462c";
      Msg_Hex : constant String := "6dbbc4375136df3b07f7c70e639e223e";
      Sig_Hex : constant String := "69afd2627b4cc0a88c900cb944f93e6566de711dcdf862503c8d260462a2d5df543087d7cf22cbdbe6ac11f2e9957ddb8c52d93d575828eb718c2401a997a106f51b2c21aaf0e4060842b96f18528c181cfd16607bb726348af0aa6976ecee40f5c21da0..."  -- truncated for readability
         & "5dd4f32f385c77858d8e9cadb0f1f70141737783a7bd77b4bc00000000000000000000000000000000000d11161d2430373a";

      Public_Key : SparkPass.Crypto.MLDSA.Public_Key;
      Secret_Key : SparkPass.Crypto.MLDSA.Secret_Key;
      Message : Byte_Array := Hex_To_Bytes (Msg_Hex);
      Signature : SparkPass.Crypto.MLDSA.Signature;
      Expected_Sig : SparkPass.Crypto.MLDSA.Signature;
      Generated_Sig : SparkPass.Crypto.MLDSA.Signature;
      Verified : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== NIST Test Vector #1 (count=0) ===");

      --  Parse test vector data
      Public_Key := Hex_To_Bytes (PK_Hex);
      Secret_Key := Hex_To_Bytes (SK_Hex);
      Expected_Sig := Hex_To_Bytes (Sig_Hex);

      --  Test 1: Verify the provided signature
      Put_Line ("[1] Verifying NIST signature...");
      SparkPass.Crypto.MLDSA.Verify (Public_Key, Message, Expected_Sig, Verified);

      if Verified then
         Put_Line ("  ✓ PASS: NIST signature verified successfully");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: NIST signature verification failed");
         Fail_Count := Fail_Count + 1;
      end if;

      --  Test 2: Generate our own signature and verify it
      Put_Line ("[2] Generating and verifying our own signature...");
      SparkPass.Crypto.MLDSA.Sign (Secret_Key, Message, Generated_Sig);
      SparkPass.Crypto.MLDSA.Verify (Public_Key, Message, Generated_Sig, Verified);

      if Verified then
         Put_Line ("  ✓ PASS: Generated signature verified successfully");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Generated signature verification failed");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_NIST_Vector_1;

   --  NIST KAT Test Vector #2 from kat_MLDSA_87_det_pure.rsp (count=2)
   procedure Test_NIST_Vector_2 is
      --  Test vector from official NIST post-quantum-cryptography/KAT repository
      --  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLDSA/kat_MLDSA_87_det_pure.rsp

      PK_Hex : constant String := "6656d235cb3d56f8bacfc69c78dd1b83ef2c7cf63af094da8838bc345d1df410f515b6d437b5ee98440e717e7b7c7d0a8a9e8d416bad600832d0496f71dbb3b2031aed9aaa033a5227346d890741b156216c6e3c680dac37e846b2b9e256daf44ea54536..."  -- truncated for readability
         & "1739bb8a4c41939cb19d48730dcf924626ba629586b01509975c0831e91347f6316795a85e163a374f5509a76988039810ed";
      SK_Hex : constant String := "6656d235cb3d56f8bacfc69c78dd1b83ef2c7cf63af094da8838bc345d1df410630562a56c5031e3779bc884dea50f590fde975387edefda5d44404c505e6902aab91353e752fc9d1d0f38aa4883eccb59ac30a6aff4a4f43f115d062abc0c6b23401459..."  -- truncated for readability
         & "6a18710591ba85f77298b5abd5c64d34aedcd4eb3b61e129e1576fa7613b50c8e9ca4db974522e84886cceee5ff87a03cfa2";
      Msg_Hex : constant String := "63470357110828f25b23edc80ed280ecd398a9f53251c3332754de2af0b15e901a43ac1d7f898991f0e86b404a1e2ab2";
      Sig_Hex : constant String := "634e693d9cdc718e9147827823c21173af7ac45e5783c56fdb5ed5692a6fed7755dd990908d5fbb7b4cbb495c02f90c6e91fc602d8aeb29062856ddfc99669197bd95d8a90679367ca79ca677fcf59fa735a2df7b06f59b76455be0289d1b241e4a06ac8..."  -- truncated for readability
         & "bbc7ea34487796b7bcff1e394c6875d6e6eb017386939ca9ec0000000000000000000000000000000000050d121f242b333a";

      Public_Key : SparkPass.Crypto.MLDSA.Public_Key;
      Secret_Key : SparkPass.Crypto.MLDSA.Secret_Key;
      Message : Byte_Array := Hex_To_Bytes (Msg_Hex);
      Signature : SparkPass.Crypto.MLDSA.Signature;
      Expected_Sig : SparkPass.Crypto.MLDSA.Signature;
      Generated_Sig : SparkPass.Crypto.MLDSA.Signature;
      Verified : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== NIST Test Vector #2 (count=2) ===");

      --  Parse test vector data
      Public_Key := Hex_To_Bytes (PK_Hex);
      Secret_Key := Hex_To_Bytes (SK_Hex);
      Expected_Sig := Hex_To_Bytes (Sig_Hex);

      --  Test 1: Verify the provided signature
      Put_Line ("[1] Verifying NIST signature...");
      SparkPass.Crypto.MLDSA.Verify (Public_Key, Message, Expected_Sig, Verified);

      if Verified then
         Put_Line ("  ✓ PASS: NIST signature verified successfully");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: NIST signature verification failed");
         Fail_Count := Fail_Count + 1;
      end if;

      --  Test 2: Generate our own signature and verify it
      Put_Line ("[2] Generating and verifying our own signature...");
      SparkPass.Crypto.MLDSA.Sign (Secret_Key, Message, Generated_Sig);
      SparkPass.Crypto.MLDSA.Verify (Public_Key, Message, Generated_Sig, Verified);

      if Verified then
         Put_Line ("  ✓ PASS: Generated signature verified successfully");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Generated signature verification failed");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_NIST_Vector_2;

   --  NIST KAT Test Vector #3 from kat_MLDSA_87_det_pure.rsp (count=4)
   procedure Test_NIST_Vector_3 is
      --  Test vector from official NIST post-quantum-cryptography/KAT repository
      --  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLDSA/kat_MLDSA_87_det_pure.rsp

      PK_Hex : constant String := "906efe081b007abf1e813c963015710430756eae0bf5dc70c72cd1f597492c4b9910c6877baab7efd3671d77be8229f6f1f63134540bdc2c93ceafd3a85033b231d2ceb16400b26fbf698c5c36bea3810315166a67dd51f4ee27eb95434a97a8f87a8357..."  -- truncated for readability
         & "b3de2ca2d8b9e43213392f8f45a5707dea7b8690e39bc871ee15cd41e13477978474181f30e78abcd36828642f161f200999";
      SK_Hex : constant String := "906efe081b007abf1e813c963015710430756eae0bf5dc70c72cd1f597492c4b08b43d8c525047206cf9e573f9838001bf1eb8271435b04b49236b4aa8048a963c4217d558a058de05c0840b1cb4b2426403ca0bf512f44e97c80b0aec2b269d504f433c..."  -- truncated for readability
         & "c0c80376f53038fd9fba241ec4edf42c671f4906080e61efea0a2dd26e565ce81f2901127894e722a31ec4e3a281608e5875";
      Msg_Hex : constant String := "8d45a2ab49d8c20d4ab5680e5c9d9d0cc9ca8228484946f9afce5b8df6f39d1921a31f5e6f03f016ed95c31460dbf285ef2a9a62f3e642760f205c5be6c2a9f17be70ca6be782ac73e02f8ed764e9af7";
      Sig_Hex : constant String := "af441ec8f07562d458d423f9305fd48bc63f775085721db367459b08bfb1b62670c99dfb8feb2a69db56c7f6a67682870cfc386fd0458fb58a882425a0997433e038619ab8cb8047164e8d969c4eb2e0e8ee8eadbabe91e8d13c524aa68ad78899ae3a2d..."  -- truncated for readability
         & "0f344b4d5984b4e4e50b63a3bbd1e6f5fd0000000000000000000000000000000000000000000000000002080f151b1f2a32";

      Public_Key : SparkPass.Crypto.MLDSA.Public_Key;
      Secret_Key : SparkPass.Crypto.MLDSA.Secret_Key;
      Message : Byte_Array := Hex_To_Bytes (Msg_Hex);
      Signature : SparkPass.Crypto.MLDSA.Signature;
      Expected_Sig : SparkPass.Crypto.MLDSA.Signature;
      Generated_Sig : SparkPass.Crypto.MLDSA.Signature;
      Verified : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== NIST Test Vector #3 (count=4) ===");

      --  Parse test vector data
      Public_Key := Hex_To_Bytes (PK_Hex);
      Secret_Key := Hex_To_Bytes (SK_Hex);
      Expected_Sig := Hex_To_Bytes (Sig_Hex);

      --  Test 1: Verify the provided signature
      Put_Line ("[1] Verifying NIST signature...");
      SparkPass.Crypto.MLDSA.Verify (Public_Key, Message, Expected_Sig, Verified);

      if Verified then
         Put_Line ("  ✓ PASS: NIST signature verified successfully");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: NIST signature verification failed");
         Fail_Count := Fail_Count + 1;
      end if;

      --  Test 2: Generate our own signature and verify it
      Put_Line ("[2] Generating and verifying our own signature...");
      SparkPass.Crypto.MLDSA.Sign (Secret_Key, Message, Generated_Sig);
      SparkPass.Crypto.MLDSA.Verify (Public_Key, Message, Generated_Sig, Verified);

      if Verified then
         Put_Line ("  ✓ PASS: Generated signature verified successfully");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Generated signature verification failed");
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_NIST_Vector_3;
