
   --  NIST KAT Test Vector #1 from kat_MLKEM_1024.rsp (count=0)
   procedure Test_NIST_Vector_1 is
      --  Test vector from official NIST post-quantum-cryptography/KAT repository
      --  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLKEM/kat_MLKEM_1024.rsp

      PK_Hex : constant String := "a8122f376b3f5d355263eba522c43995044beca78b9ba9924b87035490ae78a94d523c7ba588a386eb49bf652fdf12716373187a1ca047cc6138627aaeab8f6cebbf87a176b86c9f7f093c8f5b0bdc147399e839ae737038d0bd0142369f9c9d46b25b9a..."  -- truncated for readability
         & "bd27b3e647259bb1cccb2304d725cb2e73c6718b5abc88800949b2c1d03f159fbf396adca8526f1b476f80fe91c13308d645";
      SK_Hex : constant String := "4b8ac400218a249150aff2339367484a6bcbf637646cf750156a7b35a936d05a25647a3368e4891a030beba99ff9d550ca830ebf9b4aae231fc2dc4073aa98411a188bc2442c5349f20140c5a378037679840b23a795bd0ed5949d0b5d663bb01c575d7f..."  -- truncated for readability
         & "a3cd49df9f15ca02ad626e486f6a4921106bf696484048ec21f96cf50a56d0759c448f3779752f0383d37449690694cf7a68";
      CT_Hex : constant String := "707d18cabcf89670c80003b47d2b8678ad0da4aaad781a3351e82ac3e447e7019af5cb86020fb2ac727e79654155835b45de9b8af5e5c7efa01295da8988131dc8d6edb0645c3e4116aab080c5a571e760416fe8e299c89811963cf9776e0c828751701f..."  -- truncated for readability
         & "7e7ee0ad2fd2472bb9cf882a4851f5ba5b433def666fb0bfed4358afdf5b6005051e74c16244f4579288a0da8c2f49598f74";
      SS_Hex : constant String := "23f211b84a6ee20c8c29f6e5314c91b414e940513d380add17bd724ab3a13a52";

      Public_Key : SparkPass.Crypto.MLKEM.Public_Key;
      Secret_Key : SparkPass.Crypto.MLKEM.Secret_Key;
      Ciphertext : SparkPass.Crypto.MLKEM.Ciphertext;
      Shared_Secret : SparkPass.Crypto.MLKEM.Shared_Key;
      Expected_SS : SparkPass.Crypto.MLKEM.Shared_Key;
      Success : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== NIST Test Vector #1 (count=0) ===");

      --  Parse test vector data
      Public_Key := Hex_To_Bytes (PK_Hex);
      Secret_Key := Hex_To_Bytes (SK_Hex);
      Ciphertext := Hex_To_Bytes (CT_Hex);
      Expected_SS := Hex_To_Bytes (SS_Hex);

      --  Perform decapsulation
      Put_Line ("[1] Decapsulating with NIST test vector...");
      SparkPass.Crypto.MLKEM.Decapsulate (Secret_Key, Ciphertext, Shared_Secret, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Decapsulation failed");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Verify shared secret matches expected value
      if Bytes_Equal (Shared_Secret, Expected_SS) then
         Put_Line ("  ✓ PASS: Shared secret matches NIST expected value");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Shared secret mismatch");
         Print_Bytes ("    Expected", Expected_SS);
         Print_Bytes ("    Got     ", Shared_Secret);
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_NIST_Vector_1;

   --  NIST KAT Test Vector #2 from kat_MLKEM_1024.rsp (count=2)
   procedure Test_NIST_Vector_2 is
      --  Test vector from official NIST post-quantum-cryptography/KAT repository
      --  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLKEM/kat_MLKEM_1024.rsp

      PK_Hex : constant String := "ffea9b3fcb1023e9b838bb485033831c5564286c90cfd80bd8e895f61078e5a92f73f4829137c50548c9bc1b09f2fc476a70744de23545db42eec1aa263843e0005907c1a1fe5ab9f68aabd07b3c0ae833a105871d549d8b3464b53c66e5d165fc75c9b8..."  -- truncated for readability
         & "89222ea3eb4190872de5e11726c410b803565ddbdc03f982920cfe3bb7f55b96f25b0a9ed071f3b042d0519b419a87bedb9e";
      SK_Hex : constant String := "facb01872a06f1a053a0d03c9c742732c96f4b6a46bf16cd53e93d35b57c645388e89b39aeeb890aeaa264866b74551f6c651b9c97b98f163e94f1ab5cc952d31369562046978469ea26b1f52a33ce4b7ee7b05006176fb7585d11a710a4fc56c10c6477..."  -- truncated for readability
         & "f3a7ccde0950bc96645df6d6188b47e91e661eaae6bb91b27cd748c402c4111140d5a942cf3c95ff7977f88d2ef515bb26d0";
      CT_Hex : constant String := "b9b6c3b68f901e1143f4fa61d589645455391a09401916ac70d1cad9b42e5bd61dcf4bb62db454f580af65a8cf07d848bc85c8f4622f9dbfca424057b636fe5658eebb2c52839a43135aaccaf198f1386ed5a700bfccedd56b29889e759d6c32cbb9ca99..."  -- truncated for readability
         & "1f6a43294e7b45c04113fb9983dd26d6a9812925c1311b195b43e159930152b1b552cd661921657a287810b1940ccbcef15b";
      SS_Hex : constant String := "723ab62b92580b3def8ce887001eefe4d90565aacaf5430ee2aab7decf99db76";

      Public_Key : SparkPass.Crypto.MLKEM.Public_Key;
      Secret_Key : SparkPass.Crypto.MLKEM.Secret_Key;
      Ciphertext : SparkPass.Crypto.MLKEM.Ciphertext;
      Shared_Secret : SparkPass.Crypto.MLKEM.Shared_Key;
      Expected_SS : SparkPass.Crypto.MLKEM.Shared_Key;
      Success : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== NIST Test Vector #2 (count=2) ===");

      --  Parse test vector data
      Public_Key := Hex_To_Bytes (PK_Hex);
      Secret_Key := Hex_To_Bytes (SK_Hex);
      Ciphertext := Hex_To_Bytes (CT_Hex);
      Expected_SS := Hex_To_Bytes (SS_Hex);

      --  Perform decapsulation
      Put_Line ("[1] Decapsulating with NIST test vector...");
      SparkPass.Crypto.MLKEM.Decapsulate (Secret_Key, Ciphertext, Shared_Secret, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Decapsulation failed");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Verify shared secret matches expected value
      if Bytes_Equal (Shared_Secret, Expected_SS) then
         Put_Line ("  ✓ PASS: Shared secret matches NIST expected value");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Shared secret mismatch");
         Print_Bytes ("    Expected", Expected_SS);
         Print_Bytes ("    Got     ", Shared_Secret);
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_NIST_Vector_2;

   --  NIST KAT Test Vector #3 from kat_MLKEM_1024.rsp (count=4)
   procedure Test_NIST_Vector_3 is
      --  Test vector from official NIST post-quantum-cryptography/KAT repository
      --  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLKEM/kat_MLKEM_1024.rsp

      PK_Hex : constant String := "a9a1c276f803a4f215315c46acaa8c593abedb798849f5089da152bd83a3f75324f42a3a605017f495504d42370b9c671080ca06c24c3c900d7c56adeb948311c9c21cf337ee6a85aa3a10f077baa659baae35288853b75acc9af68b0481e1842f671b15..."  -- truncated for readability
         & "dfb4118af3c3eb2ca5dd791b96346a4de295ebd676f386e5ac359dad821c4199bb28f5f72db0f0142ff006fcd2a8139af1d2";
      SK_Hex : constant String := "97c3215241559dcb584b06501084676e5272c8f73873d4255f6cc13b6c6c450299dc638f788112bd31a8395b76b772108f7931b82b17d329961731592603b703000a5098664991aed25c9526d7cb2110280007b34e24b936623034e9ad020237549769af..."  -- truncated for readability
         & "0c906b9d4fc240c4918b404fc489f73b9d90a9f93c7b791356b66afcceb745a548c7f6b185e4f45ec1ff1a22acdd96e7a6d8";
      CT_Hex : constant String := "bab16c303772b7d8b9b98ee51a41ec73727b12dd69557c2968f8285591bc939afae45b48777b24f06591bf4a2b6cc992c7d3aedd9c0a7565b81f9598ce73e2248107cf203159cccf95501e47585aec3a196e3be879d5c58ca00c2b717c00b0d7b2d4de50..."  -- truncated for readability
         & "fbd14f7f9ba8f003704776aad1b3b468bcf259d14831f7ff12494aabf20600ee4ee6d0ef61934258482788dcf9da9a04fb16";
      SS_Hex : constant String := "092464b72d6a6363ab423bc5ab0b4020a6a360df2cf6f6a5e1696b62ec6ce43d";

      Public_Key : SparkPass.Crypto.MLKEM.Public_Key;
      Secret_Key : SparkPass.Crypto.MLKEM.Secret_Key;
      Ciphertext : SparkPass.Crypto.MLKEM.Ciphertext;
      Shared_Secret : SparkPass.Crypto.MLKEM.Shared_Key;
      Expected_SS : SparkPass.Crypto.MLKEM.Shared_Key;
      Success : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== NIST Test Vector #3 (count=4) ===");

      --  Parse test vector data
      Public_Key := Hex_To_Bytes (PK_Hex);
      Secret_Key := Hex_To_Bytes (SK_Hex);
      Ciphertext := Hex_To_Bytes (CT_Hex);
      Expected_SS := Hex_To_Bytes (SS_Hex);

      --  Perform decapsulation
      Put_Line ("[1] Decapsulating with NIST test vector...");
      SparkPass.Crypto.MLKEM.Decapsulate (Secret_Key, Ciphertext, Shared_Secret, Success);

      if not Success then
         Put_Line ("  ✗ FAIL: Decapsulation failed");
         Fail_Count := Fail_Count + 1;
         return;
      end if;

      --  Verify shared secret matches expected value
      if Bytes_Equal (Shared_Secret, Expected_SS) then
         Put_Line ("  ✓ PASS: Shared secret matches NIST expected value");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("  ✗ FAIL: Shared secret mismatch");
         Print_Bytes ("    Expected", Expected_SS);
         Print_Bytes ("    Got     ", Shared_Secret);
         Fail_Count := Fail_Count + 1;
      end if;

      New_Line;
   end Test_NIST_Vector_3;
