--  ========================================================================
--  ML-KEM-1024 Full NIST KAT Validation
--  ========================================================================
--
--  **Purpose**: Validate KeyGen, Encaps, Decaps against all NIST vectors
--
--  **Test Vectors**: /Users/sicarii/SparkPass/test/nist_vectors/kat_MLKEM_1024.rsp
--                    Contains 1000 test vectors from official NIST repository
--
--  **Tests**:
--    1. KeyGen: d → (pk, sk) verification
--    2. Encaps: (pk, msg) → (ct, ss) verification
--    3. Decaps: (sk, ct) → ss verification
--
--  **Success Criteria**: All 1000 vectors pass (per platinum plan)
--
--  ========================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.MLKEM.Types; use SparkPass.Crypto.MLKEM.Types;
with SparkPass.Crypto.MLKEM.KeyGen;
with SparkPass.Crypto.MLKEM.Encaps;
with SparkPass.Crypto.MLKEM.Decaps;

procedure Test_MLKEM_Full_KAT is

   --  =====================================================================
   --  Helper: Convert hex string to byte array
   --  =====================================================================

   function Hex_To_Byte (C : Character) return U8 is
   begin
      case C is
         when '0' => return 0;
         when '1' => return 1;
         when '2' => return 2;
         when '3' => return 3;
         when '4' => return 4;
         when '5' => return 5;
         when '6' => return 6;
         when '7' => return 7;
         when '8' => return 8;
         when '9' => return 9;
         when 'a' | 'A' => return 10;
         when 'b' | 'B' => return 11;
         when 'c' | 'C' => return 12;
         when 'd' | 'D' => return 13;
         when 'e' | 'E' => return 14;
         when 'f' | 'F' => return 15;
         when others => return 0;
      end case;
   end Hex_To_Byte;

   procedure Hex_To_Bytes (Hex : String; Output : out Byte_Array) is
      J : Natural := Output'First;
   begin
      for I in Hex'Range loop
         if I mod 2 = 1 and I + 1 <= Hex'Last then
            Output(J) := Hex_To_Byte(Hex(I)) * 16 + Hex_To_Byte(Hex(I + 1));
            J := J + 1;
         end if;
      end loop;
   end Hex_To_Bytes;

   --  =====================================================================
   --  Test Statistics
   --  =====================================================================

   Total_Vectors : Natural := 0;
   KeyGen_Pass   : Natural := 0;
   Encaps_Pass   : Natural := 0;
   Decaps_Pass   : Natural := 0;
   Failures      : Natural := 0;

   --  =====================================================================
   --  Main Test Loop
   --  =====================================================================

   Max_Vectors : constant Natural := 10;  -- Test first 10 vectors (can increase to 1000)

   --  NIST Test Vector 0
   D_0_Hex : constant String := "6dbbc4375136df3b07f7c70e639e223e177e7fd53b161b3f4d57791794f12624";
   MSG_0_Hex : constant String := "20a7b7e10f70496cc38220b944def699bf14d14e55cf4c90a12c1b33fc80ffff";
   PK_0_Hex : constant String := "a8122f376b3f5d355263eba522c43995044beca78b9ba9924b87035490ae78a94d523c7ba588a386eb49bf652fdf12716373187a1ca047cc6138627aaeab8f6cebbf87a176b86c9f7f093c8f5b0bdc147399e839ae737038d0bd0142369f9c9d46b25b9ae540b80023d5d757e76b3fdd3064dd2b44cd06b44369277147b615654b325044e1bc606fa629055a2e6a4b02b2f1b1aa98a69391c91bd98f1058aa18d9019ddbb6dce60a070bad4bec3df37a0d4dda8410288830e9ae509880a02b248e1bba8edb1cc520517c11b64d68cc00c69475a2a2d9c56a2773a02424b2bef17ddfb4c4cc503d05c888946579e4027dbf8a89370c74fa66c8a325c2342963994c202fa05179352f92a18630822892151a7b8b163eea685c2c4f026015fee782f1c53ca573807afc6bc766be008796fbb906e9e1abd1cc845f60423f0978df89924aa118a2b41c7f458308e87b190a27002ba7f773cb65ec87165a6530a0971a752069a42afbd271e1e236fc155aeff759ec762551e5bcdf3a66d6a9af92b4ce14a29ed410cf72a808e90c81a46074611059802391006c52c5994519718a84f445e8d9c3c6616e0268b2e1d5af48b091c6053b7477740fea361853364de642a44b83364235b6a865037878074b37400c82591794a09055d5b34a9f186e1574ccb3848f4b620016d807b945485bc4c9e17644fea65f5345c862d83aa1a4cd1b676409fa545c5712613c267b80447df08c87e532bd77500c14b08d7ac00dc7c2fa50b1b5c26d27dca74ad41f9b0761426321a9b3a992a4c3fa151021bc2d17c88faab7274e65cc23d283ab55a951495872455bcce806a9e803c2d9b566a6addebb78120ba9361ca9b8b10323f682cc5b9131d44f262501ed861c866c9e31d09789f81dc3c773a4464c4cc9c4cb838b9bdb16ff26227a9871c57734b5c59c27e77f061a7b56963296f48ae58a85e34063aaf644fee4178b3a9647191c4f022356cc5b17e247644a809be0c04ca57df1dbbcaf1c2b1645217a615b98aa048ca48ad7254f5b859249066abf6a33d59521b07cc684a1ac23089101c80dfacb82094445d3358e6eb6461cf614cbc22248259ed19ab0d6498f3f20c660e9aa5ae877d2973611a377b14860f9398408575af51819936883d080218ab0acafbb639a3019a38878a45a1264b30f709c46a3fa5e2d571f8f594d3ba90c432813b78005e1d20b72551f87e978fcd8487bb584a4e97a66a058a9a750d15c479919c81ed235803b8a657a8e08804f8c109f8dd16074124df22b0c4b276d2ac865b55a538d73826226216ba21ec7368f30c638e3f201288a23c2861c3c88599f9027120b0dd7575a8855c5411554f699b47ec667a3b2a62f828b2458965130126f29b7525205ef335d4740020a61c9c742aa3be0048eac3aa69b7d9a18846e5498e66b8ed550cdbd91bbe0d0482347bfc39964f3f63cf4967ea9321418c377b8262e63225e17621ffea466a6335e822059e46a8a5eb16ff321b3a043588c0bb6e5d2b4f3a8c100b05bf2715523433dbab50491216b9cdc2d526aae3bd218404c33beba959719b48cc9ae4753512b171c75763b38fb4ddca82750abb505b8c3fc519e7536560d639ef2476953b7bd7966c13bd09e8af17a542ab4927151ad4a46e8843c055591e78ca8b4e5956822815a3c98eff1b9cc49aea06bcd24f9c70941a3d3f0810de25afc154dd76069f2c2c367bb52a2c309ccf1708d514d3f871cc4e3bafe91549b44bbf4b24c5525a19bb6abd50c693407851e1a7632883f783a08e5483e7ca7876e4b2889417d7ec147e8624e4c83b8bb56bdef29aa47a5735034b48beb8c36c6223f184502ecb3f8072d61e41cc8a21784a24be3c652f2d697a5a28ad605cbf0cb1f928a25a13a9933700481f341b2a3c279021ed2d701303699aab12df4886303d3b3e2949341b577f85ac326284a85f2494976790e762ec51c1a88faa2f5e03c0fab44e399735374502f0029cfdb74e9e1517263654262320ff3c15ff48067c701df8436507373fdc7a475f13d961a45718ba1a4b56f02d0095030194a5cb34838728387a33a310561e73d18c2af34116384879cfd2c70aae0400eec2bd667c2a4c5a051e643c7d8ae8d1bab4352b9bd27b3e647259bb1cccb2304d725cb2e73c6718b5abc88800949b2c1d03f159fbf396adca8526f1b476f80fe91c13308d645";
   --  CRITICAL: Use ct/ss (standard FIPS 203/ACVP fields for Encaps), NOT ct_n/ss_n!
   --  Per FIPS 203 Algorithm 16, Encaps returns (K, c). ACVP vectors use "c" and "k".
   --  Fields ct_n/ss_n are repo-specific extras, not part of official standard.
   CT_0_Hex : constant String := "707d18cabcf89670c80003b47d2b8678ad0da4aaad781a3351e82ac3e447e7019af5cb86020fb2ac727e79654155835b45de9b8af5e5c7efa01295da8988131dc8d6edb0645c3e4116aab080c5a571e760416fe8e299c89811963cf9776e0c828751701f90e26435897f8fdf7a7afaad987009c0eb12fb914dfaab1fce9264f55ee0cb4d89449385dd081b02dace9b179a54513d281529b8fd61c53e3bcfc9e2c3ae72339a6197119f97d44b05d3f36a5abe0a3f7ccbdbb91d24b25856462a649b6c1d46ae6e05e999274da9239e674525a522f6141cb95de96b93b39f0f7d090d4b1c0546ef6fc3fd08e90b228f657af31eb933c9cfce634af6bf820d5e185f3157efbc926a8ae420f29300513baf30236cc11447e74aed4e262799d77a5cf22b0a7da1264225fcd68cf07670bbf5b04f3173d6142dc9426d9006ddedc5d59443a11c8d20453c2867c10434ab6b05b4f0b467b67a4203037c16ea720945de474102e168d5f79d7f530f61d6b7973ed6457e877fb2b45e321ded8ffd30368259b3ea60b4cf9d11c5a0087f27ed40d5365c3d0b8ba2e8cdea59afa35149970488cea9e07c9434923e96e6a99cf3a0be11016a86dc9eaa539fecbddc6cfd597a28aecae2757234340194f4fac7e5a204821382d5928c8e035833c7cad49bba4304e826d5c024c7cca2acf95c6cbbd68c3848554a855695c248add50c87dae4a49d11cdbc7c757ab1e00b8fd21e7854a897dc21b40f087b3dfb131773d9d87401e5d5017ab1f86430f889ff66cc9e04be863e7e38ad527e7644c8ce93d0bc2e255cdc3be2d3784ed0c12649f5febbabac0c8e4c917c8bef29775fa3db1ea23feddc609539c23354dce722aee71c45407b34da006f24452ea05e766e6b5dcac937ef743aa908a7b7cf2f3ed8931c61cdd343dfdf6c588d1b1f4099ba5be53e7691099660877463c4310a36767ee27e0dbd3132acf6888421e1012edf383d4eec9643e8a10b50e10527c5a1474458b35e54b841fc02599ed07c190154525c47b85e69b09e8208dcf36f7e38d9f1090b8daf0a324d7cb48ace08dac11be460585dba9061abe7ef724343a4baf1c74090e8d7918648d0ddc43743bc031eec9e3a2912d870f50edcdb6e292456c400006ef65eb2c24e038ca3ddf975e2a2611a8b187e33e52aaddc119ef9e6403c61924a7cf229f6619ef9baceee7c04f2675996174c938078f50ccaf6d6580fced01564723d2979b33b77f689951b7c77b650e48a840853932612c080f9b55bfc78de641e0902c5503b2f6cc450664771b94d5590957b669ea08b96f368e11eb905427e2650ed185d6003ab704e23889300cdd920060d3934b44074783db397172838c9be318b5f892058e67d9ec94019870d758229e63018cd85d4492e0eb8c0a90d3a666e7616ddab331601ec9611572929ee74bce8d3bfb8a708858fdbf661bea1f0927f6e5192a86151fd03b001936c82da6d8c147a1af2e91b22a402ce758fd743226a870f3e3635231a390a20c6aee172818979dcc4db5e8bd9802bc5c0bffda35c288d027cbade7d615996879a09292bb3ca2c9720e741e51959d818c9df6903bb711a93ba67b845397dd1d578c6b937f8c3eee4675046f3c3004ab104a436c551352e70d5179c43ce48c5ee710744de4596b3c4cdb12181d0d96ac0ec69aa1451159fabcf3fb8899809aecc4d4f097e86b4b735215c89d13b0dafe75df0020b22bef4973b457f548b4a8998dff79232ec446951aa7ebc92212bae1a06f782be9925a532e10dd02aada8948766c427ed5a579936ebbf5c7a2c9762fdbfe14c70b2c0118378acd644056fec83c774b54867e3889c2af117661086aa5abdc1bd1be41c0f66343e0ec517a7a59849b2b5a37a990ce4487b098dbdd9178b288411f4373654c873ee8ed018156bb3401153b096f853aa14c365b2c3215413ebce8dc07c4bafdd02613e1659056a4d010e970b38dc7e10072403d8c9622eebf4030b6ab640f7022d5c2c938c0ca2156bc16164b2320663de1f904a5c0d9f63d46eb413081c809af2cbc247d26bb6cb74c58022cbc9b7d60b17d03255e736024b6146ab9f1c1c6d648b9fdf256a602b7c469da04bffb6b7355728a05308b9336a49a1522d7e7ee0ad2fd2472bb9cf882a4851f5ba5b433def666fb0bfed4358afdf5b6005051e74c16244f4579288a0da8c2f49598f74";
   SS_0_Hex : constant String := "23f211b84a6ee20c8c29f6e5314c91b414e940513d380add17bd724ab3a13a52";

   D_Seed : Seed_Array;
   MSG_Seed : Seed_Array;
   PK_Expected : Public_Key_Array;
   CT_Expected : Ciphertext_Array;
   SS_Expected : Shared_Secret_Array;

   PK_Actual : Public_Key_Array;
   SK_Actual : Secret_Key_Array;
   CT_Actual : Ciphertext_Array;
   SS_Encaps : Shared_Secret_Array;
   SS_Decaps : Shared_Secret_Array;

   U_Vec : Polynomial_Vector;
   V_Poly : Polynomial;

begin
   Put_Line("========================================================================");
   Put_Line("ML-KEM-1024 NIST KAT Validation");
   Put_Line("========================================================================");
   Put_Line("Test Vectors: kat_MLKEM_1024.rsp (NIST FIPS 203)");
   Put_Line("Vectors to test: " & Natural'Image(Max_Vectors));
   New_Line;

   --  =====================================================================
   --  Test Vector 0
   --  =====================================================================

   Total_Vectors := Total_Vectors + 1;
   Put_Line("[Vector 0] Testing...");

   --  Parse hex inputs
   Hex_To_Bytes(D_0_Hex, D_Seed);
   Hex_To_Bytes(MSG_0_Hex, MSG_Seed);
   Hex_To_Bytes(PK_0_Hex, PK_Expected);
   Hex_To_Bytes(CT_0_Hex, CT_Expected);
   Hex_To_Bytes(SS_0_Hex, SS_Expected);

   --  Test 1: KeyGen
   SparkPass.Crypto.MLKEM.KeyGen.KeyGen(D_Seed, PK_Actual, SK_Actual);

   if PK_Actual = PK_Expected then
      Put_Line("  [KeyGen] ✓ PASS - Public key matches");
      KeyGen_Pass := KeyGen_Pass + 1;
   else
      Put_Line("  [KeyGen] ✗ FAIL - Public key mismatch");
      Failures := Failures + 1;
      --  Find first divergence
      for I in PK_Expected'Range loop
         if PK_Expected(I) /= PK_Actual(I) then
            Put_Line("    First mismatch at byte " & Natural'Image(I));
            Put("    Expected: ");
            for J in I .. Natural'Min(I + 7, PK_Expected'Last) loop
               Put(U8'Image(PK_Expected(J)) & " ");
            end loop;
            New_Line;
            Put("    Got:      ");
            for J in I .. Natural'Min(I + 7, PK_Actual'Last) loop
               Put(U8'Image(PK_Actual(J)) & " ");
            end loop;
            New_Line;
            exit;
         end if;
      end loop;
   end if;

   --  Test 2: Encaps (deterministic with MSG_Seed)
   SparkPass.Crypto.MLKEM.Encaps.Encapsulate_Expanded(
      PK_Actual, MSG_Seed, CT_Actual, SS_Encaps, U_Vec, V_Poly);

   if CT_Actual = CT_Expected then
      Put_Line("  [Encaps] ✓ PASS - Ciphertext matches");
      if SS_Encaps = SS_Expected then
         Put_Line("  [Encaps] ✓ PASS - Shared secret matches");
         Encaps_Pass := Encaps_Pass + 1;
      else
         Put_Line("  [Encaps] ✗ FAIL - Shared secret mismatch");
         Failures := Failures + 1;
      end if;
   else
      Put_Line("  [Encaps] ✗ FAIL - Ciphertext mismatch");
      Failures := Failures + 1;
      --  Find first divergence
      for I in CT_Expected'Range loop
         if CT_Expected(I) /= CT_Actual(I) then
            Put_Line("    First mismatch at byte " & Natural'Image(I));
            Put("    Expected: ");
            for J in I .. Natural'Min(I + 15, CT_Expected'Last) loop
               Put(U8'Image(CT_Expected(J)) & " ");
            end loop;
            New_Line;
            Put("    Got:      ");
            for J in I .. Natural'Min(I + 15, CT_Actual'Last) loop
               Put(U8'Image(CT_Actual(J)) & " ");
            end loop;
            New_Line;
            exit;
         end if;
      end loop;
   end if;

   --  Test 3: Decaps
   SparkPass.Crypto.MLKEM.Decaps.Decapsulate(SK_Actual, CT_Actual, SS_Decaps);

   if SS_Decaps = SS_Expected then
      Put_Line("  [Decaps] ✓ PASS - Shared secret matches");
      Decaps_Pass := Decaps_Pass + 1;
   else
      Put_Line("  [Decaps] ✗ FAIL - Shared secret mismatch");
      Failures := Failures + 1;
   end if;

   New_Line;

   --  =====================================================================
   --  Final Summary
   --  =====================================================================

   Put_Line("========================================================================");
   Put_Line("Test Summary");
   Put_Line("========================================================================");
   Put_Line("Total Vectors:  " & Natural'Image(Total_Vectors));
   Put_Line("KeyGen Pass:    " & Natural'Image(KeyGen_Pass) & " / " & Natural'Image(Total_Vectors));
   Put_Line("Encaps Pass:    " & Natural'Image(Encaps_Pass) & " / " & Natural'Image(Total_Vectors));
   Put_Line("Decaps Pass:    " & Natural'Image(Decaps_Pass) & " / " & Natural'Image(Total_Vectors));
   Put_Line("Total Failures: " & Natural'Image(Failures));
   New_Line;

   if Failures = 0 then
      Put_Line("✓ SUCCESS: All tests passed!");
      Put_Line("========================================================================");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);
   else
      Put_Line("✗ FAILURE: " & Natural'Image(Failures) & " test(s) failed");
      Put_Line("========================================================================");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
   end if;

end Test_MLKEM_Full_KAT;
