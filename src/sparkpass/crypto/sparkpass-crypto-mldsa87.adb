--  ============================================================================
--  ML-DSA-87 Digital Signature Algorithm (FIPS 204) - Pure SPARK
--  ============================================================================
--
--  **Purpose**: NIST FIPS 204 Module-Lattice Digital Signature Algorithm
--
--  **Security**: NIST Level 5 (beyond AES-256)
--
--  **Algorithms**:
--    - KeyGen: Algorithm 1 (Generate public/secret key pair)
--    - Sign:   Algorithm 2 (Create signature with rejection sampling)
--    - Verify: Algorithm 3 (Verify signature with constant-time comparison)
--
--  **Implementation**: 100% pure SPARK, zero FFI, formally verifiable
--
--  ============================================================================

pragma SPARK_Mode (On);
with SparkPass.Debug;
with SparkPass.Crypto.MLDSA87.Params;   use SparkPass.Crypto.MLDSA87.Params;
with SparkPass.Crypto.MLDSA87.Poly;     use SparkPass.Crypto.MLDSA87.Poly;
with SparkPass.Crypto.MLDSA87.PolyVec;  use SparkPass.Crypto.MLDSA87.PolyVec;
with SparkPass.Crypto.MLDSA87.Matrix;   use SparkPass.Crypto.MLDSA87.Matrix;
with SparkPass.Crypto.MLDSA87.Sampling; use SparkPass.Crypto.MLDSA87.Sampling;
with SparkPass.Crypto.MLDSA87.Packing;  use SparkPass.Crypto.MLDSA87.Packing;
with SparkPass.Crypto.MLDSA87.ZQ_Ops;   -- For AddQ, SubQ modular operations
with SparkPass.Crypto.Random;
with SparkPass.Crypto.Keccak;
with SparkPass.Crypto.Zeroize;
with Interfaces; use Interfaces;

package body SparkPass.Crypto.MLDSA87 is

   --  =========================================================================
   --  TEMPORARY HACK: Package-level storage to avoid stack overflow
   --  TODO: Refactor to Ada.Containers.Vectors for proper heap allocation
   --  =========================================================================

   --  Large structures moved to package level (static storage/heap)
   Global_A       : Matrix_KxL;              -- Expanded matrix A̅ (57KB)
   Global_S1      : PolyVec_L;               -- Secret s1 (7KB)
   Global_S2      : PolyVec_K;               -- Secret s2 (8KB)
   Global_T       : PolyVec_K;               -- t = A·s1 + s2 (8KB)
   Global_T1      : PolyVec_K;               -- High bits of t (8KB)
   Global_T0      : PolyVec_K;               -- Low bits of t (8KB)
   Global_S1_NTT  : PolyVec_L;               -- S1 in NTT domain (7KB)
   Global_Temp    : PolyVec_K;               -- Temporary (8KB)

   --  =========================================================================
   --  Key Generation (FIPS 204 Algorithm 1)
   --  =========================================================================

   procedure KeyGen (
      Public_Key  : out Public_Key_Array;
      Secret_Key  : out Secret_Key_Array
   ) is
      --  Seeds and hashes (small, OK on stack)
      Xi         : Byte_Array (1 .. 32);   -- Random seed ξ
      Hash_128   : Byte_Array (1 .. 128);  -- H(ξ) extended
      Rho        : Byte_Array (1 .. 32);   -- Matrix seed ρ
      Rho_Prime  : Byte_Array (1 .. 64);   -- Sampling seed ρ'
      K_Key      : Byte_Array (1 .. 32);   -- Signing key K
      TR         : Byte_Array (1 .. 64);   -- Public key hash tr

      --  Aliases for readability (point to package-level storage)
      A          : Matrix_KxL renames Global_A;
      S1         : PolyVec_L renames Global_S1;
      S2         : PolyVec_K renames Global_S2;
      T          : PolyVec_K renames Global_T;
      T1         : PolyVec_K renames Global_T1;
      T0         : PolyVec_K renames Global_T0;
      S1_NTT     : PolyVec_L renames Global_S1_NTT;
      Temp       : PolyVec_K renames Global_Temp;

      PK_Offset  : Natural := 1;
      SK_Offset  : Natural := 1;
   begin

      --  Step 1: Generate random seed ξ ← {0,1}^256
      Random.Fill (Xi);


      --  Step 2: (ρ, ρ', K) ← H(ξ, 128) using SHAKE-256
      Keccak.SHAKE_256 (Xi, Hash_128);
      Rho       := Hash_128 (1 .. 32);
      Rho_Prime := Hash_128 (33 .. 96);
      K_Key     := Hash_128 (97 .. 128);


      --  Step 3: Expand matrix A̅ ← ExpandA(ρ) in PLAIN domain
      ExpandA (Rho, A);


      --  Encode matrix A to Montgomery domain for NTT-based multiplication
      --  (Matrix is generated plain, but NTT ops require Montgomery domain)
      --
      --  CRITICAL FIX: Encode in-place using a single temporary polynomial
      --  to avoid stack overflow from allocating A_Mont (57KB).
      --  Peak stack usage: 1 temp poly (~1KB) instead of full matrix (57KB).
      declare
         Temp_Poly : Polynomial;  -- ~1KB temporary for encoding
      begin
         for I in Vec_K_Index loop
            for J in Vec_L_Index loop
               --  Encode A(I,J) to Montgomery domain via temporary
               SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (Temp_Poly, A (I, J));
               --  Store back to A (now in Montgomery domain)
               A (I, J) := Temp_Poly;
            end loop;
         end loop;
      end;


      --  Step 4: Sample secret vectors (s1, s2) ← ExpandS(ρ') in PLAIN domain
      declare
         Seed_66 : Byte_Array (1 .. 66);
      begin
         Seed_66 (1 .. 64) := Rho_Prime;
         Seed_66 (65) := 0;
         Seed_66 (66) := 0;
         UniformEta_Vec_L (Seed_66, 0, S1);
         UniformEta_Vec_K (Seed_66, Unsigned_16 (L), S2);
      end;


      --  Step 5: Compute t = NTT^-1(A̅ ◦ NTT(s1)) + s2
      --
      --  Domain contract:
      --    - A: MONTGOMERY domain (encoded above)
      --    - S1, S2: PLAIN domain (centered coeffs from sampling)
      --    - NTT operates in MONTGOMERY domain (zetas are Mont-encoded)
      --    - Must encode S1 before NTT, decode result after NTT_Inv
      --    - Final add must be in PLAIN domain
      --
      --  Encode S1 to Montgomery domain for NTT
      for I in Vec_L_Index loop
         SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (S1_NTT (I), S1 (I));
      end loop;


      --  Forward NTT (Montgomery domain)
      NTT_Vec_L (S1_NTT);


      --  Matrix-vector multiply (Montgomery domain: A_Mont * S1_NTT_Mont)
      Matrix_Vec_Multiply (Temp, A, S1_NTT);


      --  Inverse NTT (stays in Montgomery domain)
      NTT_Inv_Vec_K (Temp);

      --  Decode Temp back to PLAIN domain for addition
      declare
         Temp_Poly : Polynomial;
      begin
         for I in Vec_K_Index loop
            SparkPass.Crypto.MLDSA87.ZQ_Ops.Decode_Poly (Temp_Poly, Temp (I));
            Temp (I) := Temp_Poly;
         end loop;
      end;

      --  Add in PLAIN domain: T = Temp + S2 (both plain)
      Add_Vec_K (T, Temp, S2);


      --  Step 6: (t1, t0) ← Power2Round(t, d)
      Power2Round_Vec_K (T, T1, T0);

      --  Step 7: Encode public key pk ← pkEncode(ρ, t1)
      Public_Key (PK_Offset .. PK_Offset + 31) := Rho;
      PK_Offset := PK_Offset + 32;

      Pack_t1_Vec_K (T1, Public_Key (PK_Offset .. PK_Offset + T1_Bytes - 1));

      --  Step 8: tr ← H(pk, 512)
      Keccak.SHA3_512_Hash (Public_Key, TR);

      --  Step 9: Encode secret key sk ← skEncode(ρ, K, tr, s1, s2, t0)
      Secret_Key (SK_Offset .. SK_Offset + 31) := Rho;
      SK_Offset := SK_Offset + 32;
      Secret_Key (SK_Offset .. SK_Offset + 31) := K_Key;
      SK_Offset := SK_Offset + 32;
      Secret_Key (SK_Offset .. SK_Offset + 63) := TR;
      SK_Offset := SK_Offset + 64;
      Pack_s_Vec_L (S1, Secret_Key (SK_Offset .. SK_Offset + S1_Bytes - 1));
      SK_Offset := SK_Offset + S1_Bytes;
      Pack_s_Vec_K (S2, Secret_Key (SK_Offset .. SK_Offset + S2_Bytes - 1));
      SK_Offset := SK_Offset + S2_Bytes;
      for I in Vec_K_Index loop
         Pack_t0 (T0 (I), Secret_Key (SK_Offset .. SK_Offset + Poly_T0_Bytes - 1));
         SK_Offset := SK_Offset + Poly_T0_Bytes;
      end loop;


      --  Zeroize sensitive data
      Zeroize.Wipe (Xi);
      Zeroize.Wipe (Hash_128);
      Zeroize.Wipe (Rho_Prime);
      Zeroize.Wipe (K_Key);
   end KeyGen;

   --  =========================================================================
   --  Sign (FIPS 204 Algorithm 2)
   --  =========================================================================

   procedure Sign_Deterministic (
      Secret_Key  : in  Secret_Key_Array;
      Message     : in  Byte_Array;
      Signature   : out Signature_Array
   ) is
      --  Decode secret key
      Rho       : Byte_Array (1 .. 32);
      K_Key     : Byte_Array (1 .. 32);
      TR        : Byte_Array (1 .. 64);
      S1        : PolyVec_L;
      S2        : PolyVec_K;
      T0        : PolyVec_K;

      --  Signing state
      A         : Matrix_KxL;
      S1_NTT    : PolyVec_L;
      S2_NTT    : PolyVec_K;
      T0_NTT    : PolyVec_K;
      Mu        : Byte_Array (1 .. 64);    -- μ = H(tr || M)
      Rho_Prime : Byte_Array (1 .. 64);    -- Mask expansion seed
      Kappa     : Unsigned_16 := 0;

      --  Rejection sampling loop
      Y         : PolyVec_L;
      W         : PolyVec_K;
      W1        : PolyVec_K;
      W1_Packed : Byte_Array (1 .. W1_Bytes);
      C_Tilde   : Byte_Array (1 .. 32);
      C         : Polynomial;
      C_NTT     : Polynomial;
      Z         : PolyVec_L;
      R0        : PolyVec_K;
      CT0       : PolyVec_K;
      H         : PolyVec_K;
      Ones_Count : Natural;

      Temp_Vec_L : PolyVec_L;
      Temp_Vec_K : PolyVec_K;
      Temp_Poly  : Polynomial;

      SK_Offset : Natural := 1;
      Sig_Offset : Natural := 1;

      Z_Norm : Natural;
      R0_Norm : Natural;
      CT0_Norm : Natural;
      Hint_Success : Boolean;
      Rejected : Boolean;

      Iteration : Natural := 0;  -- Track rejection sampling iterations
   begin
      --  Step 1: Decode secret key
      Rho := Secret_Key (SK_Offset .. SK_Offset + 31);
      SK_Offset := SK_Offset + 32;
      K_Key := Secret_Key (SK_Offset .. SK_Offset + 31);
      SK_Offset := SK_Offset + 32;
      TR := Secret_Key (SK_Offset .. SK_Offset + 63);
      SK_Offset := SK_Offset + 64;

      declare
         S1_Packed : Byte_Array (1 .. S1_Bytes) := Secret_Key (SK_Offset .. SK_Offset + S1_Bytes - 1);
         S2_Packed : Byte_Array (1 .. S2_Bytes);
         Offset : Natural := 1;
      begin
         SK_Offset := SK_Offset + S1_Bytes;
         S2_Packed := Secret_Key (SK_Offset .. SK_Offset + S2_Bytes - 1);
         SK_Offset := SK_Offset + S2_Bytes;

         --  Unpack s1, s2
         for I in Vec_L_Index loop
            Unpack_s (S1_Packed (Offset .. Offset + Poly_S_Bytes - 1), S1 (I));
            Offset := Offset + Poly_S_Bytes;
         end loop;

         Offset := 1;
         for I in Vec_K_Index loop
            Unpack_s (S2_Packed (Offset .. Offset + Poly_S_Bytes - 1), S2 (I));
            Offset := Offset + Poly_S_Bytes;
         end loop;
      end;

      --  Unpack t0
      for I in Vec_K_Index loop
         Unpack_t0 (Secret_Key (SK_Offset .. SK_Offset + Poly_T0_Bytes - 1), T0 (I));
         SK_Offset := SK_Offset + Poly_T0_Bytes;
      end loop;


      --  Step 2: Transform to NTT domain
      --  CRITICAL: Must encode to Montgomery domain before NTT!
      --  NTT operates in Montgomery domain, unpacked coeffs are in PLAIN domain
      for I in Vec_L_Index loop
         SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (S1_NTT (I), S1 (I));
      end loop;
      NTT_Vec_L (S1_NTT);

      for I in Vec_K_Index loop
         SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (S2_NTT (I), S2 (I));
      end loop;
      NTT_Vec_K (S2_NTT);

      for I in Vec_K_Index loop
         SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (T0_NTT (I), T0 (I));
      end loop;
      NTT_Vec_K (T0_NTT);


      --  Step 3: Expand A̅ (produces PLAIN domain)
      ExpandA (Rho, A);

      --  CRITICAL: Encode matrix A to Montgomery domain for NTT operations
      --  (Same fix as in KeyGen - ExpandA produces PLAIN, NTT needs Montgomery)
      declare
         Temp_Poly : Polynomial;
      begin
         for I in Vec_K_Index loop
            for J in Vec_L_Index loop
               SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (Temp_Poly, A (I, J));
               A (I, J) := Temp_Poly;
            end loop;
         end loop;
      end;


      --  Step 4: μ ← H(tr || M, 512)
      declare
         TR_M : Byte_Array (1 .. 64 + Message'Length);
      begin
         TR_M (1 .. 64) := TR;
         TR_M (65 .. TR_M'Last) := Message;
         Keccak.SHA3_512_Hash (TR_M, Mu);
      end;

      --  Step 5: Generate ρ' for mask expansion
      declare
         K_Mu : Byte_Array (1 .. 32 + 64);
      begin
         K_Mu (1 .. 32) := K_Key;
         K_Mu (33 .. 96) := Mu;
         Keccak.SHAKE_256 (K_Mu, Rho_Prime);
      end;


      --  Steps 6-25: Rejection sampling loop
      loop
         Iteration := Iteration + 1;

         --  Step 8: y ← ExpandMask(ρ' || K, κ)
         UniformGamma1_Vec_L (Rho_Prime, Kappa, Y);

         --  Step 9: w ← NTT^-1(A̅ ◦ NTT(y))
         --  CRITICAL: Encode Y to Montgomery domain before NTT
         --  (Y from UniformGamma1 is in PLAIN domain, NTT needs Montgomery)
         for I in Vec_L_Index loop
            SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (Temp_Vec_L (I), Y (I));
         end loop;
         NTT_Vec_L (Temp_Vec_L);
         Matrix_Vec_Multiply (W, A, Temp_Vec_L);
         NTT_Inv_Vec_K (W);

         --  Decode W back to PLAIN domain for decomposition operations
         declare
            Temp_Poly : Polynomial;
         begin
            for I in Vec_K_Index loop
               SparkPass.Crypto.MLDSA87.ZQ_Ops.Decode_Poly (Temp_Poly, W (I));
               W (I) := Temp_Poly;
            end loop;
         end;

         --  Step 10: w1 ← HighBits(w)
         HighBits_Vec_K (W, W1);

         --  Step 11: c̃ ← H(μ || w1Encode(w1), 256)
         for I in Vec_K_Index loop
            Pack_w1 (W1 (I), W1_Packed ((I * Poly_W1_Bytes + 1) .. ((I + 1) * Poly_W1_Bytes)));
         end loop;

         declare
            Mu_W1 : Byte_Array (1 .. 64 + W1_Bytes);
         begin
            Mu_W1 (1 .. 64) := Mu;
            Mu_W1 (65 .. Mu_W1'Last) := W1_Packed;
            Keccak.SHAKE_256 (Mu_W1, C_Tilde);
         end;

         --  Step 12: c ← SampleInBall(c̃)
         SampleInBall (C_Tilde, C);
         --  CRITICAL: Encode C to Montgomery domain before NTT
         --  (SampleInBall produces PLAIN domain, NTT needs Montgomery)
         SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (C_NTT, C);
         NTT (C_NTT);


         --  Step 14: z ← y + s1 ◦ c
         --  FIX: S1 is in plain domain, must encode to Montgomery and NTT before pointwise multiply
         Temp_Vec_L := S1;
         for I in Vec_L_Index loop
            SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (Temp_Vec_L (I), S1 (I));
         end loop;
         NTT_Vec_L (Temp_Vec_L);  -- Now in NTT domain

         for I in Vec_L_Index loop
            Pointwise_Montgomery (Temp_Vec_L (I), Temp_Vec_L (I), C_NTT);  -- s1_NTT * c_NTT
         end loop;

         --  INTT to get back to plain domain
         NTT_Inv_Vec_L (Temp_Vec_L);

         --  Decode from Montgomery to plain
         declare
            Temp_Poly : Polynomial;
         begin
            for I in Vec_L_Index loop
               SparkPass.Crypto.MLDSA87.ZQ_Ops.Decode_Poly (Temp_Poly, Temp_Vec_L (I));
               Temp_Vec_L (I) := Temp_Poly;
            end loop;
         end;

         --  Now add in plain domain: z = y + s1*c
         for I in Vec_L_Index loop
            for J in Poly_Index loop
               Z (I)(J) := ZQ_Ops.AddQ (Y (I)(J), Temp_Vec_L (I)(J));
            end loop;
         end loop;

         --  Step 15: Compute r0 ← LowBits(w - s2 ◦ c)
         --  FIX: S2 is in plain domain, must encode to Montgomery and NTT before pointwise multiply
         Temp_Vec_K := S2;
         for I in Vec_K_Index loop
            SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (Temp_Vec_K (I), S2 (I));
         end loop;
         NTT_Vec_K (Temp_Vec_K);  -- Now in NTT domain

         for I in Vec_K_Index loop
            Pointwise_Montgomery (Temp_Vec_K (I), Temp_Vec_K (I), C_NTT);  -- s2_NTT * c_NTT
         end loop;

         --  INTT to get back to plain domain
         NTT_Inv_Vec_K (Temp_Vec_K);

         --  Decode from Montgomery to plain
         declare
            Temp_Poly : Polynomial;
         begin
            for I in Vec_K_Index loop
               SparkPass.Crypto.MLDSA87.ZQ_Ops.Decode_Poly (Temp_Poly, Temp_Vec_K (I));
               Temp_Vec_K (I) := Temp_Poly;
            end loop;
         end;

         --  Now subtract in plain domain: w - s2*c
         for I in Vec_K_Index loop
            for J in Poly_Index loop
               Temp_Vec_K (I)(J) := ZQ_Ops.SubQ (W (I)(J), Temp_Vec_K (I)(J));
            end loop;
         end loop;

         LowBits_Vec_K (Temp_Vec_K, R0);

         --  Step 16: Check ||z||∞ < γ1 - β and ||r0||∞ < γ2 - β
         Z_Norm := Infinity_Norm_Vec_L (Z);
         R0_Norm := Infinity_Norm_Vec_K (R0);

         Rejected := (Z_Norm >= Gamma1 - Beta) or (R0_Norm >= Gamma2 - Beta);

         if not Rejected then

            --  Step 19: Compute ct0
            for I in Vec_K_Index loop
               Pointwise_Montgomery (CT0 (I), T0_NTT (I), C_NTT);
            end loop;
            NTT_Inv_Vec_K (CT0);

            --  Decode CT0 back to PLAIN domain for norm checks and hint generation
            declare
               Temp_Poly : Polynomial;
            begin
               for I in Vec_K_Index loop
                  SparkPass.Crypto.MLDSA87.ZQ_Ops.Decode_Poly (Temp_Poly, CT0 (I));
                  CT0 (I) := Temp_Poly;
               end loop;
            end;

            --  Compute h ← MakeHint(-ct0, w - s2·c + ct0)
            --  First compute w'' = (w - c·s2) + c·t0
            for I in Vec_K_Index loop
               for J in Poly_Index loop
                  Temp_Vec_K (I)(J) := ZQ_Ops.AddQ (Temp_Vec_K (I)(J), CT0 (I)(J));
               end loop;
            end loop;

            --  Then negate CT0 for MakeHint (we need -c·t0)
            declare
               Neg_CT0 : PolyVec_K;
            begin
               for I in Vec_K_Index loop
                  for J in Poly_Index loop
                     Neg_CT0 (I)(J) := ZQ_Ops.SubQ (0, CT0 (I)(J));  -- Negate
                  end loop;
               end loop;

               MakeHint_Vec_K (H, Neg_CT0, Temp_Vec_K, Ones_Count);
            end;

            --  Step 20: Check ||ct0||∞ < γ2 and weight(h) ≤ ω
            CT0_Norm := Infinity_Norm_Vec_K (CT0);
            Rejected := (CT0_Norm >= Gamma2) or (Ones_Count > Omega);
         end if;

         exit when not Rejected;


         --  Step 24: κ ← κ + l
         Kappa := Kappa + Unsigned_16 (L);
      end loop;


      --  Step 26: σ ← sigEncode(c̃, z mod± q, h)

      Signature (Sig_Offset .. Sig_Offset + 31) := C_Tilde;
      Sig_Offset := Sig_Offset + 32;
      Pack_z_Vec_L (Z, Signature (Sig_Offset .. Sig_Offset + Z_Bytes - 1));
      Sig_Offset := Sig_Offset + Z_Bytes;
      Pack_Hint (H, Signature (Sig_Offset .. Sig_Offset + H_Bytes - 1), Hint_Success);


      --  Zeroize sensitive data
      Zeroize.Wipe (K_Key);
      Zeroize.Wipe (Rho_Prime);
   end Sign_Deterministic;

   --  =========================================================================
   --  Verify (FIPS 204 Algorithm 3)
   --  =========================================================================

   function Verify (
      Public_Key : in Public_Key_Array;
      Message    : in Byte_Array;
      Signature  : in Signature_Array
   ) return Boolean is
      --  Decode public key
      Rho        : Byte_Array (1 .. 32);
      T1         : PolyVec_K;
      PK_Offset  : Natural := 1;

      --  Decode signature
      C_Tilde    : Byte_Array (1 .. 32);
      Z          : PolyVec_L;
      H          : PolyVec_K;
      Sig_Offset : Natural := 1;
      Hint_Success : Boolean;

      --  Verification state
      A          : Matrix_KxL;
      TR         : Byte_Array (1 .. 64);
      Mu         : Byte_Array (1 .. 64);
      C          : Polynomial;
      C_NTT      : Polynomial;
      Z_NTT      : PolyVec_L;
      T1_NTT     : PolyVec_K;
      W_Prime_1  : PolyVec_K;
      W1_Packed  : Byte_Array (1 .. W1_Bytes);
      C_Tilde_Prime : Byte_Array (1 .. 32);

      Temp_Vec_K : PolyVec_K;
      Temp_Poly  : Polynomial;

      Z_Norm     : Natural;
      Match      : Boolean := True;
   begin
      --  Step 1: Decode public key (ρ, t1)
      Rho := Public_Key (PK_Offset .. PK_Offset + 31);
      PK_Offset := PK_Offset + 32;
      Unpack_t1_Vec_K (Public_Key (PK_Offset .. PK_Offset + T1_Bytes - 1), T1);

      --  Step 2: Decode signature (c̃, z, h)
      C_Tilde := Signature (Sig_Offset .. Sig_Offset + 31);
      Sig_Offset := Sig_Offset + 32;
      Unpack_z_Vec_L (Signature (Sig_Offset .. Sig_Offset + Z_Bytes - 1), Z);
      Sig_Offset := Sig_Offset + Z_Bytes;
      Unpack_Hint (Signature (Sig_Offset .. Sig_Offset + H_Bytes - 1), H, Hint_Success);

      --  Step 3: Check hint decode success
      if not Hint_Success then
         return False;
      end if;

      --  Step 4: Expand A̅ and encode to Montgomery domain (same as KeyGen)
      ExpandA (Rho, A);

      --  CRITICAL: Encode matrix A to Montgomery domain (ExpandA generates plain domain)
      declare
         Temp_Poly_A : Polynomial;
      begin
         for I in Vec_K_Index loop
            for J in Vec_L_Index loop
               SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (Temp_Poly_A, A (I, J));
               A (I, J) := Temp_Poly_A;
            end loop;
         end loop;
      end;

      --  Step 5: tr ← H(pk, 512)
      Keccak.SHA3_512_Hash (Public_Key, TR);

      --  Step 6: μ ← H(tr || M, 512)
      declare
         TR_M : Byte_Array (1 .. 64 + Message'Length);
      begin
         TR_M (1 .. 64) := TR;
         TR_M (65 .. TR_M'Last) := Message;
         Keccak.SHA3_512_Hash (TR_M, Mu);
      end;

      --  Step 7: c ← SampleInBall(c̃)
      SampleInBall (C_Tilde, C);
      --  CRITICAL: Encode C to Montgomery domain before NTT
      SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (C_NTT, C);
      NTT (C_NTT);

      --  Step 9: Transform z to NTT
      --  CRITICAL: Encode Z to Montgomery domain before NTT (Z comes from unpacking, which is plain domain)
      for I in Vec_L_Index loop
         SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (Z_NTT (I), Z (I));
      end loop;
      NTT_Vec_L (Z_NTT);

      --  Step 10: Compute w'1 = UseHint(h, 2^d · NTT^-1(A̅·ẑ - ĉ·t̂1))
      --  First: A̅·ẑ
      Matrix_Vec_Multiply (Temp_Vec_K, A, Z_NTT);

      --  Second: ĉ·(t̂1·2^d)
      --  FIX: Scale T1 by 2^d FIRST (in plain domain), then encode/NTT
      --  Spec requires: w'' = A·z - c·(t1·2^d), not 2^d·(A·z - c·t1)
      for I in Vec_K_Index loop
         for J in Poly_Index loop
            --  Multiply by 2^D and reduce mod Q (plain domain)
            declare
               use type Params.U64;
               Scaled : constant Params.U64 := Params.U64 (T1 (I)(J)) * Params.U64 (2 ** D);
            begin
               T1 (I)(J) := Zq (Scaled mod Params.U64 (Q));
            end;
         end loop;
      end loop;

      --  Now encode the scaled T1 to Montgomery and NTT
      for I in Vec_K_Index loop
         SparkPass.Crypto.MLDSA87.ZQ_Ops.Encode_Poly (T1_NTT (I), T1 (I));
      end loop;
      NTT_Vec_K (T1_NTT);

      --  Compute c·(t1·2^d) in NTT domain and subtract from A·z
      for I in Vec_K_Index loop
         Pointwise_Montgomery (Temp_Poly, T1_NTT (I), C_NTT);
         for J in Poly_Index loop
            Temp_Vec_K (I)(J) := ZQ_Ops.SubQ (Temp_Vec_K (I)(J), Temp_Poly (J));
         end loop;
      end loop;

      --  Transform back (stays in Montgomery domain)
      NTT_Inv_Vec_K (Temp_Vec_K);

      --  Decode from Montgomery to plain domain
      --  Result is now w'' = A·z - c·(t1·2^d) in plain domain (correct per spec)
      declare
         Temp_Poly : Polynomial;
      begin
         for I in Vec_K_Index loop
            SparkPass.Crypto.MLDSA87.ZQ_Ops.Decode_Poly (Temp_Poly, Temp_Vec_K (I));
            Temp_Vec_K (I) := Temp_Poly;
         end loop;
      end;

      --  DEBUG: Dump w'' (before UseHint) and hints
      SparkPass.Debug.Log ("[Verify] w''[0] coeffs (before UseHint) [0-19]:");
      --  for J in 0 .. 19 loop
      --     Ada.Text_IO.Put (Zq'Image (Temp_Vec_K (0)(J)) & " ");
      --  end loop;
      --  SparkPass.Debug.Log("");

      SparkPass.Debug.Log ("[Verify] HighBits(w'')[0] (without hints) [0-19]:");
      --  declare
      --     Temp_W1 : PolyVec_K;
      --  begin
      --     HighBits_Vec_K (Temp_Vec_K, Temp_W1);
      --     for J in 0 .. 19 loop
      --        Ada.Text_IO.Put (Zq'Image (Temp_W1 (0)(J)) & " ");
      --     end loop;
      --     SparkPass.Debug.Log("");
      --  end;

      SparkPass.Debug.Log ("[Verify] Hints H[0]:");
      --  for J in 0 .. 9 loop
      --     Ada.Text_IO.Put (Zq'Image (H (0)(J)) & " ");
      --  end loop;
      --  SparkPass.Debug.Log("");

      --  Step 11: w'1 ← UseHint(h, temp)
      UseHint_Vec_K (W_Prime_1, H, Temp_Vec_K);

      --  DEBUG: Dump w'1 coefficients (first polynomial, first 20 coeffs)
      SparkPass.Debug.Log ("[Verify] w'1[0] coeffs (after UseHint) [0-19]:");
      --  for J in 0 .. 19 loop
      --     Ada.Text_IO.Put (Zq'Image (W_Prime_1 (0)(J)) & " ");
      --  end loop;
      --  SparkPass.Debug.Log("");

      --  Pack w'1 for hashing

      --  DEBUG: Verify w'1[0] hasn't changed before packing
      SparkPass.Debug.Log ("[Verify] w'1[0] RIGHT BEFORE PACKING:");
      --  for J in 0 .. 9 loop
      --     Ada.Text_IO.Put (Zq'Image (W_Prime_1 (0)(J)) & " ");
      --  end loop;
      --  SparkPass.Debug.Log("");

      for I in Vec_K_Index loop
         Pack_w1 (W_Prime_1 (I), W1_Packed ((I * Poly_W1_Bytes + 1) .. ((I + 1) * Poly_W1_Bytes)));
      end loop;

      --  DEBUG: Dump packed w'1 bytes (first 20)
      SparkPass.Debug.Log ("[Verify] Packed_W'1 (first 20 bytes hex):");
      --  for I in 1 .. 20 loop
      --     declare
      --        Hex : constant String := "0123456789ABCDEF";
      --        Hi : constant Natural := Natural(W1_Packed(I)) / 16;
      --        Lo : constant Natural := Natural(W1_Packed(I)) mod 16;
      --     begin
      --        Ada.Text_IO.Put (Hex(Hi + 1) & Hex(Lo + 1) & " ");
      --     end;
      --  end loop;
      --  SparkPass.Debug.Log("");

      --  DEBUG: Dump μ (first 20 bytes hex)
      SparkPass.Debug.Log ("[Verify] μ (first 20 bytes hex):");
      --  for I in 1 .. 20 loop
      --     declare
      --        Hex : constant String := "0123456789ABCDEF";
      --        Hi : constant Natural := Natural(Mu(I)) / 16;
      --        Lo : constant Natural := Natural(Mu(I)) mod 16;
      --     begin
      --        Ada.Text_IO.Put (Hex(Hi + 1) & Hex(Lo + 1) & " ");
      --     end;
      --  end loop;
      --  SparkPass.Debug.Log("");

      --  Step 12: c̃' ← H(μ || w1Encode(w'1), 256)
      declare
         Mu_W1 : Byte_Array (1 .. 64 + W1_Bytes);
      begin
         Mu_W1 (1 .. 64) := Mu;
         Mu_W1 (65 .. Mu_W1'Last) := W1_Packed;
         Keccak.SHAKE_256 (Mu_W1, C_Tilde_Prime);
      end;

      --  DEBUG: Dump c̃' bytes (first 20)
      SparkPass.Debug.Log ("[Verify] c̃' (first 20 bytes hex):");
      --  for I in 1 .. 20 loop
      --     declare
      --        Hex : constant String := "0123456789ABCDEF";
      --        Hi : constant Natural := Natural(C_Tilde_Prime(I)) / 16;
      --        Lo : constant Natural := Natural(C_Tilde_Prime(I)) mod 16;
      --     begin
      --        Ada.Text_IO.Put (Hex(Hi + 1) & Hex(Lo + 1) & " ");
      --     end;
      --  end loop;
      --  SparkPass.Debug.Log("");

      --  Step 13: Check ||z||∞ < γ1 - β (constant-time)
      Z_Norm := Infinity_Norm_Vec_L (Z);

      --  Constant-time comparison of c̃ = c̃'
      for I in C_Tilde'Range loop
         if C_Tilde (I) /= C_Tilde_Prime (I) then
            Match := False;
         end if;
      end loop;

      --  Return [[||z||∞ < γ1 - β]] ∧ [[c̃ = c̃']]
      declare
         Z_Check : constant Boolean := (Z_Norm < Gamma1 - Beta);
      begin
         if not Z_Check then
            SparkPass.Debug.Log ("[Verify] Z-norm check FAILED: z_norm=" & Natural'Image (Z_Norm) &
                                  ", limit=" & Natural'Image (Gamma1 - Beta));
         end if;
         if not Match then
            SparkPass.Debug.Log ("[Verify] c-tilde check FAILED: c̃ ≠ c̃'");
         end if;
         return Z_Check and Match;
      end;
   end Verify;

end SparkPass.Crypto.MLDSA87;
