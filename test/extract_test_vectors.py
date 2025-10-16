#!/usr/bin/env python3
"""
Extract test vectors from NIST KAT RSP files for ML-KEM and ML-DSA
"""

def extract_mlkem_vectors(filename, num_vectors=3):
    """Extract ML-KEM test vectors from RSP file"""
    vectors = []
    with open(filename, 'r') as f:
        lines = f.readlines()

    i = 0
    while i < len(lines) and len(vectors) < num_vectors:
        line = lines[i].strip()
        if line.startswith('count = '):
            count = int(line.split('=')[1].strip())
            vector = {'count': count}

            # Read the test vector fields
            while i + 1 < len(lines):
                i += 1
                line = lines[i].strip()
                if not line or line.startswith('count ='):
                    break
                if '=' in line:
                    key, value = line.split('=', 1)
                    vector[key.strip()] = value.strip()

            # Only include vectors with all required fields for normal encap/decap
            # Note: ML-KEM test files have both valid (ct, ss) and invalid (ct_n, ss_n) tests
            if all(k in vector for k in ['pk', 'sk', 'ct', 'ss']):
                vectors.append(vector)
        i += 1

    return vectors

def extract_mldsa_vectors(filename, num_vectors=3):
    """Extract ML-DSA test vectors from RSP file"""
    vectors = []
    with open(filename, 'r') as f:
        lines = f.readlines()

    i = 0
    while i < len(lines) and len(vectors) < num_vectors:
        line = lines[i].strip()
        if line.startswith('count = '):
            count = int(line.split('=')[1].strip())
            vector = {'count': count}

            # Read the test vector fields
            while i + 1 < len(lines):
                i += 1
                line = lines[i].strip()
                if not line or line.startswith('count ='):
                    break
                if '=' in line:
                    key, value = line.split('=', 1)
                    vector[key.strip()] = value.strip()

            # Only include vectors with required fields
            if all(k in vector for k in ['pk', 'sk', 'msg', 'sm']):
                vectors.append(vector)
        i += 1

    return vectors

def format_ada_test_vector_mlkem(vector, test_num):
    """Format ML-KEM test vector as Ada procedure"""
    return f"""
   --  NIST KAT Test Vector #{test_num} from kat_MLKEM_1024.rsp (count={vector['count']})
   procedure Test_NIST_Vector_{test_num} is
      --  Test vector from official NIST post-quantum-cryptography/KAT repository
      --  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLKEM/kat_MLKEM_1024.rsp

      PK_Hex : constant String := "{vector['pk'][:200]}..."  -- truncated for readability
         & "{vector['pk'][-100:]}";
      SK_Hex : constant String := "{vector['sk'][:200]}..."  -- truncated for readability
         & "{vector['sk'][-100:]}";
      CT_Hex : constant String := "{vector['ct'][:200]}..."  -- truncated for readability
         & "{vector['ct'][-100:]}";
      SS_Hex : constant String := "{vector['ss']}";

      Public_Key : SparkPass.Crypto.MLKEM.Public_Key;
      Secret_Key : SparkPass.Crypto.MLKEM.Secret_Key;
      Ciphertext : SparkPass.Crypto.MLKEM.Ciphertext;
      Shared_Secret : SparkPass.Crypto.MLKEM.Shared_Key;
      Expected_SS : SparkPass.Crypto.MLKEM.Shared_Key;
      Success : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== NIST Test Vector #{test_num} (count={vector['count']}) ===");

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
   end Test_NIST_Vector_{test_num};
"""

def format_ada_test_vector_mldsa(vector, test_num):
    """Format ML-DSA test vector as Ada procedure"""
    # Extract signature from signed message (sm = sig || msg)
    sm_len = len(vector['sm']) // 2  # hex string, so divide by 2 for bytes
    msg_len = int(vector['mlen'])
    sig_len = sm_len - msg_len
    sig_hex = vector['sm'][:sig_len * 2]  # First part is signature

    return f"""
   --  NIST KAT Test Vector #{test_num} from kat_MLDSA_87_det_pure.rsp (count={vector['count']})
   procedure Test_NIST_Vector_{test_num} is
      --  Test vector from official NIST post-quantum-cryptography/KAT repository
      --  Source: https://github.com/post-quantum-cryptography/KAT/blob/main/MLDSA/kat_MLDSA_87_det_pure.rsp

      PK_Hex : constant String := "{vector['pk'][:200]}..."  -- truncated for readability
         & "{vector['pk'][-100:]}";
      SK_Hex : constant String := "{vector['sk'][:200]}..."  -- truncated for readability
         & "{vector['sk'][-100:]}";
      Msg_Hex : constant String := "{vector['msg']}";
      Sig_Hex : constant String := "{sig_hex[:200]}..."  -- truncated for readability
         & "{sig_hex[-100:]}";

      Public_Key : SparkPass.Crypto.MLDSA.Public_Key;
      Secret_Key : SparkPass.Crypto.MLDSA.Secret_Key;
      Message : Byte_Array := Hex_To_Bytes (Msg_Hex);
      Signature : SparkPass.Crypto.MLDSA.Signature;
      Expected_Sig : SparkPass.Crypto.MLDSA.Signature;
      Generated_Sig : SparkPass.Crypto.MLDSA.Signature;
      Verified : Boolean;
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("=== NIST Test Vector #{test_num} (count={vector['count']}) ===");

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
   end Test_NIST_Vector_{test_num};
"""

# Main execution
if __name__ == "__main__":
    import os

    # Extract ML-KEM vectors
    mlkem_file = "/Users/sicarii/SparkPass/test/nist_vectors/kat_MLKEM_1024.rsp"
    if os.path.exists(mlkem_file):
        print("Extracting ML-KEM-1024 test vectors...")
        mlkem_vectors = extract_mlkem_vectors(mlkem_file, 3)
        print(f"Found {len(mlkem_vectors)} ML-KEM test vectors")

        # Save formatted Ada code
        with open("/Users/sicarii/SparkPass/test/nist_mlkem_vectors.ada", "w") as f:
            for i, vector in enumerate(mlkem_vectors, 1):
                f.write(format_ada_test_vector_mlkem(vector, i))
        print("Saved to nist_mlkem_vectors.ada")

    # Extract ML-DSA vectors
    mldsa_file = "/Users/sicarii/SparkPass/test/nist_vectors/kat_MLDSA_87_det_pure.rsp"
    if os.path.exists(mldsa_file):
        print("\nExtracting ML-DSA-87 test vectors...")
        mldsa_vectors = extract_mldsa_vectors(mldsa_file, 3)
        print(f"Found {len(mldsa_vectors)} ML-DSA test vectors")

        # Save formatted Ada code
        with open("/Users/sicarii/SparkPass/test/nist_mldsa_vectors.ada", "w") as f:
            for i, vector in enumerate(mldsa_vectors, 1):
                f.write(format_ada_test_vector_mldsa(vector, i))
        print("Saved to nist_mldsa_vectors.ada")

    # Print summary
    print("\n=== Summary ===")
    if mlkem_vectors:
        for i, v in enumerate(mlkem_vectors, 1):
            print(f"ML-KEM Vector {i}: count={v['count']}, pk={len(v['pk'])}chars, sk={len(v['sk'])}chars")
    if mldsa_vectors:
        for i, v in enumerate(mldsa_vectors, 1):
            print(f"ML-DSA Vector {i}: count={v['count']}, msg_len={v['mlen']}, sm_len={len(v['sm'])//2}")