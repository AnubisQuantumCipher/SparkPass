#!/bin/bash

# Extract single test vectors for Ada integration

# ML-KEM vector 0
echo "Extracting ML-KEM test vector 0..."
awk '/^count = 0$/,/^count = 1$/' nist_vectors/kat_MLKEM_1024.rsp | grep "^pk =" | cut -d'=' -f2 | tr -d ' ' > mlkem_pk_0.hex
awk '/^count = 0$/,/^count = 1$/' nist_vectors/kat_MLKEM_1024.rsp | grep "^sk =" | cut -d'=' -f2 | tr -d ' ' > mlkem_sk_0.hex
awk '/^count = 0$/,/^count = 1$/' nist_vectors/kat_MLKEM_1024.rsp | grep "^ct =" | cut -d'=' -f2 | tr -d ' ' > mlkem_ct_0.hex
awk '/^count = 0$/,/^count = 1$/' nist_vectors/kat_MLKEM_1024.rsp | grep "^ss =" | cut -d'=' -f2 | tr -d ' ' > mlkem_ss_0.hex

# ML-DSA vector 0
echo "Extracting ML-DSA test vector 0..."
awk '/^count = 0$/,/^count = 1$/' nist_vectors/kat_MLDSA_87_det_pure.rsp | grep "^pk =" | cut -d'=' -f2 | tr -d ' ' > mldsa_pk_0.hex
awk '/^count = 0$/,/^count = 1$/' nist_vectors/kat_MLDSA_87_det_pure.rsp | grep "^sk =" | cut -d'=' -f2 | tr -d ' ' > mldsa_sk_0.hex
awk '/^count = 0$/,/^count = 1$/' nist_vectors/kat_MLDSA_87_det_pure.rsp | grep "^msg =" | cut -d'=' -f2 | tr -d ' ' > mldsa_msg_0.hex
awk '/^count = 0$/,/^count = 1$/' nist_vectors/kat_MLDSA_87_det_pure.rsp | grep "^sm =" | cut -d'=' -f2 | tr -d ' ' > mldsa_sm_0.hex
awk '/^count = 0$/,/^count = 1$/' nist_vectors/kat_MLDSA_87_det_pure.rsp | grep "^mlen =" | cut -d'=' -f2 | tr -d ' ' > mldsa_mlen_0.txt

echo "Done! Test vectors extracted to .hex files"