pragma SPARK_Mode (On);
with System;
with Interfaces.C;
with Interfaces.C.Strings;

package Bindings.LibOQS is
   pragma Preelaborate;

   type OQS_KEM is record
      Method_Name          : Interfaces.C.Strings.chars_ptr;
      Alg_Version          : Interfaces.C.Strings.chars_ptr;
      Claimed_Nist_Level   : Interfaces.C.int;
      Ind_CCA              : Interfaces.C.int;
      Length_Public_Key    : Interfaces.C.size_t;
      Length_Secret_Key    : Interfaces.C.size_t;
      Length_Ciphertext    : Interfaces.C.size_t;
      Length_Shared_Secret : Interfaces.C.size_t;
   end record;
   pragma Convention (C, OQS_KEM);

   type Kem_Handle is access all OQS_KEM;

   function OQS_KEM_New (Alg_Name : Interfaces.C.Strings.chars_ptr) return Kem_Handle;
   pragma Import (C, OQS_KEM_New, "OQS_KEM_new");

   procedure OQS_KEM_Free (Kem : Kem_Handle);
   pragma Import (C, OQS_KEM_Free, "OQS_KEM_free");

   function OQS_KEM_Keypair
     (Kem        : Kem_Handle;
      Public_Key : System.Address;
      Secret_Key : System.Address) return Interfaces.C.int;
   pragma Import (C, OQS_KEM_Keypair, "OQS_KEM_keypair");

   function OQS_KEM_Encaps
     (Kem        : Kem_Handle;
      Ciphertext : System.Address;
      Shared_Key : System.Address;
      Public_Key : System.Address) return Interfaces.C.int;
   pragma Import (C, OQS_KEM_Encaps, "OQS_KEM_encaps");

   function OQS_KEM_Decaps
     (Kem        : Kem_Handle;
      Shared_Key : System.Address;
      Ciphertext : System.Address;
      Secret_Key : System.Address) return Interfaces.C.int;
   pragma Import (C, OQS_KEM_Decaps, "OQS_KEM_decaps");

   type OQS_SIG is record
      Method_Name        : Interfaces.C.Strings.chars_ptr;
      Alg_Version        : Interfaces.C.Strings.chars_ptr;
      Claimed_Nist_Level : Interfaces.C.int;
      Euf_Cma            : Interfaces.C.int;
      Length_Public_Key  : Interfaces.C.size_t;
      Length_Secret_Key  : Interfaces.C.size_t;
      Length_Signature   : Interfaces.C.size_t;
   end record;
   pragma Convention (C, OQS_SIG);

   type Sig_Handle is access all OQS_SIG;

   function OQS_SIG_New (Alg_Name : Interfaces.C.Strings.chars_ptr) return Sig_Handle;
   pragma Import (C, OQS_SIG_New, "OQS_SIG_new");

   procedure OQS_SIG_Free (Sig : Sig_Handle);
   pragma Import (C, OQS_SIG_Free, "OQS_SIG_free");

   function OQS_SIG_Keypair
     (Sig        : Sig_Handle;
      Public_Key : System.Address;
      Secret_Key : System.Address) return Interfaces.C.int;
   pragma Import (C, OQS_SIG_Keypair, "OQS_SIG_keypair");

   function OQS_SIG_Sign
     (Sig         : Sig_Handle;
      Signature   : System.Address;
      Sig_Len     : access Interfaces.C.size_t;
      Message     : System.Address;
      Message_Len : Interfaces.C.size_t;
      Secret_Key  : System.Address) return Interfaces.C.int;
   pragma Import (C, OQS_SIG_Sign, "OQS_SIG_sign");

   function OQS_SIG_Verify
     (Sig         : Sig_Handle;
      Message     : System.Address;
      Message_Len : Interfaces.C.size_t;
      Signature   : System.Address;
      Sig_Len     : Interfaces.C.size_t;
      Public_Key  : System.Address) return Interfaces.C.int;
   pragma Import (C, OQS_SIG_Verify, "OQS_SIG_verify");

end Bindings.LibOQS;
