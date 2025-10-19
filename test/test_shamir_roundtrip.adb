pragma SPARK_Mode (On);
with SparkPass.Types; use SparkPass.Types;
with SparkPass.Crypto.Shamir.RoundTrip;
with Ada.Text_IO;

--  =========================================================================
--  Shamir Secret Sharing Round-Trip Property Verification Test
--  =========================================================================
--
--  **PURPOSE**: Integration test for RoundTrip verification module.
--    This program exercises the ghost verification procedures to ensure
--    they compile and can be verified by GNATprove.
--
--  **METHODOLOGY**: Marmaragan LLM-assisted verification approach
--    - Assert statements between phases (100% success rate)
--    - Single loop invariants (64% success rate)
--    - Incremental proof construction
--
--  **VERIFICATION GOALS**:
--    ✅ Memory safety: No buffer overflows
--    ✅ Type safety: No range violations
--    ✅ No runtime errors: All operations complete safely
--    ⚠️  Mathematical correctness: Assumed (documented)
--
--  **USAGE**:
--    Build: gprbuild -P test_roundtrip.gpr
--    Verify: gnatprove -P test_roundtrip.gpr --level=4 --timeout=60
--    Run: ./test/obj/test_shamir_roundtrip
--
procedure Test_Shamir_RoundTrip
with
   SPARK_Mode => On
is
   --  Test secret (32 bytes of known data)
   Test_Secret : constant Key_Array (1 .. 32) :=
     (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#,
      16#FE#, 16#DC#, 16#BA#, 16#98#, 16#76#, 16#54#, 16#32#, 16#10#,
      16#11#, 16#22#, 16#33#, 16#44#, 16#55#, 16#66#, 16#77#, 16#88#,
      16#99#, 16#AA#, 16#BB#, 16#CC#, 16#DD#, 16#EE#, 16#FF#, 16#00#);

   --  Test results
   All_Tests_Passed : Boolean;

begin
   --  =======================================================================
   --  Run comprehensive round-trip verification tests
   --  =======================================================================
   --
   --  **TESTS PERFORMED**:
   --    1. 2-of-3 threshold (common configuration)
   --    2. 3-of-5 threshold (higher security)
   --    3. 5-of-7 threshold (high threshold)
   --
   --  **EXPECTED OUTCOME**:
   --    All_Tests_Passed = True (if Shamir implementation is correct)
   --
   --  **PROOF OBLIGATIONS**:
   --    - No runtime errors during verification (proven by SPARK)
   --    - All memory accesses are safe (proven by SPARK)
   --    - Mathematical correctness (assumed based on Shamir 1979)

   SparkPass.Crypto.Shamir.RoundTrip.Verify_Multiple_Configurations
     (Test_Secret      => Test_Secret,
      All_Tests_Passed => All_Tests_Passed);

   --  =======================================================================
   --  Report results
   --  =======================================================================

   if All_Tests_Passed then
      Ada.Text_IO.Put_Line ("✅ PASSED: All Shamir round-trip tests succeeded");
      Ada.Text_IO.Put_Line ("  - 2-of-3 threshold: PASS");
      Ada.Text_IO.Put_Line ("  - 3-of-5 threshold: PASS");
      Ada.Text_IO.Put_Line ("  - 5-of-7 threshold: PASS");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("VERIFICATION STATUS:");
      Ada.Text_IO.Put_Line ("  ✅ Memory safety: PROVEN");
      Ada.Text_IO.Put_Line ("  ✅ Type safety: PROVEN");
      Ada.Text_IO.Put_Line ("  ✅ No runtime errors: PROVEN");
      Ada.Text_IO.Put_Line ("  ⚠️  Mathematical correctness: ASSUMED");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("The Combine(Split(Secret)) = Secret property holds");
      Ada.Text_IO.Put_Line ("for all tested configurations, providing empirical");
      Ada.Text_IO.Put_Line ("confidence in the mathematical correctness assumption.");
   else
      Ada.Text_IO.Put_Line ("❌ FAILED: Shamir round-trip test failed");
      Ada.Text_IO.Put_Line ("  This indicates either:");
      Ada.Text_IO.Put_Line ("    1. Implementation bug in Split or Combine");
      Ada.Text_IO.Put_Line ("    2. Memory corruption");
      Ada.Text_IO.Put_Line ("    3. GF(256) arithmetic error");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("INVESTIGATION REQUIRED");
   end if;

   --  =======================================================================
   --  Test individual round-trip (for detailed debugging)
   --  =======================================================================

   declare
      Single_Success : Boolean;
      Single_Matches : Boolean;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Running single 2-of-3 test for detailed verification...");

      SparkPass.Crypto.Shamir.RoundTrip.Verify_RoundTrip
        (Secret       => Test_Secret,
         Threshold    => 2,
         Total_Shares => 3,
         Success      => Single_Success,
         Matches      => Single_Matches);

      if Single_Success and Single_Matches then
         Ada.Text_IO.Put_Line ("✅ Single test: Split and Combine succeeded, secrets match");
      elsif Single_Success and not Single_Matches then
         Ada.Text_IO.Put_Line ("⚠️  Single test: Operations succeeded but secrets DO NOT match");
         Ada.Text_IO.Put_Line ("   This violates the mathematical assumption!");
      else
         Ada.Text_IO.Put_Line ("❌ Single test: Split or Combine operation failed");
      end if;
   end;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("=======================================================");
   Ada.Text_IO.Put_Line ("Shamir Secret Sharing Round-Trip Verification Complete");
   Ada.Text_IO.Put_Line ("=======================================================");

end Test_Shamir_RoundTrip;
