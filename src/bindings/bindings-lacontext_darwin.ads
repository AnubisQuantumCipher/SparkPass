pragma SPARK_Mode (Off);
--  LocalAuthentication framework bindings for macOS
--  Provides Touch ID / Face ID authentication via LAContext

with System;
with Interfaces.C;
with Interfaces.C.Strings;

package Bindings.LAContext_Darwin is

   --  Opaque handle to LAContext
   type LAContext_Handle is new System.Address;

   --  Authentication policy types
   type LAPolicy is new Interfaces.C.int;

   --  Biometric authentication (Touch ID or Face ID)
   LAPolicy_DeviceOwnerAuthenticationWithBiometrics : constant LAPolicy := 1;

   --  Biometric or passcode authentication
   LAPolicy_DeviceOwnerAuthentication : constant LAPolicy := 2;

   --  Error codes returned by LAContext
   type LAError is new Interfaces.C.int;

   LAError_Success               : constant LAError := 0;
   LAError_AuthenticationFailed  : constant LAError := -1;
   LAError_UserCancel            : constant LAError := -2;
   LAError_UserFallback          : constant LAError := -3;
   LAError_SystemCancel          : constant LAError := -4;
   LAError_PasscodeNotSet        : constant LAError := -5;
   LAError_BiometryNotAvailable  : constant LAError := -6;
   LAError_BiometryNotEnrolled   : constant LAError := -7;
   LAError_BiometryLockout       : constant LAError := -8;
   LAError_AppCancel             : constant LAError := -9;
   LAError_InvalidContext        : constant LAError := -10;
   LAError_NotInteractive        : constant LAError := -1004;

   --  Create a new LAContext
   --  Returns: Handle to LAContext (must be freed with LAContext_Release)
   function LAContext_Create return LAContext_Handle
   with Import, Convention => C, External_Name => "lacontext_create";

   --  Release (deallocate) an LAContext
   --  Context: Handle returned by LAContext_Create
   procedure LAContext_Release (Context : LAContext_Handle)
   with Import, Convention => C, External_Name => "lacontext_release";

   --  Check if a policy can be evaluated
   --  Context: LAContext handle
   --  Policy: Authentication policy to check
   --  Error: Out parameter for error code (if check fails)
   --  Returns: 1 if policy can be evaluated, 0 otherwise
   function LAContext_CanEvaluatePolicy
     (Context : LAContext_Handle;
      Policy  : LAPolicy;
      Error   : access LAError) return Interfaces.C.int
   with Import, Convention => C, External_Name => "lacontext_can_evaluate_policy";

   --  Evaluate a policy synchronously (blocks until user authenticates)
   --  This is the KEY function that makes LAContext work in CLI tools.
   --
   --  Context: LAContext handle
   --  Policy: Authentication policy to evaluate
   --  Reason: Localized string shown to user (e.g., "Unlock SparkPass")
   --  Returns: 1 if authentication succeeded, 0 if failed/cancelled
   --
   --  IMPORTANT: This function uses CFRunLoop internally to block execution
   --  until the biometric authentication completes. This is necessary because
   --  the standard evaluatePolicy is async and would exit immediately in a
   --  command-line context.
   function LAContext_EvaluatePolicy_Sync
     (Context : LAContext_Handle;
      Policy  : LAPolicy;
      Reason  : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int
   with Import, Convention => C, External_Name => "lacontext_evaluate_policy_sync";

   --  Get the last error code from an LAContext
   --  Context: LAContext handle
   --  Returns: Most recent error code
   function LAContext_GetError (Context : LAContext_Handle) return LAError
   with Import, Convention => C, External_Name => "lacontext_get_error";

end Bindings.LAContext_Darwin;
