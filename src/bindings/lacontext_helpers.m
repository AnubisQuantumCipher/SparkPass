/*
 * LAContext Helper Functions for SparkPass
 *
 * Provides C wrappers around macOS LocalAuthentication framework
 * for Touch ID / Face ID authentication.
 *
 * KEY INNOVATION: lacontext_evaluate_policy_sync() uses CFRunLoop
 * to block execution until biometric authentication completes.
 * This is critical for command-line tools where the main thread
 * would normally exit before the async callback fires.
 */

#include <LocalAuthentication/LocalAuthentication.h>
#include <CoreFoundation/CoreFoundation.h>
#include <stdbool.h>

// Opaque handle for LAContext
typedef void* LAContext_Handle;

// Error codes (matching LAError from Ada bindings)
typedef enum {
    LAError_Success = 0,
    LAError_AuthenticationFailed = -1,
    LAError_UserCancel = -2,
    LAError_UserFallback = -3,
    LAError_SystemCancel = -4,
    LAError_PasscodeNotSet = -5,
    LAError_BiometryNotAvailable = -6,
    LAError_BiometryNotEnrolled = -7,
    LAError_BiometryLockout = -8,
    LAError_AppCancel = -9,
    LAError_InvalidContext = -10,
    LAError_NotInteractive = -1004
} LAError_Code;

// Create a new LAContext
LAContext_Handle lacontext_create(void) {
    @autoreleasepool {
        LAContext *context = [[LAContext alloc] init];
        if (!context) {
            return NULL;
        }

        // Retain the context so it persists outside autorelease pool
        return (LAContext_Handle)CFBridgingRetain(context);
    }
}

// Release (deallocate) an LAContext
void lacontext_release(LAContext_Handle handle) {
    if (handle) {
        CFBridgingRelease(handle);
    }
}

// Check if a policy can be evaluated
// Returns: 1 if can evaluate, 0 if cannot
int lacontext_can_evaluate_policy(LAContext_Handle handle, int policy, int *error) {
    if (!handle) {
        if (error) {
            *error = LAError_InvalidContext;
        }
        return 0;
    }

    @autoreleasepool {
        LAContext *context = (__bridge LAContext*)handle;
        NSError *nsError = nil;

        BOOL canEvaluate = [context canEvaluatePolicy:(LAPolicy)policy error:&nsError];

        if (error && nsError) {
            *error = (int)nsError.code;
        }

        return canEvaluate ? 1 : 0;
    }
}

// Synchronous wrapper for evaluatePolicy
// This is the CRITICAL function that makes LAContext work in CLI tools!
//
// The trick: Use CFRunLoopRun() to keep the main thread alive until
// the biometric authentication callback completes.
//
// Without this, the main thread would exit immediately because
// evaluatePolicy is async, and the Touch ID prompt would never appear.
//
// Returns: 1 if authentication succeeded, 0 if failed/cancelled
int lacontext_evaluate_policy_sync(LAContext_Handle handle, int policy, const char *reason) {
    if (!handle || !reason) {
        return 0;
    }

    @autoreleasepool {
        LAContext *context = (__bridge LAContext*)handle;
        NSString *nsReason = [NSString stringWithUTF8String:reason];

        __block int result = 0;
        __block BOOL done = NO;

        // Capture the current run loop
        // This is the key to making async operations work in CLI tools
        CFRunLoopRef runLoop = CFRunLoopGetCurrent();

        // Start the async authentication
        [context evaluatePolicy:(LAPolicy)policy
                 localizedReason:nsReason
                           reply:^(BOOL success, NSError *error) {
            // This callback runs on a private queue
            result = success ? 1 : 0;
            done = YES;

            // Stop the run loop when authentication completes
            // This allows the blocking call below to return
            CFRunLoopStop(runLoop);
        }];

        // Keep the run loop running until the callback fires
        // This blocks the main thread, preventing early exit
        // Poll every 100ms to check if done (in case CFRunLoopStop fails)
        while (!done) {
            // Run the run loop for a short time
            // This processes events (including the Touch ID prompt)
            CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.1, YES);
        }

        return result;
    }
}

// Get the last error from an LAContext
// Returns: Error code (0 for success)
int lacontext_get_error(LAContext_Handle handle) {
    // LAContext doesn't store last error in a retrievable way
    // Errors are returned via the callback in evaluatePolicy
    // Return 0 (success) as placeholder
    return 0;
}

/*
 * USAGE NOTES:
 *
 * 1. Always check if biometry is available before calling evaluate:
 *
 *    int error = 0;
 *    if (!lacontext_can_evaluate_policy(ctx, 1, &error)) {
 *        // Touch ID not available
 *        return 0;
 *    }
 *
 * 2. The sync version blocks until user authenticates:
 *
 *    int success = lacontext_evaluate_policy_sync(ctx, 1, "Unlock vault");
 *    if (success) {
 *        // User authenticated with Touch ID
 *    } else {
 *        // User cancelled or authentication failed
 *    }
 *
 * 3. Always release the context when done:
 *
 *    lacontext_release(ctx);
 *
 * 4. For command-line tools, ALWAYS use the _sync version.
 *    The standard async evaluatePolicy will not work because
 *    the main thread will exit before the callback fires.
 */
