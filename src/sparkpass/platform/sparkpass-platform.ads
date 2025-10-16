pragma SPARK_Mode (On);

--  Platform-specific integrations for SparkPass
--
--  This package provides abstractions for OS-specific features like:
--    - Keychain/credential storage (macOS Keychain, Windows Credential Manager, Linux Secret Service)
--    - Biometric authentication (Touch ID, Windows Hello, PAM)
--    - Security event notifications (screen lock, sleep, etc.)
--
package SparkPass.Platform is
   pragma Preelaborate;
end SparkPass.Platform;
