# SparkPass Homebrew Formula
# To use: brew tap yourusername/sparkpass && brew install sparkpass

class Sparkpass < Formula
  desc "Quantum-resistant password manager with Touch ID/Face ID support"
  homepage "https://github.com/yourusername/sparkpass"
  url "https://github.com/yourusername/sparkpass/releases/download/v1.0.0/sparkpass-1.0.0-macos-arm64.tar.gz"
  sha256 "0000000000000000000000000000000000000000000000000000000000000000"  # Update with actual checksum
  license "MIT"  # Update with your license
  version "1.0.0"

  # Only supports Apple Silicon Macs
  depends_on arch: :arm64
  depends_on :macos => :big_sur

  # Runtime dependencies (Homebrew installs these automatically)
  depends_on "liboqs"
  depends_on "openssl@3"
  depends_on "libsodium"

  def install
    # SparkPass archive contains: sparkpass-VERSION/sparkpass
    # We want to install just the binary to Homebrew's bin directory
    bin.install "sparkpass"
  end

  def caveats
    <<~EOS
      SparkPass has been installed to: #{bin}/sparkpass

      First-time setup:
        sparkpass init

      SparkPass uses Touch ID/Face ID for authentication.
      You'll be prompted to enroll biometrics on first use.

      For help:
        sparkpass --help

      Documentation:
        https://github.com/yourusername/sparkpass
    EOS
  end

  test do
    # Test that the binary runs and has correct dependencies
    assert_match "SparkPass", shell_output("#{bin}/sparkpass --version 2>&1", 0)

    # Verify code signature (if notarized)
    system "codesign", "--verify", "#{bin}/sparkpass"
  end
end
