class Edit < Formula
  desc "MS-DOS style modern CLI editor"
  homepage "https://github.com/sichy/edit"
  version "1.2.1"
  
  if Hardware::CPU.intel?
    url "https://github.com/microsoft/edit/releases/download/v#{version}/edit-x86_64-macos.tar.gz"
    sha256 "REPLACE_WITH_ACTUAL_SHA256_FOR_X86_64" # Will be calculated after first release
  elsif Hardware::CPU.arm?
    url "https://github.com/microsoft/edit/releases/download/v#{version}/edit-aarch64-macos.tar.gz"
    sha256 "REPLACE_WITH_ACTUAL_SHA256_FOR_ARM64" # Will be calculated after first release
  end

  def install
    bin.install "edit"
  end

  test do
    # Test that the binary runs and shows help
    system "#{bin}/edit", "--help"
  end
end
