{ linuxPackages, pkgs }:
pkgs.stdenv.mkDerivation rec {
  version = "git";
  name = "applespi-${version}";
  src = pkgs.fetchFromGitHub {
    owner = "cb22";
    repo = "macbook12-spi-driver";
    rev = "c1063578569e7435157752a5515187b099d6e165";
    sha256 = "18mfj135w1qv51kv37rb7s3i9f7vnjnanys75yxrjlqw9bdgp8c2";
  };

  hardeningDisable = [ "pic" ];

  nativeBuildInputs = linuxPackages.kernel.moduleBuildDependencies;
  buildPhase = ''
    make KDIR="${linuxPackages.kernel.dev}/lib/modules/${linuxPackages.kernel.modDirVersion}/build";
  '';
  installPhase = ''
    INSTALL_MOD_PATH=$out make KDIR="${linuxPackages.kernel.dev}/lib/modules/${linuxPackages.kernel.modDirVersion}/build" install
  '';
}
