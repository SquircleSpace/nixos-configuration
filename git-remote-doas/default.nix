{ stdenv, ghc, ... }:
stdenv.mkDerivation rec {
  name = "git-remote-doas-${version}";
  version = "1.0.0";
  src = ./.;
  buildPhase = ''
    ${ghc}/bin/ghc -o git-remote-doas Main.hs
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp git-remote-doas $out/bin
  '';
}
