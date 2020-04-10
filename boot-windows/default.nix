{ stdenv, efibootmgr, ... }:
stdenv.mkDerivation rec {
  name = "boot-windows-${version}";
  version = "1.0.0";
  src = ./.;
  inherit efibootmgr;
  patchPhase = ''
    substituteInPlace boot-windows.cpp --subst-var efibootmgr
  '';
  buildPhase = ''
    g++ boot-windows.cpp -o boot-windows
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp boot-windows $out/bin
  '';
}
