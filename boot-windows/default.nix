{ stdenv }:
stdenv.mkDerivation rec {
  name = "boot-windows-${version}";
  version = "1.0.0";
  src = ./.;
  buildPhase = ''
    g++ boot-windows.cpp -o boot-windows
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp boot-windows $out/bin
  '';
}
