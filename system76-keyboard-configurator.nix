{ pkg-config, lib, rustPlatform, openssl, fetchFromGitHub, gtk3, hidapi, libudev }:

rustPlatform.buildRustPackage rec {
  pname = "keyboard-configurator";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "pop-os";
    repo = "keyboard-configurator";
    rev = "666266c60c89186b829efdfb061f7cde1271deb5";
    sha256 = "0hrbiwks14pp285mj5a6qsxdy9lxnpl56pscpvy49zwwz4s9fl09";
  };

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ gtk3 hidapi libudev ];
  cargoSha256 = "0rkirwhkffvlidima2b537yq7z2pnbcdbkm00k9qj8fwq09wls22";

  RUST_BACKTRACE = "full";

  meta = {
    description = "Graphical UI for managing System76 keyboard configurations";
    homepage = "https://github.com/pop-os/keyboard-configurator/";
    license = lib.licenses.gpl3;
    platforms =  [ "x86_64-linux" ];
  };
}
