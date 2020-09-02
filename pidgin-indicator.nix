{ stdenv, fetchFromGitHub, pidgin, libappindicator-gtk2, intltool, pkgconfig, autoreconfHook }:
let
  version = "1.0.1";
in
stdenv.mkDerivation rec {
  name = "pidgin-indicator-${version}";
  inherit version;

  src = fetchFromGitHub {
    owner = "philipl";
    repo = "pidgin-indicator";
    rev = version;
    sha256 = "1wyv8ylh9k1db45lpb5z0qajl9vnna8s8jjhdn8n02my9dlkzl09";
  };

  nativeBuildInputs = [ pkgconfig autoreconfHook ];
  buildInputs = [ pidgin libappindicator-gtk2 intltool ];
}
