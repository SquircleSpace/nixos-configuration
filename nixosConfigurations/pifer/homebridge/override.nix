{pkgs ? import <nixpkgs> {
    inherit system;
}, system ? builtins.currentSystem}:

let
  nodePackages = import ./default.nix {
    inherit pkgs system;
  };
in
nodePackages // {
  homebridge = nodePackages.homebridge.override {
    buildInputs = [ pkgs.openssl ];
  };
}
