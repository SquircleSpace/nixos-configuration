{ self, mkNixosSystem, nixpkgs, nixos-hardware, lanzaboote, kmonad }:
mkNixosSystem {
  nixpkgs = nixpkgs;
  system = "x86_64-linux";
  modules = [
    lanzaboote.nixosModules.lanzaboote
    kmonad.nixosModules.default
    nixos-hardware.nixosModules.framework-13-7040-amd
    self.nixosModules.default
    self.nixosModules.kde
    self.nixosModules.photosync

    ./system.nix
    ./hardware.nix
  ];
}
