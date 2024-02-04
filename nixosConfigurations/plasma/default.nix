{ self, mkNixosSystem, nixpkgs2311, nixos-hardware, lanzaboote, kmonad }:
mkNixosSystem {
  nixpkgs = nixpkgs2311;
  system = "x86_64-linux";
  modules = [
    lanzaboote.nixosModules.lanzaboote
    kmonad.nixosModules.default
    nixos-hardware.nixosModules.framework-13-7040-amd
    self.nixosModules.default
    self.nixosModules.kde

    ./system.nix
    ./hardware.nix
  ];
}
