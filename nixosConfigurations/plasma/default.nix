{ self, mkNixosSystem, nixpkgs, nixos-hardware, lanzaboote }:
mkNixosSystem {
  nixpkgs = nixpkgs;
  system = "x86_64-linux";
  modules = [
    lanzaboote.nixosModules.lanzaboote
    nixos-hardware.nixosModules.framework-13-7040-amd
    self.nixosModules.default
    self.nixosModules.gnome
    self.nixosModules.photosync

    ./system.nix
    ./hardware.nix
  ];
}
