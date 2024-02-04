{ self, mkNixosSystem, nixpkgs2311 }:
mkNixosSystem {
  nixpkgs = nixpkgs2311;
  system = "x86_64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.gnome
    self.nixosModules.photosync

    ./system.nix
    ./hardware.nix
  ];
}
