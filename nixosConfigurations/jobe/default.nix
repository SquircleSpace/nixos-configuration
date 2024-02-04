{ self, mkNixosSystem, nixpkgs }:
mkNixosSystem {
  nixpkgs = nixpkgs;
  system = "x86_64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.gnome
    self.nixosModules.photosync

    ./system.nix
    ./hardware.nix
  ];
}
