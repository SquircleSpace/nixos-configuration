{ self, mkNixosSystem, nixpkgs }:
mkNixosSystem {
  nixpkgs = nixpkgs;
  system = "x86_64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.gnome

    ./system.nix
    ./hardware.nix
  ];
}
