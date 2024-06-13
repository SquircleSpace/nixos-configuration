{ self, mkNixosSystem, nixpkgs }:
mkNixosSystem {
  nixpkgs = nixpkgs;
  system = "aarch64-linux";
  modules = [
    self.nixosModules.default

    ./system.nix
    ./hardware.nix
  ];
}
