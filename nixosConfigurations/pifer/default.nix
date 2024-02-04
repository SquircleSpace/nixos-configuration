{ self, mkNixosSystem, nixpkgs2311, rss4email }:
mkNixosSystem {
  nixpkgs = nixpkgs2311;
  system = "aarch64-linux";
  modules = [
    self.nixosModules.default
    rss4email.nixosModule

    ./system.nix
    ./hardware.nix
  ];
}
