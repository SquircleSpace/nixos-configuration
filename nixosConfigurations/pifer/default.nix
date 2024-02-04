{ self, mkNixosSystem, nixpkgs, rss4email }:
mkNixosSystem {
  nixpkgs = nixpkgs;
  system = "aarch64-linux";
  modules = [
    self.nixosModules.default
    rss4email.nixosModule

    ./system.nix
    ./hardware.nix
  ];
}
