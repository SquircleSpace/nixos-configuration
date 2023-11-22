{ self, mkNixosSystem, nixpkgs2305, rss4email }:
mkNixosSystem {
  nixpkgs = nixpkgs2305;
  system = "aarch64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.ada
    self.nixosModules.tailscale
    rss4email.nixosModule

    ./system.nix
    ./hardware.nix
  ];
}
