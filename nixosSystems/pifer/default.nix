{ self, mkNixosSystem, nixpkgs2211, nixpkgs2305, rss4email }:
mkNixosSystem {
  nixpkgs = nixpkgs2305;
  system = "aarch64-linux";
  specialArgs = { nixpkgs2211-flake = nixpkgs2211; };
  modules = [
    self.nixosModules.default
    self.nixosModules.tailscale
    rss4email.nixosModule

    ./system.nix
    ./hardware.nix
  ];
}
