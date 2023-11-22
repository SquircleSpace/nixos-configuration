{ self, mkNixosSystem, nixpkgs2305 }:
mkNixosSystem {
  nixpkgs = nixpkgs2305;
  system = "aarch64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.ada
    self.nixosModules.tailscale

    ./system.nix
    ./hardware.nix
  ];
}
