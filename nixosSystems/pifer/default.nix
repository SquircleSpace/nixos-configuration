{ self, mkNixosSystem, nixpkgs2211 }:
mkNixosSystem {
  nixpkgs = nixpkgs2211;
  system = "aarch64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.ada
    self.nixosModules.tailscale

    ./system.nix
    ./hardware.nix
  ];
}
