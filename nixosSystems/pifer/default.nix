{ self, mkNixosSystem, nixpkgs2205 }:
mkNixosSystem {
  nixpkgs = nixpkgs2205;
  system = "aarch64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.ada
    self.nixosModules.tailscale

    ./system.nix
    ./hardware.nix
  ];
}
