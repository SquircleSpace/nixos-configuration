{ self, mkNixosSystem, nixpkgs2311, nixos-hardware, rss4email }:
mkNixosSystem {
  nixpkgs = nixpkgs2311;
  system = "aarch64-linux";
  modules = [
    nixos-hardware.nixosModules.raspberry-pi-4
    self.nixosModules.default
    self.nixosModules.tailscale
    rss4email.nixosModule

    ./system.nix
    ./hardware.nix
  ];
}
