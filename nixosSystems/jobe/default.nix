{ self, mkNixosSystem, nixpkgs2211 }:
mkNixosSystem {
  nixpkgs = nixpkgs2211;
  system = "x86_64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.gnome
    self.nixosModules.ada
    self.nixosModules.photosync
    self.nixosModules.tailscale

    ./system.nix
    ./hardware.nix
  ];
}
