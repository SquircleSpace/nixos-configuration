{ self, mkNixosSystem, nixpkgs2305 }:
mkNixosSystem {
  nixpkgs = nixpkgs2305;
  system = "x86_64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.gnome
    self.nixosModules.oldAda
    self.nixosModules.photosync
    self.nixosModules.tailscale

    ./system.nix
    ./hardware.nix
  ];
}
