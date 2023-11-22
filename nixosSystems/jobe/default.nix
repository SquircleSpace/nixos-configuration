{ self, mkNixosSystem, nixpkgs2205 }:
mkNixosSystem {
  nixpkgs = nixpkgs2205;
  system = "x86_64-linux";
  modules = [
    self.nixosModules.default
    self.nixosModules.gnome
    self.nixosModules.ada
    self.nixosModules.photosync
    self.nixosModules.tailscale
    ./boot-windows

    ./system.nix
    ./hardware.nix
  ];
}
