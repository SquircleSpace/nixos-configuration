{ self, mkNixosSystem, nixpkgs2305, lanzaboote, kmonad }:
mkNixosSystem {
  nixpkgs = nixpkgs2305;
  system = "x86_64-linux";
  modules = [
    lanzaboote.nixosModules.lanzaboote
    kmonad.nixosModules.default
    self.nixosModules.default
    self.nixosModules.kde
    self.nixosModules.tailscale

    ./system.nix
    ./hardware.nix
  ];
}
