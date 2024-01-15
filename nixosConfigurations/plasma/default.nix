{ self, mkNixosSystem, nixpkgs2311, lanzaboote, kmonad }:
mkNixosSystem {
  nixpkgs = nixpkgs2311;
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
