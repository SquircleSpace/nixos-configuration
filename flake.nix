{
  inputs.dwarffs.url = "github:edolstra/dwarffs";
  inputs.nixpkgs2105.url = "github:NixOS/nixpkgs/nixos-21.05";
  inputs.nixpkgs2111.url = "github:NixOS/nixpkgs/nixos-21.11";
  inputs.nix.url = "github:NixOS/nix";
  inputs.rss4email.url = "github:SquircleSpace/rss4email";
  inputs.rss4email.inputs.nixpkgs.follows = "nixpkgs2105";

  outputs = { self, nixpkgs2105, nixpkgs2111, dwarffs, nix, rss4email }:
    let
      revisionModule = nixpkgs: {...}: {
        system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      };
      nixpkgsPinModule = nixpkgs: {...}: {
        nix.registry.nixpkgs.flake = nixpkgs;
      };
      nixosSystem = nixpkgs: cfg: nixpkgs.lib.nixosSystem (cfg // {
        modules = cfg.modules ++ [
          (revisionModule nixpkgs)
          (nixpkgsPinModule nixpkgs)
          rss4email.nixosModule
        ];
      });
    in {
      nixosConfigurations.Jobe = nixosSystem nixpkgs2111 {
        system = "x86_64-linux";
        modules = [
          ./jobe.nix
          dwarffs.nixosModules.dwarffs
          { nixpkgs.overlays = [ nix.overlay ]; }
        ];
      };

      nixosConfigurations.Libbie = nixosSystem nixpkgs2105 {
        system = "x86_64-linux";
        modules = [
          ./libbie.nix
          dwarffs.nixosModules.dwarffs
          { nixpkgs.overlays = [ nix.overlay ]; }
        ];
      };

      nixosConfigurations.pifer = nixosSystem nixpkgs2105 {
        system = "aarch64-linux";
        modules = [ ./pifer.nix ];
      };
    };
}
