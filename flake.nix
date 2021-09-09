{
  inputs.dwarffs.url = "github:edolstra/dwarffs";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  inputs.nix.url = "github:NixOS/nix";

  outputs = { self, nixpkgs, dwarffs, nix }:
    let
      revisionModule = {...}: {
        system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      };
      nixpkgsPinModule = {...}: {
        nix.registry.nixpkgs.flake = nixpkgs;
      };
      nixosSystem = cfg: nixpkgs.lib.nixosSystem (cfg // {
        modules = cfg.modules ++ [ revisionModule nixpkgsPinModule ];
      });
    in {
      nixosConfigurations.Jobe = nixosSystem {
        system = "x86_64-linux";
        modules = [ ./jobe.nix ];
      };

      nixosConfigurations.Libbie = nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./libbie.nix
          dwarffs.nixosModules.dwarffs
          { nixpkgs.overlays = [ nix.overlay ]; }
        ];
      };

      nixosConfigurations.pifer = nixosSystem {
        system = "aarch64-linux";
        modules = [ ./pifer.nix ];
      };
    };
}
