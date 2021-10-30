{
  inputs.dwarffs.url = "github:edolstra/dwarffs";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  inputs.nix.url = "github:NixOS/nix";
  inputs.rss4email.url = "github:SquircleSpace/rss4email";
  inputs.rss4email.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, dwarffs, nix, rss4email }:
    let
      revisionModule = {...}: {
        system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      };
      nixpkgsPinModule = {...}: {
        nix.registry.nixpkgs.flake = nixpkgs;
      };
      nixosSystem = cfg: nixpkgs.lib.nixosSystem (cfg // {
        modules = cfg.modules ++ [ revisionModule nixpkgsPinModule rss4email.nixosModule ];
      });
    in {
      nixosConfigurations.Jobe = nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./jobe.nix
          dwarffs.nixosModules.dwarffs
          { nixpkgs.overlays = [ nix.overlay ]; }
        ];
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
