{
  inputs.dwarffs.url = "github:edolstra/dwarffs";
  inputs.dwarffs.inputs.nixpkgs.follows = "nixpkgs2111";
  inputs.nixpkgs2111.url = "github:NixOS/nixpkgs/nixos-21.11";
  inputs.nixpkgs2205.url = "github:NixOS/nixpkgs/nixos-22.05";
  inputs.nixpkgs2305.url = "github:NixOS/nixpkgs/nixos-23.05";
  inputs.nix.url = "github:NixOS/nix";
  inputs.rss4email.url = "github:SquircleSpace/rss4email";

  outputs = { self, nixpkgs2111, nixpkgs2205, nixpkgs2305, dwarffs, nix, rss4email }:
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
      nixosConfigurations.Jobe = nixosSystem nixpkgs2205 {
        system = "x86_64-linux";
        modules = [
          ./jobe.nix
        ];
      };

      nixosConfigurations.Libbie = nixosSystem nixpkgs2205 {
        system = "x86_64-linux";
        modules = [
          ./libbie.nix
        ];
      };

      nixosConfigurations.pifer = nixosSystem nixpkgs2205 {
        system = "aarch64-linux";
        modules = [ ./pifer.nix ];
      };
      nixosConfigurations.plasma = nixosSystem nixpkgs2305 {
        system = "x86_64-linux";
	modules = [ ./plasma.nix ];
      };
    };
}
