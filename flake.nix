{
  inputs.dwarffs.url = "github:edolstra/dwarffs";
  inputs.nixpkgs2305.url = "github:NixOS/nixpkgs/nixos-23.05";
  inputs.lanzaboote = {
    url = "github:nix-community/lanzaboote/v0.3.0";
    inputs.nixpkgs.follows = "nixpkgs2305";
  };
  inputs.kmonad = {
    url = "github:kmonad/kmonad?dir=nix";
    inputs.nixpkgs.follows = "nixpkgs2305";
  };
  inputs.nix.url = "github:NixOS/nix";
  inputs.rss4email.url = "github:SquircleSpace/rss4email";

  outputs = { self, nixpkgs2305, lanzaboote, kmonad, dwarffs, nix, rss4email }@inputs:
    let
      mkNixosSystem = { nixpkgs, system, modules }: nixpkgs.lib.nixosSystem {
        inherit system modules;
        specialArgs = { inherit self; nixpkgs-flake = nixpkgs; };
      };
      systemContext = inputs // { inherit mkNixosSystem; };
      callNixosSystem = system: let
        fn = if builtins.isFunction system then system else import system;
      in fn (builtins.intersectAttrs (builtins.functionArgs fn) systemContext);
    in {
      nixosModules = rec {
        configurationRevision = { lib, ... }: {
          system.configurationRevision = lib.mkIf (self ? rev) self.rev;
        };
        nixpkgsPinModule = { nixpkgs-flake, ... }: {
          nix.registry.nixpkgs.flake = nixpkgs-flake;
        };

        common = import ./common.nix;
        ssh = import ./ssh.nix;
        backup = import ./backup.nix;
        flakeAutoupdate = import ./flake-autoupdate.nix;
        gnome = import ./gnome.nix;
        kde = import ./kde.nix;
        ada = import ./ada.nix;
        tailscale = import ./tailscale.nix;
        photosync = import ./photosync.nix;

        default = { ... }: {
          imports = [ configurationRevision nixpkgsPinModule common ssh ];
        };
      };

      nixosConfigurations = {
        plasma = callNixosSystem ./nixosSystems/plasma;
        Jobe = callNixosSystem ./nixosSystems/jobe;
        Libbie = callNixosSystem ./nixosSystems/libbie;
        pifer = callNixosSystem ./nixosSystems/pifer;
      };

      adaExtras.backupServers.rsync = import ./server-rsync.net.nix;
      adaExtras.publicKeys = {
        libbie = ./libbie.pub;
        phone = ./phone.pub;
        plasma = ./plasma.pub;
        photosync = ./photosync.pub;
      };
    };
}
