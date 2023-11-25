{
  inputs.agenix.url = "github:ryantm/agenix";
  inputs.dwarffs.url = "github:edolstra/dwarffs";
  inputs.nixpkgs2211.url = "github:NixOS/nixpkgs/nixos-22.11";
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

  outputs = {
    self,
    nixpkgs2211,
    nixpkgs2305,
    lanzaboote,
    kmonad,
    dwarffs,
    nix,
    rss4email,
    agenix
  }@inputs:
    let
      mkNixosSystem = { nixpkgs, system, modules, specialArgs ? {} }: nixpkgs.lib.nixosSystem {
        inherit system modules;
        specialArgs = { inherit self; nixpkgs-flake = nixpkgs; } // specialArgs;
      };
      systemContext = inputs // { inherit mkNixosSystem; };
      callNixosSystem = system: let
        fn = if builtins.isFunction system then system else import system;
      in fn (builtins.intersectAttrs (builtins.functionArgs fn) systemContext);

      allSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" "aarch64-darwin" ];
      nixosSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];

      genAttrs = list: fn: builtins.foldl' (l: r: l // r) {} (builtins.map (key: {"${key}" = fn key;}) list);

      runTestWithNixpkgs = system: nixpkgs: test: (import (nixpkgs2305 + "/nixos/lib") {}).runTest {
        imports = [test];
        hostPkgs = nixpkgs.legacyPackages."${system}";
      };
    in {
      checks = genAttrs nixosSystems (system: {
        passwordPriorityOrder = runTestWithNixpkgs system nixpkgs2305 ./checks/passwordPriorityOrder.nix;
      });

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
        oldAda = import ./ada.nix;
        tailscale = import ./tailscale.nix;
        photosync = import ./photosync.nix;
        ada = import ./modules/ada.nix agenix;

        default = { ... }: {
          imports = [ configurationRevision nixpkgsPinModule common ssh ada ];
        };
      };

      nixosConfigurations = {
        plasma = callNixosSystem ./nixosSystems/plasma;
        Jobe = callNixosSystem ./nixosSystems/jobe;
        Libbie = callNixosSystem ./nixosSystems/libbie;
        pifer = callNixosSystem ./nixosSystems/pifer;
      };

      packages = genAttrs allSystems (system: {
        emacs = import ./emacs.nix {
          pkgs = nixpkgs2305.legacyPackages."${system}";
          emacs = nixpkgs2305.legacyPackages."${system}".emacs29;
        };
        emacs-nox = import ./emacs.nix {
          pkgs = nixpkgs2305.legacyPackages."${system}";
          emacs = nixpkgs2305.legacyPackages."${system}".emacs29-nox;
        };

        git-remote-doas = nixpkgs2305.legacyPackages."${system}".callPackage ./git-remote-doas {};
      });

      lib = {
        publicKeys = import ./publicKeys.nix;
        backupServers.rsync = import ./server-rsync.net.nix;
      };
    };
}
