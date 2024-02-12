{
  inputs.agenix.url = "github:ryantm/agenix";
  inputs.dwarffs.url = "github:edolstra/dwarffs";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  inputs.nixpkgsDarwin.url = "github:NixOS/nixpkgs/nixpkgs-23.11-darwin";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  inputs.lanzaboote = {
    url = "github:nix-community/lanzaboote/v0.3.0";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.kmonad = {
    url = "github:kmonad/kmonad?dir=nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nix.url = "github:NixOS/nix";
  inputs.rss4email.url = "github:SquircleSpace/rss4email";

  outputs = {
    self,
    nixpkgs,
    nixpkgsDarwin,
    nixos-hardware,
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

      genAttrs = list: fn: builtins.foldl' (l: r: l // r) {} (builtins.map (key: {"${key}" = fn key;}) list);
      nixosSystems = genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ] (key: nixpkgs);
      allSystems = nixosSystems // { "aarch64-darwin" = nixpkgsDarwin; };
      genSystems = systemSet: fn: builtins.mapAttrs fn systemSet;

      runTestWithNixpkgs = system: nixpkgs: test: (import (nixpkgs + "/nixos/lib") {}).runTest {
        imports = [test];
        hostPkgs = nixpkgs.legacyPackages."${system}";
        defaults.nixpkgs.pkgs = nixpkgs.legacyPackages."${system}";
      };
    in {
      checks = genSystems nixosSystems (system: nixpkgs: {
        passwordPriorityOrder = runTestWithNixpkgs system nixpkgs ./checks/passwordPriorityOrder.nix;
        adaModule = runTestWithNixpkgs system nixpkgs (import ./checks/ada.nix self);
      });

      nixosModules = rec {
        configurationRevision = { lib, ... }: {
          system.configurationRevision = lib.mkIf (self ? rev) self.rev;
        };
        nixpkgsPinModule = { nixpkgs-flake, ... }: {
          nix.registry.nixpkgs.flake = nixpkgs-flake;
          nix.registry.squirclespace.flake = self;
        };

        common = import ./nixosModules/common.nix;
        ssh = import ./nixosModules/ssh.nix;
        backup = import ./nixosModules/backup.nix;
        flakeAutoupdate = import ./nixosModules/flake-autoupdate.nix;
        gnome = import ./nixosModules/gnome.nix;
        kde = import ./nixosModules/kde.nix;
        photosync = import ./nixosModules/photosync.nix;
        ada = import ./nixosModules/ada.nix {inherit agenix self;};
        fprintdLidSwitch = import ./nixosModules/fprintdLidSwitch.nix;

        default = { ... }: {
          imports = [
            configurationRevision
            nixpkgsPinModule
            common
            flakeAutoupdate
            backup
            ssh
            ada
            fprintdLidSwitch
          ];
        };
      };

      nixosConfigurations = {
        plasma = callNixosSystem ./nixosConfigurations/plasma;
        Jobe = callNixosSystem ./nixosConfigurations/jobe;
        Libbie = callNixosSystem ./nixosConfigurations/libbie;
        pifer = callNixosSystem ./nixosConfigurations/pifer;
      };

      packages = genSystems allSystems (system: nixpkgs: {
        emacs = nixpkgs.legacyPackages."${system}".callPackage ./packages/emacs.nix {
          emacs = nixpkgs.legacyPackages."${system}".emacs29;
        };
        emacs-nox = nixpkgs.legacyPackages."${system}".callPackage ./packages/emacs.nix {
          emacs = nixpkgs.legacyPackages."${system}".emacs29-nox;
        };

        git-remote-doas = nixpkgs.legacyPackages."${system}".callPackage ./packages/git-remote-doas {};

        etc-nixos-post-receive-hook = nixpkgs.legacyPackages."${system}".callPackage ./packages/etc-nixos-post-receive-hook.nix {};
      });

      lib = {
        publicKeys = import ./lib/publicKeys.nix;
        backupServers.rsync = import ./lib/server-rsync.net.nix;
      };
    };
}
