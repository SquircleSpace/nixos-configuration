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
        defaults.nixpkgs.pkgs = nixpkgs.legacyPackages."${system}";
      };
    in {
      checks = genAttrs nixosSystems (system: {
        passwordPriorityOrder = runTestWithNixpkgs system nixpkgs2305 ./checks/passwordPriorityOrder.nix;
        adaModule = runTestWithNixpkgs system nixpkgs2305 (import ./checks/ada.nix self);
      });

      nixosModules = rec {
        configurationRevision = { lib, ... }: {
          system.configurationRevision = lib.mkIf (self ? rev) self.rev;
        };
        nixpkgsPinModule = { nixpkgs-flake, ... }: {
          nix.registry.nixpkgs.flake = nixpkgs-flake;
        };

        common = import ./nixosModules/common.nix;
        ssh = import ./nixosModules/ssh.nix;
        backup = import ./nixosModules/backup.nix;
        flakeAutoupdate = import ./nixosModules/flake-autoupdate.nix;
        gnome = import ./nixosModules/gnome.nix;
        kde = import ./nixosModules/kde.nix;
        tailscale = import ./nixosModules/tailscale.nix;
        photosync = import ./nixosModules/photosync.nix;
        ada = import ./nixosModules/ada.nix {inherit agenix self;};

        default = { ... }: {
          imports = [
            configurationRevision
            nixpkgsPinModule
            common
            flakeAutoupdate
            backup
            ssh
            ada
          ];
        };
      };

      nixosConfigurations = {
        plasma = callNixosSystem ./nixosConfigurations/plasma;
        Jobe = callNixosSystem ./nixosConfigurations/jobe;
        Libbie = callNixosSystem ./nixosConfigurations/libbie;
        pifer = callNixosSystem ./nixosConfigurations/pifer;
      };

      packages = genAttrs allSystems (system: {
        emacs = nixpkgs2305.legacyPackages."${system}".callPackage ./packages/emacs.nix {
          emacs = nixpkgs2305.legacyPackages."${system}".emacs29;
        };
        emacs-nox = nixpkgs2305.legacyPackages."${system}".callPackage ./packages/emacs.nix {
          emacs = nixpkgs2305.legacyPackages."${system}".emacs29-nox;
        };

        git-remote-doas = nixpkgs2305.legacyPackages."${system}".callPackage ./packages/git-remote-doas {};

        etc-nixos-post-receive-hook = nixpkgs2305.legacyPackages."${system}".callPackage ./packages/etc-nixos-post-receive-hook.nix {};
      });

      lib = {
        publicKeys = import ./publicKeys.nix;
        backupServers.rsync = import ./server-rsync.net.nix;
      };
    };
}
