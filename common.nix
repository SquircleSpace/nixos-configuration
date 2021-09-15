{ config, pkgs, lib, ... }:
{
  imports = [
    ./overlays.nix
    ./flake-autoupdate.nix
    ./backup.nix
  ];
  config = {
    # Manage that network
    networking.networkmanager.enable = lib.mkDefault true;

    # Emacs is bestest
    services.emacs.enable = lib.mkDefault true;
    services.emacs.defaultEditor = lib.mkDefault true;

    # Firejail the things
    programs.firejail.enable = lib.mkDefault true;

    # Keep tmp temporary!
    boot.tmpOnTmpfs = lib.mkDefault true;
    boot.cleanTmpDir = lib.mkDefault true;

    programs.gnupg.agent.enable = lib.mkDefault true;
    programs.gnupg.agent.enableSSHSupport = lib.mkDefault true;

    # No sudo!
    security.sudo.enable = lib.mkDefault false;
    security.doas.enable = lib.mkDefault true;
    security.doas.extraRules = [
      {
        groups = [ "wheel" ];
        persist = true;
        keepEnv = true;
      }
    ];

    # Nix flakes requires git to be in the PATH.  Its silly.
    environment.systemPackages = [ pkgs.gitFull ];

    # Enable flakes!
    nix = {
      package = lib.mkDefault pkgs.nixUnstable;
      extraOptions = ''
      experimental-features = nix-command flakes
    '';
    };
  };
}
