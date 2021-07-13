{ config, pkgs, ... }:
{
  imports = [
    ./overlays.nix
  ];

  # Manage that network
  networking.networkmanager.enable = true;

  # Emacs is bestest
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  # Firejail the things
  programs.firejail.enable = true;

  # Keep tmp temporary!
  boot.tmpOnTmpfs = true;
  boot.cleanTmpDir = true;

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  # No sudo!
  security.sudo.enable = false;
  security.doas.enable = true;
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
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
