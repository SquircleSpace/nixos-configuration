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

  system.activationScripts = {
    "Protect /etc/nixos/user-passwords/" = ''
      if [ -d /etc/nixos/user-passwords/ ]; then
        chown -R root:root /etc/nixos/user-passwords/
        chmod -R go-rwx /etc/nixos/user-passwords/
      fi
    '';
  };
}
