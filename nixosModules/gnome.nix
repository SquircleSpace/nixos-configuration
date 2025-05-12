{ config, pkgs, ... }:
{
  imports = [
    ./ui.nix
  ];

  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.displayManager.gdm.enable = true;

  services.xserver.desktopManager.gnome.sessionPath = [
    pkgs.gnomeExtensions.appindicator
    pkgs.gnomeExtensions.hide-universal-access
    pkgs.gnomeExtensions.unite
  ];
}
