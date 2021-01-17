{ config, pkgs, ... }:
{
  services.xserver.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.sddm.enable = true;

  services.xserver.desktopManager.gnome3.sessionPath = [
    pkgs.gnomeExtensions.appindicator
  ];
}
