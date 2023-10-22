{ config, pkgs, ... }:
{
  imports = [
    ./ui.nix
  ];

  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.sddm.enable = true;
}
