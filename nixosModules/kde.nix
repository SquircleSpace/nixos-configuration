{ config, pkgs, ... }:
{
  imports = [
    ./ui.nix
  ];

  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.enable = true;
}
