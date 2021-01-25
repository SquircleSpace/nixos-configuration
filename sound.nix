{ config, pkgs, lib, ... }:
{
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
  programs.dconf.enable = lib.mkDefault true;
}
