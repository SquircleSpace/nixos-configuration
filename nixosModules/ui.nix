{ config, pkgs, lib, ... }:
{
  imports = [
    ./sound.nix
  ];

  services.xserver.enable = true;

  # Hardware acceleration for video decoding
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.graphics.enable = true;
  hardware.graphics.extraPackages = with pkgs; [
    intel-media-driver # LIBVA_DRIVER_NAME=iHD
    vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
    vaapiVdpau
    libvdpau-va-gl
    libva
  ];
  hardware.graphics.extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel ];

  # Make boot pretty!
  boot.plymouth.enable = true;

  # For recovery
  specialisation."nonui".configuration = {
    services.xserver.enable = lib.mkForce false;
    services.desktopManager.plasma6.enable = lib.mkForce false;
    services.displayManager.sddm.enable = lib.mkForce false;
    services.xserver.desktopManager.gnome.enable = lib.mkForce false;
    services.xserver.displayManager.gdm.enable = lib.mkForce false;
    boot.plymouth.enable = lib.mkForce false;
    services.flatpak.enable = lib.mkForce false;
  };
}
