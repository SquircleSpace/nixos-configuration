{ config, pkgs, ... }:
{
  services.xserver.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = false;
  services.xserver.windowManager.stumpwm.enable = true;

  services.xserver.desktopManager.gnome3.sessionPath = [
    pkgs.gnomeExtensions.appindicator
  ];

  users.extraUsers.ada.packages = with pkgs; [
    firefox
    gnome3.gnome-tweaks
    gparted
    pavucontrol
    terminator
    vlc
    xscreensaver
    (pidgin-with-plugins.override {
      plugins = [
        purple-hangouts
        pidginwindowmerge
        pidgin-opensteamworks
        (callPackage ./pidgin-indicator.nix { })
      ];
    })
    signal-desktop
  ];
}
