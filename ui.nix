{ config, pkgs, ... }:
{
  services.xserver.enable = true;
  # services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.displayManager.gdm.wayland = false;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.windowManager.stumpwm.enable = true;

  services.xserver.desktopManager.gnome3.sessionPath = [
    pkgs.gnomeExtensions.appindicator
  ];

  users.extraUsers.ada.packages = with pkgs; [
    firefox
    gparted
    kdeApplications.kontact
    kdeApplications.kaddressbook
    kdeApplications.kmail
    kdeApplications.kmail-account-wizard
    kdeApplications.korganizer
    kdeApplications.okular
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
    (signal-desktop.overrideAttrs (old: {
      preFixup = old.preFixup + ''
        # Make signal desktop shortcut launch it into the tray
        substituteInPlace $out/share/applications/signal-desktop.desktop \
          --replace $out/bin/signal-desktop "$out/bin/signal-desktop --use-tray-icon --start-in-tray"
      '';
    }))
  ];
}
