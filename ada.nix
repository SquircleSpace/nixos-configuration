{ config, pkgs, lib, ...}:
let
assertPath = import ./assertPath.nix lib;
nonUIPackages = with pkgs; [
    aspellDicts.en
    btrfs-progs
    file
    gdb
    git
    glances
    graphviz
    htop
    ispell
    lldb
    lsof
    mosh
    pandoc
    psmisc
    sbcl
    smartmontools
    stow
    tmux
    unzip
    w3m
    wget
    xxd
  ];
uiPackages = with pkgs; [
    bitwarden
    chromium
    firefox
    gimp
    gparted
    pavucontrol
    terminator
    vlc
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
kdePackages = with pkgs; [
    kdeApplications.kontact
    kdeApplications.kaddressbook
    kdeApplications.kmail
    kdeApplications.kmail-account-wizard
    kdeApplications.korganizer
    kdeApplications.okular
  ];
isUI = config.services.xserver.enable;
emacsPackage = with pkgs; if isUI then emacs else emacs-nox;
optionalList = condition: list: if condition then list else [];
packages = lib.flatten [
    nonUIPackages
    (optionalList isUI uiPackages)
    (optionalList config.services.xserver.desktopManager.plasma5.enable kdePackages)
    [ emacsPackage ]
  ];
in
{
  users.extraUsers.ada = {
    isNormalUser = true;
    home = "/home/ada";
    description = "Ada";
    extraGroups = [ "wheel" "networkmanager" ];
    passwordFile = assertPath "/etc/nixos/user-passwords/ada" "Must give ada a password";
    packages = packages;
  };
}
