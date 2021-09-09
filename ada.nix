{ config, pkgs, lib, ...}:
let
  nonUIPackages = with pkgs; [
    btrfs-progs
    file
    gdb
    git
    glances
    htop
    lldb
    lsof
    mosh
    psmisc
    smartmontools
    tmux
    unzip
    w3m
    wget
    xxd
  ];
  uiPackages = with pkgs; [
    aspellDicts.en
    bitwarden
    caffeine-ng
    chromium
    firefox
    gimp
    gitAndTools.git-filter-repo
    gparted
    graphviz
    ispell
    nextcloud-client
    pandoc
    paprefs
    pavucontrol
    sbcl
    stow
    terminator
    vlc
    (signal-desktop.overrideAttrs (old: {
      preFixup = old.preFixup + ''
        # Make signal desktop shortcut launch it into the tray
        substituteInPlace $out/share/applications/signal-desktop.desktop \
          --replace $out/bin/signal-desktop "$out/bin/signal-desktop --use-tray-icon --start-in-tray"
      '';
    }))
  ];
  kdePackages = with (if "20.09" == config.system.nixos.release then pkgs.kdeApplications else pkgs.plasma5Packages); [
    kaddressbook
    kleopatra
    kmail
    kmail-account-wizard
    kontact
    korganizer
    okular
    yakuake
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
  groups = lib.flatten [
    [ "wheel" ]
    (optionalList config.networking.networkmanager.enable [ "networkmanager" ])
  ];
  passwordPath = "/etc/user-passwords/ada";
in
{
  users.extraUsers.ada = {
    isNormalUser = true;
    home = "/home/ada";
    description = "Ada";
    extraGroups = groups;
    passwordFile = passwordPath;
    packages = packages;
  };

  system.activationScripts = {
    "Protect ${passwordPath}" = ''
      if [ -e ${passwordPath} ]; then
        chown -R root:root ${passwordPath}
        chmod -R go-rwx ${passwordPath}
      fi
    '';
  };
}
