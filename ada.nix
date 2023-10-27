{ config, pkgs, lib, ...}:
let
  gitRemoteDoas = pkgs.callPackage ./git-remote-doas {};
  nonUIPackages = with pkgs; [
    btrfs-progs
    file
    gdb
    git
    gitRemoteDoas
    glances
    htop
    lldb
    lsof
    mosh
    psmisc
    roswell
    smartmontools
    tmux
    unzip
    w3m
    wget
    xxd
  ];
  uiPackages = with pkgs; [
    aspellDicts.en
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
    stow
    terminator
    vlc
  ];
  kdePackages = with (if "20.09" == config.system.nixos.release then pkgs.kdeApplications else pkgs.plasma5Packages); [
    discover
    kaddressbook
    kleopatra
    kmail
    kmail-account-wizard
    kontact
    korganizer
    okular
    yakuake
  ];
  gnomePackages = with pkgs.gnome; [
    gnome-tweaks
    gnome-shell-extensions
  ];
  isUI = config.services.xserver.enable;
  baseEmacsPackage = with pkgs; if isUI then emacs29 else emacs29-nox;
  emacsPackage = (import ./emacs.nix) { inherit pkgs; emacs = baseEmacsPackage; };
  optionalList = condition: list: if condition then list else [];
  packages = lib.flatten [
    nonUIPackages
    (optionalList isUI uiPackages)
    (optionalList config.services.xserver.desktopManager.plasma5.enable kdePackages)
    (optionalList config.services.xserver.desktopManager.gnome.enable gnomePackages)
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
        chown root:root ${passwordPath}
        chmod go-rwx ${passwordPath}
      fi
    '';
  };
}
