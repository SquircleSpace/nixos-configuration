{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./machine.nix
    ./overlays.nix
  ];

  environment.systemPackages = with pkgs; [
    # Keep the global environment clean
  ];

  # You want a UI?  I got a UI!
  services.xserver.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = false;
  services.xserver.windowManager.stumpwm.enable = true;
  #services.xserver.windowManager.default = "stumpwm";
  #services.xserver.desktopManager.default = "none";

  # Manage that network
  networking.networkmanager.enable = true;

  # Emacs is bestest
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Everyone loves logging in
  users.extraUsers.ada = {
    isNormalUser = true;
    home = "/home/ada";
    description = "Ada";
    extraGroups = [ "wheel" "networkmanager" ];
    uid = 1000;
    openssh.authorizedKeys.keyFiles = [ ./ada-pubkey ];
    passwordFile = "/etc/nixos/user-passwords/ada";
    packages = with pkgs; [
      aspellDicts.en
      btrfs-progs
      chromium
      darktable
      discord
      emacs
      file
      firefox
      gdb
      git
      gnomeExtensions.nohotcorner
      gparted
      graphviz
      htop
      lldb
      lsof
      mosh
      nox
      pavucontrol
      psmisc
      sbcl
      smartmontools
      stow
      terminator
      tmux
      unzip
      vlc
      w3m
      wget
      xscreensaver
      xxd
      (pidgin-with-plugins.override {
        plugins = [
          purple-hangouts pidginwindowmerge
          pidgin-opensteamworks
        ];
      })
    ];
  };
  users.mutableUsers = false;

  system.activationScripts = {
    "Protect /etc/nixos/user-passwords/" = ''
      chown -R root:root /etc/nixos/user-passwords/
      chmod -R go-rwx /etc/nixos/user-passwords/
    '';
  };

  hardware.pulseaudio.enable = true;
}
