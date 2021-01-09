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

  # Manage that network
  networking.networkmanager.enable = true;

  # Emacs is bestest
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  # Firejail the things
  programs.firejail.enable = true;

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Keep tmp temporary!
  boot.tmpOnTmpfs = true;
  boot.cleanTmpDir = true;

  # Make boot pretty!
  boot.plymouth.enable = true;

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
      emacs
      file
      gdb
      git
      graphviz
      htop
      ispell
      lldb
      lsof
      mosh
      psmisc
      glances
      smartmontools
      stow
      tmux
      unzip
      w3m
      wget
      xxd
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
