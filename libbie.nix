{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./common.nix
    ./ui.nix
    ./ada.nix
    ./ssh.nix
    ./backup.nix
  ];

  hardware.system76.enableAll = true;

  boot.loader.timeout = 1;
  boot.loader.systemd-boot.editor = false;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelPatches = [
    {
      name = "system76-acpi-update";
      patch = ./system76-acpi.patch;
      extraConfig = "";
    }
  ];

  boot.initrd.luks.devices.crypted = {
    allowDiscards = true;
    device = "/dev/disk/by-uuid/66df9155-0a7a-420c-b2df-1cbe9120c232";
    preLVM = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/2ff8a826-b407-4ae5-b371-2e040216a9e0";
    fsType = "btrfs";
    options = [ "subvol=/subvolumes/root" "discard" ];
  };
  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/2ff8a826-b407-4ae5-b371-2e040216a9e0";
    fsType = "btrfs";
    options = [ "subvol=/subvolumes/home" "discard" ];
  };
  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/2ff8a826-b407-4ae5-b371-2e040216a9e0";
    fsType = "btrfs";
    options = [ "subvol=/subvolumes/nix" "discard" ];
  };
  fileSystems."/btrfs" = {
    device = "/dev/disk/by-uuid/2ff8a826-b407-4ae5-b371-2e040216a9e0";
    fsType = "btrfs";
    options = [ "discard" ];
  };
  services.btrfs.autoScrub = {
    enable = true;
    fileSystems = [ "/" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/7C33-E4CC";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/fb60397a-f288-408e-b6ad-5ecb95fb7534"; }
  ];

  networking.hostName = "Libbie";

  time.timeZone = "America/Los_Angeles";

  environment.etc."systemd/system-sleep/unlock-after-hibernate".source = pkgs.writeScript "post-hibernate.sh" ''
    #! ${pkgs.bash}/bin/bash

    if [ "$1" == "post" ] && [ "$2" == "hibernate" ]; then
      ${pkgs.systemd}/bin/loginctl --no-ask-password unlock-sessions
    fi
  '';

  services.xserver.displayManager.autoLogin = {
    enable = true;
    user = "ada";
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services.xserver.libinput.enable = true;

  users.mutableUsers = false;

  powerManagement.powerUpCommands = ''
    echo 70 > /sys/class/power_supply/BAT0/charge_control_start_threshold
    echo 75 > /sys/class/power_supply/BAT0/charge_control_end_threshold
  '';

  powerManagement.powertop.enable = true;
  services.tlp.enable = true;
  networking.networkmanager.wifi.powersave = true;

  system.stateVersion = "21.05";

  services.borgbackup.smartjobs."rsync" = {
    paths = [ "/home" ];
    subvolumes = [ "/home" ];
    exclude = [
      "/home/ada/.cache"
      "/home/ada/Downloads"
      "/home/ada/.local/share/Steam"
    ];
    server = import ./server-rsync.net.nix;
    repoName = "borg/libbie/main";
    privateKeyPath = "/var/lib/borg/id_ed25519";
    passwordPath = "/var/lib/borg/password";
    snapshotPath = "/btrfs/snapshots/backup";
  };

  nixpkgs.config.allowUnfree = true;

  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  users.extraUsers.ada.packages = with pkgs; [
    discord
    steam
    steam.run
    wine
  ];
}
