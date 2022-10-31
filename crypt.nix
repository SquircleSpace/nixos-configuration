{ config, lib, pkgs, ... }:
{
  imports = [
    ./crypt-hardware.nix
    ./common.nix
    ./ada.nix
    ./tailscale.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 4;
  boot.loader.raspberryPi.uboot.configurationLimit = 3;
  boot.kernelPackages = pkgs.linuxPackages_rpi4;

  fileSystems."/" = {
    device = "/dev/disk/by-label/core";
    fsType = "btrfs";
    options = [ "subvol=/root" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/core";
    fsType = "btrfs";
    options = [ "subvol=/nix" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 5d";
  nix.gc.dates = "Mon *-*-* 00:03:15";
  system.autoUpgrade.enable = true;
  system.autoUpgrade.flake = "/etc/nixos";
  system.autoUpgrade.allowReboot = false;
  system.autoUpgrade.dates = "Mon *-*-* 00:04:40";
  services.flakeAutoUpdate."/etc/nixos".enable = true;
  environment.etc."nixos/.git/hooks/post-receive" = {
    enable = true;
    source = pkgs.etc-nixos-post-receive-hook;
  };

  boot.consoleLogLevel = lib.mkDefault 7;

  networking.hostName = "crypt";

  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.eth0.tempAddress = "disabled";
  networking.interfaces.wlan0.useDHCP = true;
  networking.interfaces.wlan0.tempAddress = "disabled";

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "America/Los_Angeles";

  environment.systemPackages = with pkgs; [
    emacs-nox tmux rsync
  ];

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  services.openssh.permitRootLogin = "no";

  services.fail2ban.enable = true;
  services.fail2ban.bantime-increment.enable = true;
  services.fail2ban.bantime-increment.maxtime = "30d";
  services.fail2ban.jails = {
    sshd = ''
      mode = aggressive
      enabled = true
      port = ${lib.concatMapStringsSep "," (p: toString p) config.services.openssh.ports}
    '';
  };

  users.mutableUsers = false;

  users.users.ada = {
    uid = 1000;
    openssh.authorizedKeys.keyFiles = [ ./jobe.pub ./phone.pub ./libbie.pub ];
  };

  system.stateVersion = "21.05";

  networking.networkmanager.enable = false;

  networking.firewall.enable = true;
}
