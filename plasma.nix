{ config, pkgs, lib, ... }:
{
  imports = [
    ./plasma-hardware.nix
    ./common.nix
    ./ui.nix
    ./ada.nix
    ./ssh.nix
    ./tailscale.nix
  ];

  boot.loader.systemd-boot.enable = false;
  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/etc/secureboot";
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.bootspec.enable = true;

  services.fwupd.enable = true;

  services.fprintd.enable = true;

  services.btrfs.autoScrub = {
    enable = true;
    fileSystems = [ "/" ];
  };

  networking.hostName = "plasma";

  time.timeZone = "America/Los_Angeles";

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services.xserver.libinput.enable = true;

  users.mutableUsers = false;

  powerManagement.powertop.enable = true;
  networking.networkmanager.wifi.powersave = true;

  services.logind.lidSwitch = "suspend";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "suspend";

  system.stateVersion = "23.05";

  services.flakeAutoUpdate."/etc/nixos".enable = true;
  environment.etc."nixos/.git/hooks/post-receive" = {
    enable = true;
    source = pkgs.etc-nixos-post-receive-hook;
  };

  documentation.dev.enable = true;
  documentation.man.generateCaches = true;

  services.flatpak.enable = true;

  specialisation."nonui".configuration = {
    services.xserver.enable = lib.mkForce false;
    services.xserver.desktopManager.plasma5.enable = lib.mkForce false;
    services.xserver.displayManager.sddm.enable = lib.mkForce false;
    boot.plymouth.enable = lib.mkForce false;
    services.flatpak.enable = lib.mkForce false;
  };
}
