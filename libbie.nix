{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./common.nix
    ./ui.nix
    ./ada.nix
    ./ssh.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" "system76" "system76-io" "system76-acpi" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];

  boot.initrd.luks.devices.crypted = {
    allowDiscards = true;
    device = "/dev/disk/by-uuid/66df9155-0a7a-420c-b2df-1cbe9120c232";
    preLVM = true;
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/2ff8a826-b407-4ae5-b371-2e040216a9e0";
      fsType = "btrfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/7C33-E4CC";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/fb60397a-f288-408e-b6ad-5ecb95fb7534"; }
    ];

  networking.hostName = "Libbie"; # Define your hostname.
  networking.networkmanager.enable = true;

  time.timeZone = "America/Los_Angeles";

  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.autoLogin = {
    enable = true;
    user = "ada";
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio = {
    package = pkgs.pulseaudioFull; # for bluetooth output
    extraModules = [ pkgs.pulseaudio-modules-bt ]; # support more codecs
  };
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services.xserver.libinput.enable = true;

  users.mutableUsers = false;

  system.stateVersion = "20.09";
}
