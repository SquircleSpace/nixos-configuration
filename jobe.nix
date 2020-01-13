{ config, pkgs, ... }:
let
  boot-windows = import ./boot-windows { stdenv = pkgs.stdenv; };
in
{
  imports = [
    ./vpn.nix
  ];

  # Hello!  My name is
  networking.hostName = "Jobe";

  # Updates are important
  system.autoUpgrade.enable = true;

  # We're self respecting and use EFI boot around here
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "America/Los_Angeles";

  # SSH.  The lifeblood of a server.
  services.openssh.enable = true;
  services.openssh.forwardX11 = true;
  # And Mosh!
  programs.mosh.enable = true;

  # Share some files
  services.samba.enable = true;
  services.samba.shares = {
    media = {
      browseable = "yes";
      comment = "Media";
      "guest ok" = "no";
      path = "/media/";
      "read only" = "yes";
    };
    homes = {
      "guest ok" = "no";
      "browseable" = "no";
      "read only" = "yes";
    };
  };
  services.samba.syncPasswordsByPam = true;

  # Open up samba ports
  networking.firewall.allowedTCPPorts = [ 137 138 139 445 ];
  networking.firewall.allowedUDPPorts = [ 137 138 139 445 ];

  # Drivers
  services.xserver.videoDrivers = ["nvidia"];
  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages = [ pkgs.libva ];
  hardware.pulseaudio.support32Bit = true;

  system.stateVersion = "18.03";

  # Mount some partitions
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/c17e85fc-f9f4-45ef-b468-c6a97d28be7d";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/C420-4469";
      fsType = "vfat";
    };
    "/malachi" = {
      device = "/dev/disk/by-uuid/AA584CB4584C80D5";
      fsType = "ntfs-3g";
      options = [ "ro" "dmask=0222" "fmask=0333" ];
    };
    "/home" = {
      device = "/dev/disk/by-uuid/82b4a0b5-3266-4d53-840d-061d43b3db57";
      fsType = "btrfs";
      options = [ "subvol=/home" ];
    };
    "/crypt" = {
      device = "/dev/disk/by-uuid/82b4a0b5-3266-4d53-840d-061d43b3db57";
      fsType = "btrfs";
    };
  };

  # For working on nixpkgs
  nix.useSandbox = true;

  # Let me tell you where my screens go
  services.xserver.dpi = 150;
  services.xserver.xrandrHeads = [
    {
      "output" = "DVI-D-0";
      "primary" = false;
      "monitorConfig" = ''
        DisplaySize 476 298
        Option "LeftOf" "DP-0"
      '';
    }
    {
      "output" = "DP-0";
      "primary" = true;
      "monitorConfig" = ''DisplaySize 622 343'';
    }
    {
      "output" = "HDMI-0";
      "primary" = false;
      "monitorConfig" = ''Option "Enable" "false"'';
    }
  ];

  # Booting windows should be easy
  security.wrappers = {
    boot-windows = {
      source = "${boot-windows}/bin/boot-windows";
      owner = "root";
      setuid = true;
    };
  };

  users.extraUsers.brad.packages = with pkgs; [
    steam playonlinux wine
  ];

  # ESYNC support in some Proton games requires a LOT of file descriptors
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";
}
