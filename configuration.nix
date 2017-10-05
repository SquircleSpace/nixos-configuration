# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./hkwindow.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "Jobe";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  environment.systemPackages = with pkgs; [
    # Keep the global environment clean
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    crashplan = (pkgs.crashplan.overrideAttrs (oldAttrs: {
      version = "4.8.3";
      rev = "0";
      src = (pkgs.fetchurl {
        url = "https://download.code42.com/installs/linux/install/CrashPlan/CrashPlan_4.8.3_Linux.tgz";
        sha256 = "c25d87ec1d442a396b668547e39b70d66dcfe02250cc57a25916ebb42a407113";
      });
    }));
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.forwardX11 = true;
  # And Mosh!
  programs.mosh.enable = true;

  # Samba!  Let's do Samba!
  services.samba.enable = true;
  services.samba.shares = {
    photos = {
      browseable = "yes";
      comment = "Photos";
      "guest ok" = "no";
      path = "/hermes/Photos";
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
  networking.firewall.allowedTCPPorts = [ 80 137 138 139 445 ];
  networking.firewall.allowedUDPPorts = [ 80 137 138 139 445 ];
  # networking.firewall.enable = false;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";

  # Drivers
  services.xserver.videoDrivers = ["AMDGPU"];

  # KDE is nice
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # Let's not forget backups!
  services.crashplan.enable = true;
  nixpkgs.config.allowUnfree = true;

  # Everyone loves logging in
  users.extraUsers.brad = {
    isNormalUser = true;
    home = "/home/brad";
    description = "Brad Jensen";
    extraGroups = [ "wheel" "networkmanager" ];
    uid = 1000;
    openssh.authorizedKeys.keyFiles = [ ./brad-pubkey ];
  };
  users.mutableUsers = false;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

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
    "/crypt" = {
      device = "/dev/disk/by-uuid/481a934a-253e-44d5-aa6f-5429be6b7cd1";
      fsType = "btrfs";
    };
    "/hermes" = {
      device = "/dev/disk/by-uuid/D4CEEC09CEEBE22A";
      fsType = "ntfs-3g";
      options = [ "rw" "gid=${toString config.users.groups."wheel".gid}" "dmask=0002" "fmask=0113" ];
    };
    "/gaia" = {
      device = "/dev/disk/by-uuid/001C54791C546C22";
      fsType = "ntfs-3g";
      options = [ "ro" "dmask=0222" "fmask=0333" ];
    };
    "/malachi" = {
      device = "/dev/disk/by-uuid/AA584CB4584C80D5";
      fsType = "ntfs-3g";
      options = [ "ro" "dmask=0222" "fmask=0333" ];
    };
  };

  boot.initrd.luks.devices = {
    "crypt1" = {
      device = "/dev/disk/by-uuid/8a6a9794-5f18-4a4a-a7bb-550c7d8f62fe";
      keyFile = "/crypto_keyfile.bin";
    };
    "crypt2" = {
      device = "/dev/disk/by-uuid/eb811fec-f862-4b3d-b7c6-f5f0ba99f7fb";
      keyFile = "/crypto_keyfile.bin";
    };
  };

  nix.useSandbox = true;

  services.httpd.enable = false;
  services.httpd.documentRoot = /home/brad/lsite/build-tup;
  services.httpd.adminAddr = "brad@bradjensen.net";
  services.httpd.extraModules = [{ name = "fastcgi"; path = "${pkgs.mod_fastcgi}/modules/mod_fastcgi.so"; }];
  services.httpd.extraConfig = ''
    FastCgiIpcDir ${config.services.httpd.stateDir}/fcgi
    <Directory "${config.services.httpd.documentRoot}">
    AddHandler fastcgi-script fcgi
    Options +FollowSymLinks +ExecCGI

    RewriteEngine On
    RewriteBase /
    RewriteRule ^main\.fcgi/ - [L]
    RewriteRule ^static/ - [L]
    RewriteRule ^(.*)$ main.fcgi/$1 [L]
    </Directory>
  '';
}
