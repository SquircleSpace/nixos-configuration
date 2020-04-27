{ config, pkgs, ... }:
let
  boot-windows = import ./boot-windows pkgs;
in
{
  imports = [
    ./vpn.nix
    ./minecraftServer.nix
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
  hardware.pulseaudio = {
    support32Bit = true;
    package = pkgs.pulseaudioFull; # for bluetooth output
    extraModules = [ pkgs.pulseaudio-modules-bt ]; # support more codecs
  };
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  system.stateVersion = "20.03";

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
  services.xserver.monitorSection = ''
    VendorName     "Unknown"
    ModelName      "Samsung U28E590"
    HorizSync       30.0 - 135.0
    VertRefresh     56.0 - 75.0
    Option         "DPMS"
  '';
  services.xserver.screenSection = ''
    Monitor        "Monitor[0]"
    Option         "Stereo" "0"
    Option         "nvidiaXineramaInfoOrder" "DFP-4"
    Option         "metamodes" "DP-2: nvidia-auto-select +1680+0, DVI-D-0: nvidia-auto-select +0+0"
    Option         "SLI" "Off"
    Option         "MultiGPU" "Off"
    Option         "BaseMosaic" "off"
    SubSection     "Display"
        Depth       24
    EndSubSection
  '';

  # Booting windows should be easy
  security.wrappers = {
    boot-windows = {
      source = "${boot-windows}/bin/boot-windows";
      owner = "root";
      setuid = true;
    };
  };

  users.extraUsers.ada.packages = with pkgs; [
    steam
    (import ./minecraft.nix { inherit pkgs; })
    gimp
  ];

  hardware.steam-hardware.enable = true;

  # ESYNC support in some Proton games requires a LOT of file descriptors
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";

  swapDevices = [
    {
      device = "/dev/disk/by-uuid/d82294ec-e7fb-4225-b6d0-9296dba8a23b";
    }
  ];
}
