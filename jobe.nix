{ config, pkgs, lib, ... }:
let
  boot-windows = import ./boot-windows pkgs;
in
{
  imports = [
    ./hardware-configuration.nix
    ./common.nix
    ./vpn.nix
    ./ui.nix
    ./ada.nix
  ];

  # Hello!  My name is
  networking.hostName = "Jobe";

  # Updates are important
  system.autoUpgrade.enable = true;
  # Updates fill up my disk
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 10d";

  # We're self respecting and use EFI boot around here
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "America/Los_Angeles";

  # SSH.  The lifeblood of a server.
  services.openssh.enable = true;
  services.openssh.forwardX11 = true;
  # And Mosh!
  programs.mosh.enable = true;

  networking.firewall.allowedTCPPorts = [ 7777 ];
  networking.firewall.allowedUDPPorts = [ 7777 ];

  # Drivers
  services.xserver.videoDrivers = ["nvidia"];
  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages = [ pkgs.libva ];
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio = {
    support32Bit = true;
    package = pkgs.pulseaudioFull; # for bluetooth output
    extraModules = [ pkgs.pulseaudio-modules-bt ]; # support more codecs
  };
  hardware.bluetooth.enable = true;
  hardware.bluetooth.config = {
    General = {
      ControllerMode = "dual";
    };
  };
  services.blueman.enable = true;

  system.stateVersion = "20.09";

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

  services.btrfs.autoScrub = {
    enable = true;
    fileSystems = [ "/crypt" ];
  };

  services.smartd = {
    enable = true;
    notifications.x11 = {
      enable = true;
      display = ":1"; # For some reason, I seem to always log in on display 1?
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
    darktable
    discord
    lutris
    steam
    steam.run
    wine
    (import ./minecraft.nix { inherit pkgs; })
  ];

  hardware.steam-hardware.enable = true;

  # ESYNC support in some Proton games requires a LOT of file descriptors
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";

  swapDevices = [
    {
      device = "/dev/disk/by-uuid/d82294ec-e7fb-4225-b6d0-9296dba8a23b";
    }
  ];

  services.borgbackup.repos.pifer = {
    allowSubRepos = true;
    authorizedKeysAppendOnly = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINKi5JMv0RCL4XLdTkACef/dgQdkVgVOoUdTcoCGD3ww ada@squircle.space"
    ];
    path = "/crypt/borg/pifer";
  };

  services.borgbackup.jobs."rsync" = let
    rawBorgPasswordPath = "/var/lib/borg/password";
    borgPasswordPath = assert (lib.assertMsg (builtins.pathExists rawBorgPasswordPath) "Make sure to put the encryption password in place!"); rawBorgPasswordPath;
    rawBorgPrivateKeyPath = "/var/lib/borg/id_ed25519";
    borgPrivateKeyPath = assert (lib.assertMsg (builtins.pathExists rawBorgPrivateKeyPath) "Make sure to put the private key in place!"); rawBorgPrivateKeyPath;
    subvolumeNames = [
      "home"
      "photos"
    ];
    snapshotDirectory = subvolumeName: "/crypt/snapshots/${subvolumeName}";
    subvolumeDirectory = subvolumeName: "/crypt/${subvolumeName}";
    backupDirectory = subvolumeName: "${snapshotDirectory subvolumeName}/current";
  in {
    repo = "7995@usw-s007.rsync.net:borg/jobe/main";
    startAt = "weekly";
    environment = {
      "BORG_REMOTE_PATH" = "borg1";
      "BORG_RSH" = "ssh -i ${borgPrivateKeyPath}";
    };
    encryption.mode = "repokey";
    encryption.passCommand = "cat ${borgPasswordPath}";
    compression = "auto,lzma";
    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 14;
      weekly = 26;
      monthly = 6;
      yearly = 5;
    };
    preHook = ''
      backupTime="$(date --rfc-3339=seconds)"
      '' + lib.strings.concatMapStrings (subvolumeName: ''
      if [ -d "${backupDirectory subvolumeName}" ]; then
        ${pkgs.btrfs-progs}/bin/btrfs subvolume delete "${backupDirectory subvolumeName}"
      fi
      ${pkgs.btrfs-progs}/bin/btrfs subvolume snapshot -r "${subvolumeDirectory subvolumeName}" "${snapshotDirectory subvolumeName}/$backupTime"
      ${pkgs.btrfs-progs}/bin/btrfs subvolume snapshot -r "${snapshotDirectory subvolumeName}/$backupTime" "${backupDirectory subvolumeName}"
    '') subvolumeNames;
    postHook = lib.strings.concatMapStrings (subvolumeName: ''
      if [ -d "${backupDirectory subvolumeName}" ]; then
        ${pkgs.btrfs-progs}/bin/btrfs subvolume delete "${backupDirectory subvolumeName}"
      fi
    '') subvolumeNames;
    paths = map backupDirectory subvolumeNames;
    readWritePaths = map snapshotDirectory subvolumeNames;
    exclude = [
      "${backupDirectory "home"}/ada/Downloads"
      "${backupDirectory "home"}/ada/.local/share/Steam"
    ];
  };

  system.activationScripts = {
    "Protect /var/lib/borg/" = ''
      chown -R root:root /var/lib/borg/
      chmod -R go-rwx /var/lib/borg/
    '';
  };
}
