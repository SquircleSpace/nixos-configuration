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
    ./ssh.nix
    ./backup.nix
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

  # Mosh is cool!
  programs.mosh.enable = true;

  # Drivers
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  powerManagement.cpuFreqGovernor = "ondemand";

  services.wakeonlan.interfaces = [
    {
      interface = "eno1";
      method = "magicpacket";
    }
  ];

  system.stateVersion = "21.05";

  boot.initrd.luks.reusePassphrases = true;

  boot.initrd.luks.devices.crypt1 = {
    allowDiscards = true;
    device = "/dev/disk/by-partuuid/42024e68-9074-4943-b210-ae526e2d34b4";
    preLVM = true;
  };
  boot.initrd.luks.devices.crypt2 = {
    allowDiscards = true;
    device = "/dev/disk/by-partuuid/de0ce953-6b46-4c5e-9429-ccf400edee1a";
    preLVM = true;
  };
  boot.initrd.luks.forceLuksSupportInInitrd = true;

  # Mount some partitions
  fileSystems = {
    "/" = {
      device = "/dev/mapper/crypt1";
      fsType = "btrfs";
      options = [ "subvol=/root" "discard" "noatime" ];
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/C420-4469";
      fsType = "vfat";
    };
    "/home" = {
      device = "/dev/mapper/crypt1";
      fsType = "btrfs";
      options = [ "subvol=/home" "discard" "noatime" ];
    };
    "/crypt" = {
      device = "/dev/mapper/crypt1";
      fsType = "btrfs";
      options = [ "discard" "noatime" ];
    };
    "/nix" = {
      device = "/dev/mapper/crypt1";
      fsType = "btrfs";
      options = [ "subvol=/nix" "discard" "noatime" ];
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
    nvtop
    steam
    steam.run
    wine
    (import ./minecraft.nix { inherit pkgs; })
  ];

  hardware.steam-hardware.enable = true;
  # Open ports for steam remote play
  networking.firewall.allowedTCPPorts = [ 27036 ];
  networking.firewall.allowedUDPPorts = [ 27031 27032 27033 27034 27035 27036 ];

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
    user = "borg";
  };
  users.extraUsers.borg.isSystemUser = true;

  services.borgbackup.smartjobs."rsync" = {
    paths = [ "/home" "/crypt/photos" ];
    subvolumes = [ "/home" "/crypt/photos" ];
    exclude = [
      "/home/ada/Downloads"
      "/home/ada/.local/share/Steam"
    ];
    server = import ./server-rsync.net.nix;
    repoName = "borg/jobe/main";
    privateKeyPath = "/var/lib/borg/id_ed25519";
    passwordPath = "/var/lib/borg/password";
    snapshotPath = "/crypt/snapshots/backup";
  };

  users.extraUsers.command = {
    createHome = false;
    home = "/var/empty";
    isSystemUser = true;
    openssh.authorizedKeys.keys = [
      ''command="/run/wrappers/bin/doas ${pkgs.systemd}/bin/systemctl suspend" ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAbpjRKHraUCNHijRzHdPQI6XmgfT0Gj/TlXQ6pcYbOY root@pifer''
    ];
    useDefaultShell = true;
  };
  security.doas.extraRules = [
    {
      users = ["command"];
      cmd = "${pkgs.systemd}/bin/systemctl";
      args = ["suspend"];
      runAs = "root";
      noPass = true;
    }
  ];
}
