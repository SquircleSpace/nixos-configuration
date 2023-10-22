{ config, pkgs, ... }:
{
  imports = [
    ./libbie-hardware.nix
    ./common.nix
    ./gnome.nix
    ./ada.nix
    ./sasha.nix
    ./ssh.nix
    ./tailscale.nix
  ];

  hardware.system76.enableAll = true;

  boot.loader.timeout = 1;
  boot.loader.systemd-boot.editor = false;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.luks.devices.crypted = {
    allowDiscards = true;
    device = "/dev/disk/by-uuid/e6c8ba08-a85b-483f-bd1b-0b2452f7cfef";
    preLVM = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e901b8a1-c8da-49e9-a868-3043bea918e9";
    fsType = "btrfs";
    options = [ "subvol=/subvolumes/root" "discard" "noatime" ];
  };
  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/e901b8a1-c8da-49e9-a868-3043bea918e9";
    fsType = "btrfs";
    options = [ "subvol=/subvolumes/home" "discard" "noatime" ];
  };
  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/e901b8a1-c8da-49e9-a868-3043bea918e9";
    fsType = "btrfs";
    options = [ "subvol=/subvolumes/nix" "discard" "noatime" ];
  };
  fileSystems."/btrfs" = {
    device = "/dev/disk/by-uuid/e901b8a1-c8da-49e9-a868-3043bea918e9";
    fsType = "btrfs";
    options = [ "discard" "noatime" ];
  };
  services.btrfs.autoScrub = {
    enable = true;
    fileSystems = [ "/" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/B1BE-2BE0";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/c594e4ec-5d60-401b-b4ff-84a7c239cb92"; }
  ];

  networking.hostName = "Libbie";

  time.timeZone = "America/Los_Angeles";

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services.xserver.libinput.enable = true;

  users.mutableUsers = false;

  powerManagement.powerUpCommands = ''
    echo 70 > /sys/class/power_supply/BAT0/charge_control_start_threshold
    echo 75 > /sys/class/power_supply/BAT0/charge_control_end_threshold
  '';

  powerManagement.powertop.enable = true;
  networking.networkmanager.wifi.powersave = true;

  services.logind.lidSwitch = "suspend-then-hibernate";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "suspend";
  services.upower.enable = true;
  services.upower.criticalPowerAction = "HybridSleep";

  system.stateVersion = "21.05";

  services.borgbackup.smartjobs."rsync" = {
    paths = [ "/home/ada" ];
    subvolumes = [ "/home" ];
    exclude = [
      "/home/ada/.cache"
      "/home/ada/Downloads"
      "/home/ada/.local/share/Steam"
      "/home/ada/.local/share/flatpak"
      "/home/ada/.var/app/com.valvesoftware.Steam"
    ];
    server = import ./server-rsync.net.nix;
    repoName = "borg/libbie/main";
    privateKeyPath = "/var/lib/borg/id_ed25519";
    passwordPath = "/var/lib/borg/password";
    snapshotPath = "/btrfs/snapshots/backup";
  };

  nixpkgs.config.allowUnfree = true;

  hardware.opengl.driSupport32Bit = true;

  users.extraUsers.ada.packages = with pkgs; [
    steam.run
    wineWowPackages.stable
  ];

  services.flakeAutoUpdate."/etc/nixos".enable = true;
  environment.etc."nixos/.git/hooks/post-receive" = {
    enable = true;
    source = pkgs.etc-nixos-post-receive-hook;
  };

  documentation.dev.enable = true;
  documentation.man.generateCaches = true;

  programs.steam.enable = true;
  hardware.steam-hardware.enable = true;

  services.flatpak.enable = true;
}
