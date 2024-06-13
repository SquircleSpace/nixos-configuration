{ self, config, pkgs, lib, ... }:
{
  squircle.space.ada.enable = true;
  squircle.space.fprintd.disableOnLidClose = true;
  services.tailscale.enable = true;
  services.tailscale.openFirewall = true;
  services.tailscale.useRoutingFeatures = "server";

  boot.loader.systemd-boot.enable = false;
  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/etc/secureboot";
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "amdgpu.sg_display=0" # https://community.frame.work/t/tracking-graphical-corruption-in-fedora-39-amd-3-03-bios/39073/27
  ] ++ lib.optionals config.boot.plymouth.enable [
    "quiet"
    "udev.log_level=3"
  ];
  boot.bootspec.enable = true;

  # To allow cross compilation
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # Use the fancy new stage 1
  boot.initrd.systemd.enable = true;

  services.fwupd.enable = true;

  services.btrfs.autoScrub = {
    enable = true;
    fileSystems = [ "/" ];
  };

  networking.hostName = "plasma";

  time.timeZone = "America/Los_Angeles";

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services.libinput.enable = true;

  users.mutableUsers = false;

  services.logind.lidSwitch = "suspend";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "suspend";

  system.stateVersion = "23.05";

  system.autoUpgrade.enable = true;
  system.autoUpgrade.flake = "/etc/nixos";
  system.autoUpgrade.flags = [ "--commit-lock-file" "--recreate-lock-file" ];
  environment.etc."nixos/.git/hooks/post-receive" = {
    enable = true;
    source = self.packages."${pkgs.system}".etc-nixos-post-receive-hook;
  };

  documentation.dev.enable = true;
  documentation.man.generateCaches = true;

  services.flatpak.enable = true;
  services.packagekit.enable = true;
  # https://github.com/NixOS/nixpkgs/issues/49793
  services.packagekit.settings.Daemon.DefaultBackend = "test_nop";

  services.kmonad = {
    enable = true;
    keyboards."builtin" = {
      device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      defcfg.fallthrough = true;
      defcfg.enable = true;
      config = ''
        (defsrc
          esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12
          grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
          tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
          caps a    s    d    f    g    h    j    k    l    ;    '    ret
          lsft z    x    c    v    b    n    m    ,    .    /    rsft up
          lctl      lmet lalt            spc            ralt rctl left down rght
        )

        (deflayer default
          _    _    _    _    _    _    _    _    _    _    _    _    _
          _    _    _    _    _    _    _    _    _    _    _    _    _    _
          _    _    _    _    _    _    _    _    _    _    _    _    _    _
          lctl _    _    _    _    _    _    _    _    _    _    _    _
          _    _    _    _    _    _    _    _    _    _    _    _    _
          _         lalt lmet           _              _    _    _    _    _
        )
      '';
    };
  };

  services.borgbackup.smartjobs."rsync" = {
    paths = [ "/home/ada" ];
    subvolumes = [ "/" "/home" ];
    exclude = [
      "/home/ada/.cache"
      "/home/ada/Downloads"
      "/home/ada/.local/share/flatpak"
    ];
    server = self.lib.backupServers.rsync;
    repoName = "borg/plasma/main";
    privateKeyPath = "/var/lib/borg/id_ed25519";
    passwordPath = "/var/lib/borg/password";
    snapshotPath = "/btrfs/backups";
  };

  specialisation."nonui".configuration = {
    services.xserver.enable = lib.mkForce false;
    services.desktopManager.plasma6.enable = lib.mkForce false;
    services.displayManager.sddm.enable = lib.mkForce false;
    boot.plymouth.enable = lib.mkForce false;
    services.flatpak.enable = lib.mkForce false;
  };
}
