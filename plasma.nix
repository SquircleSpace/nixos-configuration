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

  specialisation."nonui".configuration = {
    services.xserver.enable = lib.mkForce false;
    services.xserver.desktopManager.plasma5.enable = lib.mkForce false;
    services.xserver.displayManager.sddm.enable = lib.mkForce false;
    boot.plymouth.enable = lib.mkForce false;
    services.flatpak.enable = lib.mkForce false;
  };
}
