{ config, lib, pkgs, ... }:
let
  postSensor = pkgs.writeScript "postSensor" ''
    
  '';
  upToDate = pkgs.writeScript "upToDate" ''
    #! ${pkgs.bash}/bin/bash
    ${pkgs.systemd}/bin/systemctl is-active nixos-upgrade.service || ! ${pkgs.systemd}/bin/systemctl is-failed nixos-upgrade.service
    ${postSensor} systemUpToDate $?
  '';
in
{
  imports = [
    ./pifer-hardware.nix
    ./common.nix
    ./ada.nix
    ./homebridge-module.nix
    ./rss4email.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 4;
  boot.loader.raspberryPi.uboot.configurationLimit = 3;
  boot.kernelPackages = pkgs.linuxPackages_rpi4;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9d373950-72e0-44ab-9253-1bf37bc30edf";
    fsType = "btrfs";
    options = [ "subvol=/root" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/9d373950-72e0-44ab-9253-1bf37bc30edf";
    fsType = "btrfs";
    options = [ "subvol=/nix" ];
  };

  fileSystems."/btrfs" = {
    device = "/dev/disk/by-uuid/9d373950-72e0-44ab-9253-1bf37bc30edf";
    fsType = "btrfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/4A80-CA73";
    fsType = "vfat";
  };

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 21d";
  nix.gc.dates = "Mon *-*-* 00:03:15";
  system.autoUpgrade.enable = true;
  system.autoUpgrade.flake = "/etc/nixos";
  system.autoUpgrade.allowReboot = true;
  system.autoUpgrade.dates = "Mon *-*-* 00:04:40";
  services.flakeAutoUpdate."/etc/nixos".enable = true;
  environment.etc."nixos/.git/hooks/post-receive" = {
    enable = true;
    source = pkgs.etc-nixos-post-receive-hook;
  };

  boot.consoleLogLevel = lib.mkDefault 7;

  networking.hostName = "pifer";

  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.eth0.tempAddress = "disabled";
  networking.interfaces.wlan0.useDHCP = true;
  networking.interfaces.wlan0.tempAddress = "disabled";

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "America/Los_Angeles";

  environment.systemPackages = with pkgs; [
    emacs-nox tmux rsync
  ];

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  services.openssh.permitRootLogin = "no";

  services.fail2ban.enable = true;
  services.fail2ban.bantime-increment.enable = true;
  services.fail2ban.bantime-increment.maxtime = "30d";
  services.fail2ban.jails = {
    sshd = ''
      mode = aggressive
      enabled = true
      port = ${lib.concatMapStringsSep "," (p: toString p) config.services.openssh.ports}
    '';
  };

  users.mutableUsers = false;

  users.users.ada = {
    uid = 1000;
    openssh.authorizedKeys.keyFiles = [ ./jobe.pub ./phone.pub ./libbie.pub ];
  };

  system.stateVersion = "20.09";

  networking.networkmanager.enable = false;

  system.activationScripts."Protect /etc/user-passwords/nextcloud-root" = ''
    chmod go-rwx /etc/user-passwords/nextcloud-root
    chown nextcloud:nextcloud /etc/user-passwords/nextcloud-root
  '';

  services.nextcloud.enable = true;
  services.nextcloud.config.adminpassFile = "/etc/user-passwords/nextcloud-root";
  services.nextcloud.config.overwriteProtocol = "https";
  services.nextcloud.hostName = "cloud.squircle.space";
  services.nextcloud.autoUpdateApps.enable = true;
  services.nextcloud.package = pkgs.nextcloud22;
  services.nginx.virtualHosts."cloud.squircle.space" = {
    forceSSL = true;
    enableACME = true;
    extraConfig = ''
      if ($host != "cloud.squircle.space") {
        return 444;
      }
    '';
  };

  networking.firewall.allowedTCPPorts = [ 22 80 443 41177 8080 21064 ];
  networking.firewall.enable = true;
  security.acme.acceptTerms = true;
  security.acme.email = "cert@squircle.space";

  services.nginx.virtualHosts."_" = {
    default = true;
    extraConfig = ''
      return 444;
    '';
  };

  services.borgbackup.smartjobs."rsync" = {
    paths = ["/var" "/etc"];
    exclude = [ "/var/log" ];
    subvolumes = [ "/" ];
    server = import ./server-rsync.net.nix;
    repoName = "borg/pifer/main";
    privateKeyPath = "/var/lib/borg/id_ed25519";
    passwordPath = "/var/lib/borg/password";
    snapshotPath = "/btrfs/snapshots/backup";
  };

  services.home-assistant.enable = true;
  services.home-assistant.openFirewall = true;
  services.home-assistant.configWritable = true;
  services.home-assistant.lovelaceConfigWritable = true;
  services.home-assistant.package = (pkgs.home-assistant.override {
    extraComponents = [
      # "apple_tv" # Tests seem to be failing on NixOS 21.11 for now.  I wasn't using it anyway.
      "automation"
      "default_config"
      "ffmpeg"
      "frontend"
      "group"
      "homekit"
      "homekit_controller"
      "http"
      "hue"
      "light"
      "roomba"
      "scene"
    ];
  }).overrideAttrs (oldAttrs: { doInstallCheck = false; });
  services.home-assistant.autoExtraComponents = false;
  services.home-assistant.config = {
    homeassistant = {
      name = "Home";
      unit_system = "metric";
      time_zone = "UTC";
      customize = "!include customize.yaml";
      latitude = 37.763017;
      longitude = -122.435776;
    };
    default_config = null;
    scene = "!include scenes.yaml";
    automation = "!include automations.yaml";
    shell_command = {
      "up_to_date" = "${upToDate}";
    };
    light = [
      {
        platform = "group";
        name = "Entry";
        entities = [
          "light.hall_1"
          "light.hall_2"
        ];
      }
      {
        platform = "group";
        name = "Bedroom";
        entities = [
          "light.bed_1"
          "light.bed_2"
        ];
      }
      {
        platform = "group";
        name = "Couch";
        entities = [
          "light.leftie"
          "light.rightie"
        ];
      }
      {
        platform = "group";
        name = "Living Room";
        entities = [
          "light.leftie"
          "light.rightie"
          "light.dingle"
        ];
      }
    ];
    frontend = {
      themes = "!include_dir_merge_named themes";
    };
    http = {};
  };
}
