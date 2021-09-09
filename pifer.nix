{ config, lib, pkgs, ... }:
let
  borgPasswordPath = "/var/lib/borg/password";
  borgPrivateKeyPath = "/var/lib/borg/id_ed25519";
  defaultBorgConfig = {
    paths = [ "/var" "/etc" ];
    exclude = [ "/var/log" ];
    encryption.mode = "repokey";
    encryption.passCommand = "cat ${borgPasswordPath}";
    startAt = "daily";
    environment = {
      "BORG_RSH" = "ssh -i ${borgPrivateKeyPath}";
    };
    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 14;
      weekly = 26;
      monthly = 6;
      yearly = 5;
    };
  };
  mkBorgConfig = overrides: lib.recursiveUpdate defaultBorgConfig overrides;

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
    ./homebridge-module.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 4;
  boot.loader.raspberryPi.uboot.configurationLimit = 3;
  boot.kernelPackages = pkgs.linuxPackages_rpi4;

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 21d";
  nix.gc.dates = "Mon *-*-* 00:03:15";
  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;
  system.autoUpgrade.dates = "Mon *-*-* 00:04:40";

  boot.consoleLogLevel = lib.mkDefault 7;

  networking.hostName = "pifer";

  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.eth0.tempAddress = "disabled";
  networking.interfaces.wlan0.useDHCP = true;
  networking.interfaces.wlan0.tempAddress = "disabled";

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "America/LosAngeles";

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
    description = "Ada";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    uid = 1000;
    passwordFile = "/etc/nixos/user-passwords/ada";
    openssh.authorizedKeys.keyFiles = [ ./ada-pubkey ./phone-pubkey ./libbie.pub ];
  };

  system.stateVersion = "20.09";

  networking.networkmanager.enable = false;

  system.activationScripts."Protect /etc/nixos/user-passwords/" = ''
    chown -R root:root /etc/nixos/user-passwords/
    chmod -R go-rwx /etc/nixos/user-passwords/*
    chown nextcloud:nextcloud /etc/nixos/user-passwords/nextcloud-root
    chmod +x /etc/nixos/user-passwords/
  '';

  services.nextcloud.enable = true;
  services.nextcloud.config.adminpassFile = "/etc/nixos/user-passwords/nextcloud-root";
  services.nextcloud.config.overwriteProtocol = "https";
  services.nextcloud.hostName = "cloud.squircle.space";
  services.nextcloud.autoUpdateApps.enable = true;
  services.nextcloud.package = pkgs.nextcloud21;
  services.nginx.virtualHosts."cloud.squircle.space" = {
    forceSSL = true;
    enableACME = true;
    extraConfig = ''
      if ($host != "cloud.squircle.space") {
        return 444;
      }
    '';
  };

#   services.bitwarden_rs.enable = true;
#   services.bitwarden_rs.config = {
#     domain = "https://bw.squircle.space";
#     signupsAllowed = false;
#     invitationsAllowed = false;
#     websocketPort = 3012;
#     rocketPort = 8222;
#     smtpHost = "smtp.dreamhost.com";
#     smtpSsl = true;
#     smtpExplicitTls = true;
#     smtpPort = 465;
#     smtpFrom = "bw@squircle.space";
#     smtpFromName = "Bitwarden_RS";
#     smtpPassword = "MXeQdbX3";
#     smtpUsername = "bw@squircle.space";
#   };
#   services.nginx.virtualHosts."bw.squircle.space" = {
#     forceSSL = true;
#     enableACME = true;
#     locations."/" = {
#       proxyPass = "http://localhost:8222";
#       proxyWebsockets = true;
#       extraConfig = ''
#         allow 192.168.0.0/16;
#         allow 2601:0645:4000:0010::/64;
# #        deny all;
#       '';
#     };
#     locations."/notifications/hub" = {
#       proxyPass = "http://localhost:3012";
#       proxyWebsockets = true;
#       extraConfig = ''
#         allow 192.168.0.0/16;
#         allow 2601:0645:4000:0010::/64;
# #        deny all;
#       '';
#     };
#     locations."/notifications/hub/negotiate" = {
#       proxyPass = "http://localhost:8222";
#       proxyWebsockets = true;
#       extraConfig = ''
#         allow 192.168.0.0/16;
#         allow 2601:0645:4000:0010::/64;
# #        deny all;
#       '';
#     };
#     extraConfig = ''
#       if ($host != "bw.squircle.space") {
#         return 444;
#       }
#     '';
#   };

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

  services.borgbackup.jobs."local" = mkBorgConfig {
    repo = "borg@jobe.lan:main";
  };

  services.borgbackup.jobs."rsync" = mkBorgConfig {
    repo = "7995@usw-s007.rsync.net:borg/pifer/main";
    environment = {
      "BORG_REMOTE_PATH" = "borg1";
    };
  };

  system.activationScripts."Protect borg password" = ''
    chown -R root:root /var/lib/borg
    chmod -R go-rwx /var/lib/borg
  '';

  services.home-assistant.enable = true;
  services.home-assistant.openFirewall = true;
  services.home-assistant.configWritable = true;
  services.home-assistant.lovelaceConfigWritable = true;
  services.home-assistant.package = (pkgs.home-assistant.override {
    extraComponents = [
      "apple_tv"
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
