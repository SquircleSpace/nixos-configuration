{ self, config, lib, pkgs, nixpkgs2211-flake, ... }:
let
  postSensor = pkgs.writeScript "postSensor" ''
    
  '';
  upToDate = pkgs.writeScript "upToDate" ''
    #! ${pkgs.bash}/bin/bash
    ${pkgs.systemd}/bin/systemctl is-active nixos-upgrade.service || ! ${pkgs.systemd}/bin/systemctl is-failed nixos-upgrade.service
    ${postSensor} systemUpToDate $?
  '';
  homeAssistantVirtualHostConfig = {
    sslCertificate = "/var/cert/home.lan.crt";
    sslCertificateKey = "/var/cert/home.lan.key";
    extraConfig = ''
      allow 100.0.0.0/8;
      allow 192.168.1.0/24;
      allow 127.0.0.1;
      deny all;
    '';
    locations."/" = {
      proxyPass = "http://localhost:8123";
    };
  };
in
{
  users.extraUsers.ada.password = builtins.trace ''
    =====================================================
    == WARNING WARNING WARNING WARNING WARNING WARNING ==
    =====================================================
    This version of NixOS no longer supports the password format you're using.
    Your password has been changed to "asdf" temporarily.  Regenerate your
    password file with mkpasswd.
  '' "asdf";
  users.motd = "YOU MUST REGENERATE YOUR PASSWORD.";
  system.activationScripts."Warn about reset password" = ''
    echo 'This version of NixOS no longer supports the password format you're using.'
    echo 'Your password has been changed to "asdf" temporarily.  Regenerate your'
    echo 'password file with mkpasswd.'
    sleep 20
  '';

  imports = [
    ./homebridge-module.nix
    ./rss4email.nix
    ./pi-hole.nix
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
  services.openssh.settings.PermitRootLogin = "no";

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
    openssh.authorizedKeys.keyFiles = [
      self.adaExtras.publicKeys.jobe
      self.adaExtras.publicKeys.phone
      self.adaExtras.publicKeys.libbie
    ];
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
  services.nextcloud.package = nixpkgs2211-flake.legacyPackages."aarch64-linux".nextcloud24;
  services.nginx.virtualHosts."cloud.squircle.space" = {
    forceSSL = true;
    sslCertificate = "/var/cert/cloud.squircle.space.crt";
    sslCertificateKey = "/var/cert/cloud.squircle.space.key";
    extraConfig = ''
      if ($host != "cloud.squircle.space") {
        return 444;
      }
    '';
  };

  networking.firewall.allowedTCPPorts = [ 22 80 443 41177 8080 21064 8123 ];
  networking.firewall.enable = true;

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
    server = self.adaExtras.backupServers.rsync;
    repoName = "borg/pifer/main";
    privateKeyPath = "/var/lib/borg/id_ed25519";
    passwordPath = "/var/lib/borg/password";
    snapshotPath = "/btrfs/snapshots/backup";
  };

  virtualisation.oci-containers.containers.home-assistant = {
    autoStart = true;
    environment.TZ = "America/Los_Angeles";
    image = "ghcr.io/home-assistant/home-assistant:stable";
    volumes = [ "/var/lib/home-assistant/config:/config" ];
    extraOptions = [ "--privileged" "--network=host"];
  };

  virtualisation.oci-containers.containers.zwave-js-server = {
    autoStart = true;
    image = "zwavejs/zwavejs2mqtt:latest";
    volumes = [ "/var/lib/zwave-js-server/:/usr/src/app/store" ];
    extraOptions = [ "--device=/dev/serial/by-id/usb-Silicon_Labs_Zooz_ZST10_700_Z-Wave_Stick_b2d4b15fc360ec11abd83f7625bfaa52-if00-port0:/dev/zwave" ];
    ports = [ "8091:8091" "3000:3000" ];
  };

  services.nginx.virtualHosts."home.lan" = homeAssistantVirtualHostConfig;
  services.nginx.virtualHosts."home.squircle.space" = homeAssistantVirtualHostConfig;
}
