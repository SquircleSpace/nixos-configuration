{ self, config, lib, pkgs, ... }:
let
  postSensor = pkgs.writeScript "postSensor" ''
    
  '';
  upToDate = pkgs.writeScript "upToDate" ''
    #! ${pkgs.bash}/bin/bash
    ${pkgs.systemd}/bin/systemctl is-active nixos-upgrade.service || ! ${pkgs.systemd}/bin/systemctl is-failed nixos-upgrade.service
    ${postSensor} systemUpToDate $?
  '';
  homeAssistantVirtualHostConfig = {
    forceSSL = true;
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
      extraConfig = ''
        proxy_set_header   Host             $host;
        proxy_set_header   X-Real-IP        $remote_addr;
        proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;
        proxy_set_header   Upgrade          $http_upgrade;
        proxy_set_header   Connection       "upgrade";
      '';
    };
  };
in
{
  squircle.space.ada.enable = true;
  users.users.ada = {
    uid = 1000;
  };

  imports = [
    ./homebridge-module.nix
    ./rss4email.nix
    ./wg.nix
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/b9755c1c-e633-4498-a4c7-3b07a539d6b9";
    fsType = "btrfs";
    options = [ "subvol=/root" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/b9755c1c-e633-4498-a4c7-3b07a539d6b9";
    fsType = "btrfs";
    options = [ "subvol=/nix" ];
  };

  fileSystems."/btrfs" = {
    device = "/dev/disk/by-uuid/b9755c1c-e633-4498-a4c7-3b07a539d6b9";
    fsType = "btrfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/3BB6-E6F7";
    fsType = "vfat";
  };

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 21d";
  nix.gc.dates = "Mon *-*-* 00:03:15";
  system.autoUpgrade.enable = true;
  system.autoUpgrade.flake = "/etc/nixos";
  system.autoUpgrade.allowReboot = true;
  system.autoUpgrade.dates = "Mon *-*-* 00:04:40";
  system.autoUpgrade.flags = [ "--commit-lock-file" "--recreate-lock-file" ];
  environment.etc."nixos/.git/hooks/post-receive" = {
    enable = true;
    source = self.packages."${pkgs.system}".etc-nixos-post-receive-hook;
  };

  boot.consoleLogLevel = lib.mkDefault 7;

  # https://web.archive.org/web/20230814074946/https://www.eisfunke.com/posts/2023/nixos-on-raspberry-pi-4.html
  # https://web.archive.org/web/20230814075812/https://git.eisfunke.com/config/nixos/-/blob/main/devices/amethyst.nix
  boot.initrd.availableKernelModules = [
    "usbhid"
	  "usb_storage"
	  "vc4"
	  "pcie_brcmstb" # required for the pcie bus to work
	  "reset-raspberrypi" # required for vl805 firmware to load
  ];
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 7;
  };
  hardware.enableRedistributableFirmware = true;

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
  services.openssh.settings.PasswordAuthentication = false;
  services.openssh.settings.PermitRootLogin = "no";

  services.fail2ban.enable = true;
  services.fail2ban.bantime-increment.enable = true;
  services.fail2ban.bantime-increment.maxtime = "30d";

  users.mutableUsers = false;

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
  services.nextcloud.package = pkgs.nextcloud27;
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

  services.adguardhome = {
    enable = true;
    openFirewall = true;
    mutableSettings = false;
    settings = {
      users = [
        {
          name = "ada";
          # Good luck.
          password = "$2y$10$7KIPMFc67n5anIJxQAUc7OLwELyG5nqfnFoLA4fQfjzKVkMVPH1wi";
        }
      ];
      http.address = "0.0.0.0:8080";
      dns.bind_hosts = [ "0.0.0.0" ];
      dns.upstream_dns = [ "https://dns.cloudflare.com/dns-query" ];
      dns.bootstrap_dns = [ "1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ];
      statistics.enabled = true;
      querylog.enabled = false;
      filters = [
        {
          enabled = true;
          url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_1.txt";
          name = "AdGuard DNS filter";
          id = 1;
        }
        {
          enabled = true;
          url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_2.txt";
          name = "AdAway Default Blocklist";
          id = 2;
        }
      ];
    };
  };
  services.nginx.virtualHosts."pihole.squircle.space" = {
    forceSSL = true;
    sslCertificate = "/var/cert/pihole.squircle.space.crt";
    sslCertificateKey = "/var/cert/pihole.squircle.space.key";
    extraConfig = ''
      allow 100.0.0.0/8;
      allow 127.0.0.1;
      deny all;
    '';
    locations."/".proxyPass = "http://localhost:8080";
  };

  networking.firewall.allowedTCPPorts = [ 22 80 443 41177 21064 ];
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
    server = self.lib.backupServers.rsync;
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

  services.nginx.virtualHosts."home.lan" = homeAssistantVirtualHostConfig;
  services.nginx.virtualHosts."home.squircle.space" = homeAssistantVirtualHostConfig;
}
