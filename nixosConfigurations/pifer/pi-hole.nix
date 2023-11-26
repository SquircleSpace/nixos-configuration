{ config, pkgs, ... }:
let
  piholeVirtualHostConfig = {
    sslCertificate = "/var/cert/pi.hole.crt";
    sslCertificateKey = "/var/cert/pi.hole.key";
    extraConfig = ''
      allow 100.0.0.0/8;
      allow 192.168.1.0/24;
      allow 127.0.0.1;
      deny all;
    '';
    locations."/" = {
      proxyPass = "http://localhost:8080";
    };
  };
in
{
  virtualisation.oci-containers.containers."pi-hole" = {
    image = "pihole/pihole:latest";
    environment = {
      FTLCONF_REPLY_ADDR4 = "192.168.1.142";
    };
    ports = [
      "53:53/tcp"
      "53:53/udp"
      "8080:80/tcp"
    ];
    volumes = [
      "/var/lib/pihole/etc/pihole:/etc/pihole"
      "/var/lib/pihole/etc/dnsmasq.d:/etc/dnsmasq.d"
    ];
  };

  services.nginx.virtualHosts."pi.hole" = piholeVirtualHostConfig;
  services.nginx.virtualHosts."pihole.squircle.space" = piholeVirtualHostConfig;
}
