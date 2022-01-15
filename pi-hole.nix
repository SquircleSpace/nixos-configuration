{ config, pkgs, ... }:
{
  virtualisation.oci-containers.containers."pi-hole" = {
    image = "pihole/pihole:latest";
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
}
