{ pkgs, lib, config, ... }:
{
  networking.firewall.allowedUDPPorts = lib.mkIf (0 != config.services.tailscale.port) [ config.services.tailscale.port ];

  services.tailscale.enable = true;
  environment.systemPackages = [ pkgs.tailscale ];

  # Strict reverse path filtering breaks Tailscale exit node use and
  # some subnet routing setups.
  networking.firewall.checkReversePath = "loose";
}
