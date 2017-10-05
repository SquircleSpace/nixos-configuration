{ config, lib, pkgs, ... }:
let private-nixpkgs = import /home/brad/private-nixpkgs {};
in
{
  imports = [];
  networking.firewall.allowedTCPPorts = [ 12345 ];
  networking.firewall.allowedUDPPorts = [ 12345 1900 5350 5351 5353 ];

  systemd.services."hkwindow" = {
    path = [ private-nixpkgs.hkwindow private-nixpkgs.boot-windows pkgs.sudo pkgs.efibootmgr pkgs.systemd ];
    wantedBy = [ "network-online.target" ];
    description = "HomeKit Window Service";
    script = ''
      PATH="${config.security.wrapperDir}:$PATH"
      cd /etc/HomeKitDevices && exec sudo -u _hkwindow hkwindow
    '';
  };

  security.wrappers = {
    boot-windows = {
      source = "${private-nixpkgs.boot-windows.out}/bin/boot-windows";
      owner = "root";
      setuid = true;
    };
  };

  users.extraUsers."_hkwindow" = {
    isNormalUser = false;
    createHome = false;
    description = "hkwindow";
  };

  environment.etc."HomeKitDevices/ComputerWindow" = {
    source = /home/brad/private-nixpkgs/pkgs/hkwindow/ComputerWindow;
  };
}
