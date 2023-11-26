{config, pkgs, lib, ...}:
let
  homebridgePackages = import ./homebridge { inherit pkgs; };
  packageModulePath = package: "${package}/lib/node_modules/";
  nodeModulePaths = map packageModulePath (builtins.attrValues homebridgePackages);
  nodePath = builtins.concatStringsSep ":" nodeModulePaths;
  homebridgeWrapped = pkgs.stdenv.mkDerivation rec {
    version = "1.0.0";
    name = "homepi-server-${version}";
    unpackPhase = "true";
    buildPhase = "true";
    installPhase = ''
      mkdir -p $out/bin
      cat > $out/bin/homebridge <<EOF
      #!/bin/sh
      NODE_PATH=${nodePath} exec ${homebridgePackages.homebridge}/bin/homebridge -I "$@"
      EOF
      chmod +x $out/bin/homebridge
    '';
  };
in
{
  users.users.homebridge = {
    home = "/var/lib/homebridge";
    createHome = true;
    group = "homebridge";
    isSystemUser = true;
    isNormalUser = false;
    description = "Home Bridge";
  };

  users.groups.homebridge = {};

  networking.firewall.allowedTCPPorts = lib.range 52100 52150;
  networking.firewall.allowedUDPPorts = (lib.range 52100 52150) ++ [1900 5350 5351 5353];

  systemd.services.homebridge = {
    enable = true;
    # confinement = {
    #   enable = true;
    #   mode = "chroot-only";
    # };
    restartTriggers = [ homebridgeWrapped ];
    script = "exec ${homebridgeWrapped}/bin/homebridge";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "homebridge";
      Restart = "always";
      RestartSec = "15";
      # CapabilityBoundingSet = "CAP_NET_RAW";
      AmbientCapabilities = "CAP_NET_RAW";
    };
  };
}
