{ pkgs, config, lib, ... }:
{
  options.hardware.cpuArchitecture = with lib; with types; mkOption {
    type = nullOr str;
    default = null;
    description = ''
      CPU architecture to build for.

      Hint: use `cat /sys/devices/cpu/caps/pmu_name`
    '';
  };

  config = lib.mkIf (config.hardware.cpuArchitecture != null) {
    nixpkgs.overlays = [
      (self: super: {
        stdenv = super.stdenv // {
          mkDerivation = args: super.stdenv.mkDerivation (args // {
            NIX_CFLAGS_COMPILE = toString (args.NIX_CFLAGS_COMPILE or "") + " -mtune=${config.hardware.cpuArchitecture} -march=${config.hardware.cpuArchitecture}";
          });
        };
      })
    ];
  };
}
