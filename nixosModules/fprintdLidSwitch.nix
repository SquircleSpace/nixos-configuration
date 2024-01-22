{ config, lib, pkgs, ... }:
let
  cfg = config.squircle.space.fprintd;
  checkLid = pkgs.writeShellScript "check-lid" ''
    set -e
    if ${pkgs.gnugrep}/bin/grep -q open /proc/acpi/button/lid/LID0/state; then
      ${pkgs.coreutils}/bin/unlink /run/systemd/transient/fprintd.service || ! test -e /run/systemd/transient/fprintd.service
      ${pkgs.systemd}/bin/systemctl daemon-reload
    else
      ${pkgs.coreutils}/bin/ln -s /dev/null /run/systemd/transient/fprintd.service || test -h /run/systemd/transient/fprintd.service
      ${pkgs.systemd}/bin/systemctl daemon-reload
      ${pkgs.systemd}/bin/systemctl stop fprintd.service
    fi
  '';
in
{
  options.squircle.space.fprintd.disableOnLidClose = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "Whether fprintd should be disabled when the laptop lid is closed.";
  };

  config.services.acpid = lib.mkIf cfg.disableOnLidClose {
    lidEventCommands = "${checkLid}";
    enable = true;
  };
}
