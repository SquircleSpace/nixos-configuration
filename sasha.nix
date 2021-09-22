{ config, pkgs, lib, ...}:
let
  passwordPath = "/etc/user-passwords/important_bird";
in
{
  users.extraUsers.important_bird = {
    isNormalUser = true;
    home = "/home/important_bird";
    description = "a_very_important_bird";
    passwordFile = passwordPath;
    packages = with pkgs; [
      firefox
      discord
      steam
    ];
  };

  system.activationScripts = {
    "Protect ${passwordPath}" = ''
      if [ -e ${passwordPath} ]; then
        chown root:root ${passwordPath}
        chmod go-rwx ${passwordPath}
      fi
    '';
  };
}
