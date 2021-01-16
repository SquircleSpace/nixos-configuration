{ config, pkgs, ... }:
{
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  services.openssh.permitRootLogin = "no";
  services.openssh.startWhenNeeded = true;
}
