{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./machine.nix
  ];

  environment.systemPackages = with pkgs; [
    # Keep the global environment clean
  ];

  # You want a UI?  I got a UI!
  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.windowManager.stumpwm.enable = true;

  # Manage that network
  networking.networkmanager.enable = true;

  # Emacs is bestest
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Everyone loves logging in
  users.extraUsers.brad = {
    isNormalUser = true;
    home = "/home/brad";
    description = "Brad Jensen";
    extraGroups = [ "wheel" "networkmanager" ];
    uid = 1000;
    openssh.authorizedKeys.keyFiles = [ ./brad-pubkey ];
    passwordFile = "/etc/nixos/user-passwords/brad";
    packages = with pkgs; [
      # Essentials
      git emacs tmux stow
      # Love 'em
      file htop lsof mosh nox nix-repl sbcl wget xxd
      # For screen locking
      xscreensaver
    ];
  };
  users.mutableUsers = false;

  system.activationScripts = {
    "Protect /etc/nixos/user-passwords/" = ''
      chown -R root:root /etc/nixos/user-passwords/
      chmod -R go-rwx /etc/nixos/user-passwords/
    '';
  };
}
