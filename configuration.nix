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
  services.xserver.windowManager.default = "stumpwm";
  services.xserver.desktopManager.default = "none";

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
      git emacs tmux stow firefox terminator
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

  nixpkgs.overlays = [
    (self: super: {
      stumpwm = assert super.stumpwm.drvAttrs.src.drvAttrs.rev == "refs/tags/1.0.0";
      super.stumpwm.overrideAttrs (oldAttrs: {
        buildInputs = pkgs.lib.concatLists [ oldAttrs.buildInputs [ pkgs.lispPackages.alexandria ] ];
        name = "stumpwm-20180313";
        src = pkgs.fetchgit {
          url = "https://github.com/stumpwm/stumpwm";
          rev = "8a19932093fe8cc78955459bc9213f2329bf91e7";
          sha256 = "17s6ij6fl9ms6rwvxn5pl8icbr7kk0laf0vlr7qwjyjfqa97iw5c";
        };
      });
    })
  ];
}
