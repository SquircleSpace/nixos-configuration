{ self, agenix }:
{ config, pkgs, lib, ... }:
let
  cfg = config.squircle.space.ada;
  gitRemoteDoas = self.packages."${pkgs.system}".git-remote-doas;
  cliPackages = with pkgs; [
    btrfs-progs
    file
    gdb
    git
    gitRemoteDoas
    glances
    htop
    lldb
    lsof
    mosh
    psmisc
    roswell
    smartmontools
    tmux
    unzip
    w3m
    wget
    xxd
  ];
  uiPackages = with pkgs; [
    aspellDicts.en
    caffeine-ng
    chromium
    firefox
    gimp
    gitAndTools.git-filter-repo
    gparted
    graphviz
    ispell
    nextcloud-client
    pandoc
    paprefs
    pavucontrol
    stow
    terminator
    vlc
  ];
  kdePackages = with pkgs.plasma5Packages; [
    discover
    kaddressbook
    kleopatra
    kmail
    kmail-account-wizard
    kontact
    korganizer
    okular
    yakuake
  ];
  gnomePackages = with pkgs.gnome; [
    gnome-tweaks
    gnome-shell-extensions
  ];

  # This is NOT intended to be Ada's password.  Its only there so she
  # doesn't get locked out by forgetting to rekey the age encrypted
  # password during new system setup.  The default is miserable to
  # type to motivate Ada to fix her damn password.  For attackers out
  # there, the default password is randomly generated with tons of
  # entropy.  Good luck!
  fallbackHashedPassword = "$y$j9T$m8cSBKkrKZn/VX52yWu6x0$08knDlnniXOSJmPWLYpEDv4gSHMsPl0kOrFj7vFXs5C";
in
with lib; with types;
{
  imports = [ agenix.nixosModules.default ];

  options.squircle.space.ada = {
    enable = mkOption {
      type = bool;
      default = false;
      description = "Whether to create an account for Ada";
    };

    authorizedKeys = mkOption {
      type = listOf str;
      default = [];
      description = "Which SSH public keys can log in to this account.";
    };
    authorizeStandardKeys = mkOption {
      type = bool;
      default = true;
      description = "Whether to trust Ada's usual public keys.";
    };

    home = mkOption {
      type = str;
      default = "/home/ada";
      description = "Where Ada will live.";
    };
    username = mkOption {
      type = str;
      default = "ada";
      description = "Ada's username.";
    };
    isAdmin = mkOption {
      type = bool;
      default = true;
      description = "Should Ada have sudo powers?";
    };
    extraGroups = mkOption {
      type = listOf str;
      default = [];
      description = "Bonus groups that Ada is a member of.";
    };
    description = mkOption {
      type = str;
      default = "Ada";
      description = "Who is this elusive \"ada\"?  What is her name?  The press demands answers!";
    };

    extraPackages = mkOption {
      type = listOf package;
      default = [];
      description = "What software does Ada want?";
    };
    packages.cliUtilities.enable = mkOption {
      type = bool;
      default = true;
      description = "Should ada have her standard CLI utilities?  (yes, yes she should)";
    };
    packages.uiUtilities.enable = mkOption {
      type = bool;
      defaultText = literalExpression "config.services.xserver.enable";
      description = "Should ada have her standard UI programs?";
    };

    hashedPasswordFile = mkOption {
      type = nullOr str;
      default = null;
      description = ''
        The path to Ada's hashed password.  This file should be suitable for use with chpasswd -e.

        Setting this to a non-null value overrides ageEncryptedHashedPassword.  Setting both this and ageEncryptedHashedPassword to null results in *NO PASSWORD* for Ada (not even the fallback one).  You better know what you're doing!
      '';
    };
    ageEncryptedHashedPassword = mkOption {
      type = nullOr path;
      default = ../secrets/adaHashedPassword.age;
      description = ''
        The path to an age encrypted, hashed password for Ada.  After decryption, this file should be suitable for use with chpasswd -e.

        This setting is overridden if hashedPasswordFile is set to a non-null value.  Setting both this and hashedPasswordFile to null results in *NO PASSWORD* for Ada (not even the fallback one).  You better know what you're doing!
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      squircle.space.ada.packages.uiUtilities.enable = mkDefault config.services.xserver.enable;
    }

    (mkIf (cfg.hashedPasswordFile == null && cfg.ageEncryptedHashedPassword != null) {
      users.users."${cfg.username}" = {
        hashedPassword = fallbackHashedPassword;
        hashedPasswordFile = config.age.secrets.adaHashedPassword.path;
      };
      age.secrets.adaHashedPassword.file = cfg.ageEncryptedHashedPassword;
    })

    (mkIf (cfg.hashedPasswordFile != null) {
      users.users."${cfg.username}".hashedPassword = fallbackHashedPassword;
      users.users."${cfg.username}".hashedPasswordFile = cfg.hashedPasswordFile;
    })

    {
      users.users."${cfg.username}" = mkMerge [
        {
          isNormalUser = true;
          home = cfg.home;
          description = cfg.description;
          extraGroups = cfg.extraGroups;
          packages = cfg.extraPackages;
          openssh.authorizedKeys.keys = cfg.authorizedKeys;
        }

        (mkIf config.services.xserver.enable {
          packages = [self.packages."${pkgs.system}".emacs];
        })
        (mkIf (!config.services.xserver.enable) {
          packages = [self.packages."${pkgs.system}".emacs-nox];
        })

        (mkIf cfg.isAdmin {
          extraGroups = ["wheel"];
        })
        (mkIf (cfg.isAdmin && config.networking.networkmanager.enable) {
          extraGroups = ["networkmanager"];
        })

        (mkIf cfg.authorizeStandardKeys {
          openssh.authorizedKeys.keys = self.lib.publicKeys.trustedClients;
        })

        (mkIf cfg.packages.cliUtilities.enable {
          packages = cliPackages;
        })
        (mkIf cfg.packages.uiUtilities.enable {
          packages = uiPackages;
        })
        (mkIf (cfg.packages.uiUtilities.enable && config.services.xserver.desktopManager.plasma5.enable) {
          packages = kdePackages;
        })
        (mkIf (cfg.packages.uiUtilities.enable && config.services.xserver.desktopManager.gnome.enable) {
          packages = gnomePackages;
        })
      ];
    }
  ]);
}
