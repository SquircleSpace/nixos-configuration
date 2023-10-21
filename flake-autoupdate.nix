{ config, pkgs, lib, ... }:
let
  shellQuote = import ./shellQuote.nix lib;
  identity = with lib; with types; submodule {
    options = {
      name = mkOption {
        type = str;
        example = "Joanna Schmo";
        description = "The name of the committer/author";
      };
      email = mkOption {
        type = str;
        example = "jschmo@example.com";
        description = "The email address of the committer/author";
      };
    };
  };
  gitConfig = with lib; with types; submodule {
    options = {
      author = mkOption {
        type = nullOr identity;
        default = null;
        description = "The author info git should use";
      };
      committer = mkOption {
        type = nullOr identity;
        default = null;
        description = "The committer info git should use";
      };
    };
  };
  jobOptions = args@{name, ...}: with lib; with types; {
    options = {
      enable = mkOption {
        type = bool;
        default = false;
        description = "Whether to enable auto-update of flake.lock.";
      };
      gitConfig = mkOption {
        type = gitConfig;
        default = {};
        description = "Configuration details for how the job should interact with git.";
      };
      path = mkOption {
        type = path;
        defaultText = "attr name";
        description = "The location of the flake to update.";
      };
      startAt = mkOption {
        type = either str (listOf str);
        example = "daily";
        default = [];
        description = "When to initiate the update expressed as a systemd timer string.";
      };
      persistent = mkOption {
        type = bool;
        description = "Whether the job should run even if the fire time is missed.";
        default = true;
      };
      serviceName = mkOption {
        type = str;
        defaultText = "\"flake-autoupdate-\${...}\"";
        description = "The launchd service name for this job (without .service).";
      };
      startOnAutoUpgrade = mkOption {
        type = bool;
        defaultText = "config.system.autoUpgrade.flake == path";
        description = "Whether the auto-upgrade service should start this service when it starts.";
      };
    };

    config = mkMerge [
      {
        serviceName = mkDefault ("flake-autoupdate-" + lib.replaceStrings ["/"] ["-"] name);
        path = mkDefault name;
      }
      {
        startOnAutoUpgrade = mkDefault (args.config.path == config.system.autoUpgrade.flake);
      }
    ];
  };

  transformIdentity = type: ident: if ident == null then {} else
    {
      "GIT_${type}_NAME" = ident.name;
      "GIT_${type}_EMAIL" = ident.email;
    };
  mkEnv = cfg: transformIdentity "COMMITTER" cfg.gitConfig.committer
               // transformIdentity "AUTHOR" cfg.gitConfig.author;
  mkScript = cfg: ''
    cd ${shellQuote cfg.path}
    if [ -e flake-autoupdate.inhibit ]; then
        echo "Not updating flake due to inhibition file"
        exit 1
    fi
    exec ${config.nix.package}/bin/nix flake update --commit-lock-file
  '';
  mkService = name: cfg: lib.nameValuePair cfg.serviceName {
    description = "Update flake.lock at ${cfg.path}";
    inherit (cfg) startAt enable;
    environment = mkEnv cfg;
    script = mkScript cfg;
    path = [ pkgs.git ];
    serviceConfig = {
      CPUSchedulingPolicy = "idle";
      IOSchedulingClass = "idle";
      ReadWritePaths = [ cfg.path ];
      Type = "oneshot";
    };
    requiredBy = lib.mkIf cfg.startOnAutoUpgrade ["nixos-upgrade.service"];
    before = lib.mkIf cfg.startOnAutoUpgrade ["nixos-upgrade.service"];
  };
  mkTimer = name: cfg: lib.nameValuePair cfg.serviceName {
    timerConfig.Persistent = cfg.persistent;    
  };
  rawCfg = config.services.flakeAutoUpdate;
in
{
  options.services.flakeAutoUpdate = with lib; with types; mkOption {
    type = with lib; with types; attrsOf (submodule jobOptions);
    default = {};
    description = "Flake auto-update jobs";
  };
  config = lib.mkIf (rawCfg != {}) {
    systemd.services = lib.mapAttrs' mkService rawCfg;
    systemd.timers = lib.mapAttrs' mkTimer (lib.filterAttrs (name: cfg: cfg.startAt != []) rawCfg);
  };
}
