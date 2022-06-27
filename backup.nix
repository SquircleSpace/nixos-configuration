{ config, pkgs, lib, ... }:
let
  cleanup = pkgs.stdenv.mkDerivation {
    name = "btrfs-subvolume-cleanup";
    version = "1.0.0";
    unpackPhase = "true";
    buildPhase = "true";
    buildInputs = [ pkgs.makeWrapper ];
    installPhase = ''
      mkdir -p $out/bin
      makeWrapper ${./btrfs-cleanup.sh} $out/bin/btrfs-subvolume-cleanup --prefix PATH : "${pkgs.btrfs-progs}/bin"
    '';
  };
  dirname = path: let
    parts = lib.splitString "/" (assert (lib.assertMsg (path != "") "Path cannot be empty"); path);
    head = lib.head parts;
    tail = lib.tail parts;
    cleanedTail = lib.remove "." (lib.remove "" tail);
    chopHead = (head == "" || head == ".");
    fixedHead = if chopHead then head else ".";
    fixedTail = if chopHead then cleanedTail else lib.concatLists [ [ head ] cleanedTail ];
    newTail = lib.take ((lib.length fixedTail) - 1) fixedTail;
    newParts = lib.concatLists [ [ fixedHead ] newTail ];
  in if newParts == [""] then "/" else lib.concatStringsSep "/" newParts;

  serverType = with lib; with types; submodule {
    options = {
      hostname = mkOption {
        type = str;
        example = "example.com";
        description = "The remote to backup to.";
      };
      user = mkOption {
        type = str;
        example = "user";
        description = "The username to log into the remote with.";
      };
      publicKeyFile = mkOption {
        type = path;
        description = "The hosts's public key.";
      };
      borgCommand = mkOption {
        type = str;
        example = "borg1";
        default = "borg";
        description = "How to invoke borg on the remote.";
      };
    };
  };
  smartjobType = with lib; with types; submodule {
    options = {
      paths = mkOption {
        type = listOf path;
        example = [ "/home" ];
        description = "Paths to backup.";
      };
      subvolumes = mkOption {
        type = listOf path;
        example = [ "/home" ];
        description = "Subvolumes to snapshot before initiating backup.";
      };
      exclude = mkOption {
        type = listOf path;
        example = [ "/var/log" ];
        description = "Paths to exclude from the backup.";
      };
      server = mkOption {
        type = serverType;
        description = "The server to backup to";
      };
      repoName = mkOption {
        type = str;
        example = "borg/main";
        description = "A path on the remote to the repository to backup to.";
      };
      startAt = mkOption {
        type = str;
        example = "weekly";
        default = "daily";
        description = "When to initiate the backup expressed as a systemd timer string.";
      };
      privateKeyPath = mkOption {
        type = path;
        example = "/var/borg/key";
        description = "The private key to use for logging in to the server.";
      };
      passwordPath = mkOption {
        type = path;
        example = "/var/borg/password";
        description = "The password to use for encrypting the backup.";
      };
      snapshotPath = mkOption {
        type = path;
        example = "/var/borg/snapshot";
        description = "Where to store snapshots when initiating a backup";
      };
    };
  };
  checkSnapshotPathIsSafe = pkgs.writeScript "check-path.sh" ''
    #!${pkgs.runtimeShell}
    ORIGINAL="$1"
    check() {
      if ! [ "$1" = "/" ] && ! check "$(dirname "$1")"; then
        return $?
      fi

      if ! [ -d "$1" ]; then
        echo "$1 is not a directory!  $ORIGINAL is not suitable for backups!" >&2
        return 1
      elif [ "$(stat --format="%G" "$1")" != "root" ] || [ "$(stat --format="%U" "$1")" != "root" ]; then
        echo "$1 is not owned by root.  $ORIGINAL is not suitable for backups!" >&2
        return 1
      else
        return 0
      fi
    }
    check "$1"
  '';
  shellQuote = import ./shellQuote.nix lib;
  mkActivationScript = name: cfg: lib.nameValuePair "Protect borgbackup for ${name}" ''
    chown root:root ${shellQuote cfg.privateKeyPath} ${shellQuote cfg.passwordPath}
    chmod go-rwx ${shellQuote cfg.privateKeyPath} ${shellQuote cfg.passwordPath}
  '';
  mkKnownHosts = name: cfg: lib.nameValuePair cfg.server.hostname {
    hostNames = [ cfg.server.hostname ];
    publicKeyFile = cfg.server.publicKeyFile;
  };
  snapshotsRootDirectory = cfg: lib.any (x: x == "/") cfg.subvolumes;
  mkBorgbackupJob = name: cfg: lib.nameValuePair name {
    repo = "${cfg.server.user}@${cfg.server.hostname}:${cfg.repoName}";
    startAt = cfg.startAt;
    environment = {
      "BORG_REMOTE_PATH" = cfg.server.borgCommand;
      "BORG_RSH" = "ssh -i ${cfg.privateKeyPath}";
    };
    encryption.mode = "repokey";
    encryption.passCommand = "cat \"${cfg.passwordPath}\"";
    compression = "auto,lzma";
    prune.keep = {
      within = "1d"; # Keep all archives from the last day
      daily = 5;
      weekly = 13;
      monthly = 6;
      yearly = 5;
    };

    paths = map (p: "./${p}") cfg.paths;
    readWritePaths = [ (dirname cfg.snapshotPath) ];
    exclude = map (p: "./${p}") cfg.exclude;

    preHook = ''
      is_subvolume() {
        [ -e "$1" ] && [ "$(stat -f --format="%T" "$1")" == "btrfs" ] && [ "$(stat --format="%i" "$1")" == "256" ]
      }

      capture() {
        SUBVOL_PATH="$1"
        echo >&2 "Capturing $SUBVOL_PATH"
        if ! is_subvolume "$SUBVOL_PATH"; then
          echo "$SUBVOL_PATH is not a subvolume!  Refusing to proceed." >&2
          exit 1
        fi
        mkdir -p "$(dirname "$SNAPSHOT_PATH/$SUBVOL_PATH")"
        ${pkgs.btrfs-progs}/bin/btrfs subvolume snapshot "$SUBVOL_PATH" "$SNAPSHOT_PATH/$SUBVOL_PATH"
      }

      seal() {
        echo >&2 "Sealing $SNAPSHOT_PATH/$1"
        ${pkgs.btrfs-progs}/bin/btrfs property set -t subvol "$SNAPSHOT_PATH/$1" ro true
      }

      SNAPSHOT_PATH=${shellQuote cfg.snapshotPath}/"$(date --rfc-3339=seconds)"
      mkdir -p "$(dirname "$SNAPSHOT_PATH")"
      ${checkSnapshotPathIsSafe} "$(dirname "$SNAPSHOT_PATH")" || exit $?

      if [ -e "$SNAPSHOT_PATH" ]; then
        echo "$SNAPSHOT_PATH already exists!" >&2
        exit 1
      fi

      # TODO: Verify subvol path doesn't include /../

      ${lib.optionalString (! snapshotsRootDirectory cfg) ''
        # Set up a root subvol for this backup
        ${pkgs.btrfs-progs}/bin/btrfs subvolume create "$SNAPSHOT_PATH"
      ''}

      # Create snapshots
    '' +
    (lib.concatMapStrings (subvol: ''
      capture ${shellQuote subvol}
    '') (lib.sort (x: y: x < y) cfg.subvolumes)) + ''

      # Seal snapshots
    '' + (lib.concatMapStrings (subvol: ''
      seal ${shellQuote subvol}
    '') (lib.sort (x: y: x > y) cfg.subvolumes)) +
    (lib.optionalString (! snapshotsRootDirectory cfg) ''
      seal /
    '') + ''

      # Move into the backup directory so we can use relative paths for backup
      cd "$SNAPSHOT_PATH"
    '';
  };
in
{
  options.services.borgbackup.smartjobs = with lib; with types; mkOption {
    type = attrsOf smartjobType;
    default = {};
    description = "borgbackup jobs that automatically perform btrfs snapshots.";
  };
  config = lib.mkIf (config.services.borgbackup.smartjobs != {}) {
    services.borgbackup.jobs = lib.mapAttrs' mkBorgbackupJob config.services.borgbackup.smartjobs;
    system.activationScripts = lib.mapAttrs' mkActivationScript config.services.borgbackup.smartjobs;
    programs.ssh.knownHosts = lib.mapAttrs' mkKnownHosts config.services.borgbackup.smartjobs;
    environment.systemPackages = [ cleanup ];
  };
}
