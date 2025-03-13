{ self, config, pkgs, lib, ... }:
{
  users.extraUsers.photosync = {
    createHome = false;
    home = "/var/empty";
    isSystemUser = true;
    useDefaultShell = true;
    group = "photosync";
    openssh.authorizedKeys.keys = [ self.lib.publicKeys.users.ada.photosync ];
  };
  users.groups.photosync = {};
  programs.firejail.enable = true;
  users.extraUsers.ada.extraGroups = ["photosync"];

  fileSystems."/var/lib/photosync" = {
    depends = [
      # The mounts above have to be mounted in this given order
      "/btrfs"
    ];
    device = "/btrfs/photos/phone";
    fsType = "none";
    options = [
      "bind"
    ];
  };

  services.openssh.extraConfig = let
    profile = pkgs.stdenv.mkDerivation {
      name = "photosync-sftp-profile";
      unpackPhase = "true";
      installPhase = "true";
      buildPhase = ''
        mkdir $out

        cat >> $out/profile <<EOF
        nonewprivs
        nosound
        novideo
        net none
        whitelist /var/lib/photosync/
        read-write /var/lib/photosync/
        private-dev
        private-lib
        private-etc passwd
        private-tmp
        whitelist /run/user/
        whitelist /nix/store/
        EOF
      '';
    };
  in
    ''
    Match User photosync
            ForceCommand /run/wrappers/bin/firejail --quiet --include=${profile}/profile ${pkgs.openssh}/libexec/sftp-server
    Match all
  '';
}
