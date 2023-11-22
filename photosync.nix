{ self, config, pkgs, lib, ... }:
{
  users.extraUsers.photosync = {
    createHome = false;
    home = "/var/empty";
    isSystemUser = true;
    useDefaultShell = true;
    group = "photosync";
    openssh.authorizedKeys.keyFiles = [ self.lib.publicKeys.photosync ];
  };
  users.groups.photosync = {};

  services.openssh.extraConfig = let
    closure = pkgs.closureInfo { rootPaths = [pkgs.openssh]; };
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
        whitelist /crypt/photos/phone
        private-dev
        private-lib
        private-etc passwd
        private-tmp
        whitelist /run/user/
        EOF

        cat ${closure}/store-paths | while read storePath; do
          echo whitelist $storePath
        done >> $out/profile
      '';
    };
  in
    ''
    Match User photosync
            ForceCommand /run/wrappers/bin/firejail --quiet --include=${profile}/profile --shell=none ${pkgs.openssh}/libexec/sftp-server
    Match all
  '';
}
