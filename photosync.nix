{ config, pkgs, lib, ... }:
{
  users.extraUsers.photosync = {
    createHome = false;
    home = "/var/empty";
    isSystemUser = true;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCh6G74U+/DYTYiCxuxIJCg3VFmJogkJrxwQuFn7K4jMZoZBXq4wUvIL+cFi2mlpmBANhmkuKrohf3wIXUuvwmtSIGXfVDrYMoVdrm2gx+Pvo5FHysZA9qSn3KlSJa5Q/C51o7oP9yrmAuMJDdh+Acv+sBIw3JdR6qErwOEkJ02M5jwdF76cRm+xXU/jfObEcRBE/L73+MxZ2kXPzk7RlcsNfhzEDWe5SDMr2wGEygXUYENWdoS30DHCGiLHaXu5Ft/7SCJUVcrtciAVQvWwcb5OyESXo02T4QX7cTdBNhQhRb7HUFyPttESRby25t/VdEMpZ54R7QQGfHMfUv/FKW5mZ0rgYnMvnQarQs+75fjY0iBuUR/ZaPCwpZr2cnxFbK/JarhCDZMBRM/F28QgOr+UMi/BMO/Lp48orGRUdATc+lRlzv4S6QQRC6qTXySfL6yoxtPz8NHnBwp/1QVH+3xhgWZG/v5Icw1NZPTGu6N/T7hzKBigKJCiaBrwKd/iGk= ada@blues"
    ];
    useDefaultShell = true;
  };

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
  '';
}
