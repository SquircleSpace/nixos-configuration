{ pkgs, config, ... }:
{
  nixpkgs.overlays = [
    (self: super: {
      stumpwm = assert super.stumpwm.drvAttrs.src.drvAttrs.rev == "refs/tags/1.0.0";
      super.stumpwm.overrideAttrs (oldAttrs: {
        buildInputs = pkgs.lib.concatLists [ oldAttrs.buildInputs [ pkgs.lispPackages.alexandria ] ];
        name = "stumpwm-20180313";
        src = pkgs.fetchgit {
          url = "https://github.com/stumpwm/stumpwm";
          rev = "v1.0.1-rc";
          sha256 = "12nbq4iyfhzhrzbbbakg2lijmn568skd48dbbb0q4rkcqyzqiz6v";
        };
      });

      yabar = assert super.yabar.drvAttrs.src.drvAttrs.outputHash == "1nw9dar1caqln5fr0dqk7dg6naazbpfwwzxwlkxz42shsc3w30a6";
      super.yabar.overrideAttrs (oldAttrs: {
        name = "yabar-20180314";
        buildInputs = pkgs.lib.concatLists [
          oldAttrs.buildInputs
          [ pkgs.alsaLib pkgs.libxkbcommon pkgs.wirelesstools pkgs.asciidoc pkgs.libxslt ]
        ];
        postPatch = (super.postPatch or "") + ''
          substituteInPlace ./Makefile --replace 'a2x --doctype manpage' 'a2x --no-xmllint --doctype manpage'
        '';
        src = pkgs.fetchFromGitHub {
          owner = "geommer";
          repo = "yabar";
          rev = "c516e8e78d39dd2b339acadc4c175347171150bb";
          sha256 = "1p9lx78cayyn7qc2q66id2xfs76jyddnqv2x1ypsvixaxwcvqgdb";
        };
      });

      firejail = assert super.firejail.version == "0.9.64.4";
        super.firejail.overrideAttrs (oldAttrs: {
          version = "20210614";
          src = pkgs.fetchFromGitHub {
            owner = "netblue30";
            repo = "firejail";
            rev = "e770ab6d858bf8594edb012b1df6b899efb37e94";
            sha256="0zk3w4dvpcdixv6c6hpaf0akazin8h1l4zfl2lff1mmp4y89i57b";
          };
        });

      steam = (super.steam.override {
        extraPkgs = pkgs: with pkgs; [ pango harfbuzz libthai ];
      });

      keyboard-configurator = assert ! (super ? "keyboard-configurator");
        super.callPackage ./system76-keyboard-configurator.nix {};

      etc-nixos-post-receive-hook = self.writeScript "nixos-post-receive-hook.sh" ''
        #! ${self.bash}/bin/bash
        while read oldSHA newSHA ref; do
          if [[ "$ref" != "refs/heads/master" ]]; then
            continue
          fi
          export GIT_WORK_TREE=$(cd $GIT_DIR/../ ; pwd)
          env | grep GIT
          echo ${self.git}/bin/git checkout -f --detach $newSHA
          ${self.git}/bin/git checkout -f --detach $newSHA
          break
        done
      '';
    })
  ];
}
