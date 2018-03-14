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
          rev = "8a19932093fe8cc78955459bc9213f2329bf91e7";
          sha256 = "17s6ij6fl9ms6rwvxn5pl8icbr7kk0laf0vlr7qwjyjfqa97iw5c";
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
    })
    ];
}
