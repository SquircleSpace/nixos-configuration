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
    })
  ];
}
