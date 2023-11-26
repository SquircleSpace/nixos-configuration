{ pkgs, emacs }:
let
  emacsWithPackages = (pkgs.emacsPackagesFor emacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    magit
    haskell-mode
    ido-yes-or-no
    slime
    ac-slime
    smartparens
    unfill
    volatile-highlights
    define-word
    nix-mode
    exec-path-from-shell
    beacon
    diminish
    flx-ido
  ])
  ++ (with epkgs.melpaPackages; [
    python-mode
    git-gutter
    git-gutter-fringe
  ])
  ++ (with epkgs.elpaPackages; [
    undo-tree
    auctex
  ]))
