{ emacsPackagesFor, emacs, writeTextDir }:
let
  emacsWithPackages = (emacsPackagesFor emacs).emacsWithPackages;
in
emacsWithPackages (epkgs: [
  (writeTextDir "share/emacs/site-lisp/default.el"
    (builtins.readFile ./emacs-config.el))
]
++ (with epkgs.melpaStablePackages; [
  beacon
  consult
  define-word
  diminish
  exec-path-from-shell
  haskell-mode
  magit
  marginalia
  nix-mode
  orderless
  slime
  smartparens
  volatile-highlights
])
++ (with epkgs.melpaPackages; [
  git-gutter
  git-gutter-fringe
  python-mode
])
++ (with epkgs.elpaPackages; [
  auctex
  undo-tree
  vertico
]))
