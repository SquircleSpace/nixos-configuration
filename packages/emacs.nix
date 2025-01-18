{ emacsPackagesFor
, emacs
, writeText
, runCommand
, lib
, extraInitFiles ? []
, extraPackages ? []
}:
let
  emacsWithPackages = (emacsPackagesFor emacs).emacsWithPackages;
  withNativeCompilation = emacs.withNativeCompilation or emacs.nativeComp or false;
  optionalString = condition: str: if condition then str else "";

  compile = {sourceFile, installName}: runCommand "compiled-${builtins.baseNameOf sourceFile}" {
    inherit emacs sourceFile installName;
  } ''
    mkdir -p "$out/share/emacs/site-lisp/$(dirname "$installName")"
    cp "$sourceFile" "$out/share/emacs/site-lisp/$installName"

    $emacs/bin/emacs --batch -f batch-byte-compile "$out/share/emacs/site-lisp/$installName"

    ${optionalString withNativeCompilation ''
      mkdir -p $out/share/emacs/native-lisp
      $emacs/bin/emacs --batch \
        --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp/\")" \
        -f batch-native-compile "$out/share/emacs/site-lisp/$installName"
    ''}
  '';

  defaultInitFile = ./emacs-config.el;

  initFiles = [defaultInitFile] ++ extraInitFiles;
  initPackageRecords = lib.zipListsWith (serial: sourceFile: rec {
    installName = "default/${builtins.toString serial}-${builtins.baseNameOf sourceFile}";
    package = compile { inherit sourceFile installName; };
  }) (builtins.genList (a: a + 1) (builtins.length initFiles)) initFiles;
  defaultFiles = builtins.catAttrs "installName" initPackageRecords;
  defaultPackages = builtins.catAttrs "package" initPackageRecords;

  # We want to allow loading eln or elc files when possible.  As far
  # as I can tell, the way to do that is by using load instead of
  # load-file and removing the .el extension.  Emacs will then try to
  # locate a compiled version of the file before trying the .el file
  # itself.
  loadStatements = builtins.map (installName: "  (load (concat here \"${lib.strings.removeSuffix ".el" installName}\"))") defaultFiles;
  compoundInitFile = writeText "default.el" ''
  ;; -*- lexical-binding: t -*-
  (let ((here (file-name-parent-directory load-file-name)))
  ${builtins.concatStringsSep "\n" loadStatements})
  '';

  trivialInitPackage = compile { sourceFile = defaultInitFile; installName = "default.el"; };
  compoundInitPackage = compile { sourceFile = compoundInitFile; installName = "default.el"; };

  initPackages = if (builtins.tail initFiles != []) then [compoundInitPackage] ++ defaultPackages else [trivialInitPackage];

  prioritizedEpkgs = epkgs: epkgs.elpaPackages // epkgs.melpaPackages;
in
emacsWithPackages (epkgs:
  initPackages
  ++ (with (prioritizedEpkgs epkgs); [
    auctex
    beacon
    color-theme-sanityinc-tomorrow
    company
    consult
    define-word
    diminish
    doom-themes
    exec-path-from-shell
    expand-region
    git-gutter
    git-gutter-fringe
    haskell-mode
    htmlize
    magit
    marginalia
    nix-mode
    olivetti
    orderless
    org-bullets
    paredit
    python-mode
    sly
    solarized-theme
    spacemacs-theme
    swift-mode
    undo-tree
    vertico
    volatile-highlights
    zenburn-theme
  ])
  ++ (if builtins.isFunction extraPackages
      then
        extraPackages epkgs
      else
        (map
          (thing: if builtins.isString thing then epkgs."${thing}" else thing)
          extraPackages)))
