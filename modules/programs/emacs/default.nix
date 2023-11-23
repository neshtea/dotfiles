{ config, pkgs, lib, ... }:

let cfg = config.modules.programs.emacs;
in {
  options.modules.programs.emacs = {
    enable = lib.mkEnableOption "emacs";
    emacsPackage = lib.mkOption {
      type = lib.types.package;
      example = lib.literalExpression "pkgs.emacsMacport";
      description =
        "The emacs package that should be used as a base for emacs.";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      extraPackages = epkgs: [
        # Basics, ergonomics, movement
        epkgs.use-package
        epkgs.consult
        epkgs.emacsql
        epkgs.envrc
        epkgs.exec-path-from-shell
        epkgs.reformatter

        # Visuals
        epkgs.diff-hl
        epkgs.default-text-scale
        epkgs.gruvbox-theme
        epkgs.zenburn-theme
        epkgs.doom-themes
        epkgs.catppuccin-theme

        # Latex
        epkgs.auctex-latexmk

        # Clojure
        epkgs.cider
        epkgs.clj-refactor
        epkgs.clojure-mode

        # Common Lisp
        epkgs.sly
        epkgs.sly-quicklisp

        # Scheme
        epkgs.geiser
        epkgs.geiser-guile
        epkgs.geiser-chicken

        # Racket
        epkgs.racket-mode

        # Haskell
        epkgs.haskell-mode
        epkgs.lsp-haskell

        # Ocaml
        epkgs.merlin
        epkgs.merlin-company
        epkgs.ocp-indent
        epkgs.utop

        # Rust
        epkgs.rustic

        # Misc file modes
        epkgs.dockerfile-mode
        epkgs.nix-mode

        # Unsorted, general stuff
        epkgs.benchmark-init
        epkgs.helpful
        epkgs.hl-todo
        epkgs.hledger-mode
        epkgs.magit
        epkgs.marginalia
        epkgs.markdown-mode
        epkgs.org
        epkgs.org-roam
        epkgs.org-roam-ui
        epkgs.org-appear
        epkgs.projectile
        epkgs.tuareg
        epkgs.which-key
        epkgs.yaml-mode
        epkgs.vertico
        epkgs.orderless
        epkgs.zoom
      ];
      package = cfg.emacsPackage;
    };

    xdg.configFile."emacs/early-init.el".source = ./emacs.d/early-init.el;
    xdg.configFile."emacs/init.el".source = ./emacs.d/init.el;
  };
}
