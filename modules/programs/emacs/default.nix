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
        epkgs.consult
        epkgs.evil

        epkgs.evil-collection
        epkgs.evil-nerd-commenter
        epkgs.evil-org
        epkgs.exec-path-from-shell
        epkgs.general
        epkgs.envrc

        # Visuals
        epkgs.solaire-mode
        epkgs.cycle-themes
        epkgs.diff-hl
        epkgs.doom-modeline
        epkgs.doom-themes
        epkgs.all-the-icons

        # Elixir
        epkgs.alchemist
        epkgs.elixir-mode

        # Racket
        epkgs.racket-mode

        # Latex
        epkgs.auctex-latexmk

        # Clojure
        epkgs.cider
        epkgs.clojure-mode
        epkgs.clj-refactor

        # Rust
        epkgs.rustic

        # Misc file modes

        epkgs.dockerfile-mode

        epkgs.haskell-mode
        epkgs.lsp-haskell

        epkgs.helpful
        epkgs.hl-todo
        epkgs.hledger-mode
        epkgs.hydra
        epkgs.lsp-mode
        epkgs.lsp-ui
        epkgs.magit
        epkgs.magit-todos
        epkgs.marginalia
        epkgs.markdown-mode

        # Ocaml
        epkgs.merlin
        epkgs.merlin-company
        epkgs.ocp-indent
        epkgs.utop

        epkgs.nix-mode
        epkgs.origami

        epkgs.org
        epkgs.org-roam

        epkgs.projectile
        epkgs.rainbow-delimiters
        epkgs.selectrum
        epkgs.selectrum-prescient
        epkgs.tuareg
        epkgs.use-package
        epkgs.which-key
        epkgs.yaml-mode
        epkgs.zoom
      ];
      package = cfg.emacsPackage;
    };

    xdg.configFile."emacs/early-init.el".source = ./emacs.d/early-init.el;
    xdg.configFile."emacs/init.el".source = ./emacs.d/init.el;
  };
}
