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

        epkgs.envrc
        epkgs.evil-collection
        epkgs.evil-nerd-commenter
        epkgs.evil-org
        epkgs.exec-path-from-shell
        epkgs.general

        # Visuals
        epkgs.all-the-icons
        epkgs.cycle-themes
        epkgs.diff-hl
        epkgs.modus-themes # until we're on emacs 28 everywehere

        # Elixir
        epkgs.alchemist
        epkgs.elixir-mode

        # Racket
        epkgs.racket-mode

        # Latex
        epkgs.auctex-latexmk

        # Clojure
        epkgs.cider
        epkgs.clj-refactor
        epkgs.clojure-mode

        # Haskell
        epkgs.haskell-mode
        epkgs.lsp-haskell

        # Ocaml
        epkgs.merlin
        epkgs.merlin-company
        epkgs.ocp-indent
        epkgs.utop

        # Misc file modes
        epkgs.dockerfile-mode
        epkgs.nix-mode

        # Unsorted, general stuff
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
        epkgs.org
        epkgs.org-roam
        epkgs.origami
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
