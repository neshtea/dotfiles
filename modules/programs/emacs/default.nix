{ config, pkgs, lib, ... }:

let cfg = config.modules.programs.emacs;
in {
  options.modules.programs.emacs = { enable = lib.mkEnableOption "emacs"; };

  config = lib.mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      extraPackages = epkgs: [
        # Basics, ergonomics, movement
        epkgs.consult
        epkgs.corfu
        epkgs.evil

        epkgs.evil-collection
        epkgs.evil-nerd-commenter
        epkgs.evil-org
        epkgs.exec-path-from-shell
        epkgs.general
        epkgs.envrc
        epkgs.no-littering
        epkgs.yasnippet

        # Purescript
        epkgs.psc-ide
        epkgs.purescript-mode

        # Visuals
        epkgs.solaire-mode
        epkgs.cycle-themes
        epkgs.diff-hl
        epkgs.doom-modeline
        epkgs.doom-themes

        # Elixir
        epkgs.alchemist
        epkgs.elixir-mode

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

        epkgs.babel
        epkgs.haskell-mode
        epkgs.helpful
        epkgs.kind-icon
        epkgs.hl-todo
        epkgs.hledger-mode
        epkgs.hydra
        epkgs.lsp-mode
        epkgs.lsp-ui
        epkgs.magit
        epkgs.magit-todos
        epkgs.marginalia
        epkgs.markdown-mode

        epkgs.merlin
        epkgs.merlin-company
        epkgs.ocp-indent
        epkgs.utop

        epkgs.nix-mode
        epkgs.origami

        epkgs.tidal

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
      package = pkgs.emacsMacport;
    };

    xdg.configFile."emacs/early-init.el".source = ./emacs.d/early-init.el;
    xdg.configFile."emacs/init.el".source = ./emacs.d/init.el;
  };
}
