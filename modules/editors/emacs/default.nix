{ config, pkgs, lib, ... }:

let
  cfg = config.modules.editors.emacs;
in
  {
    options.modules.editors.emacs = {
      enable = lib.mkEnableOption "emacs";
    };

    config = lib.mkIf cfg.enable {
      programs.emacs = {
        enable = true;
        extraPackages = epkgs: [
          epkgs.alchemist  # for elixir
          epkgs.auctex-latexmk
          epkgs.babel
          epkgs.cider
          epkgs.clojure-mode
          epkgs.clj-refactor
          epkgs.company
          epkgs.consult
          epkgs.cycle-themes
          epkgs.diff-hl
          epkgs.dockerfile-mode
          epkgs.doom-modeline
          epkgs.doom-themes
          epkgs.elixir-mode  # for elixir
          epkgs.evil
          epkgs.evil-collection
          epkgs.evil-nerd-commenter
          epkgs.evil-org
          epkgs.exec-path-from-shell
          epkgs.general
          epkgs.gnus-alias
          epkgs.helpful
          epkgs.hl-todo
          epkgs.hledger-mode
          epkgs.hydra
          epkgs.magit
          epkgs.magit-todos
          epkgs.marginalia
          epkgs.markdown-mode
          epkgs.nix-mode
          epkgs.origami
          epkgs.orderless
          epkgs.org
          epkgs.org-roam
          epkgs.projectile
          epkgs.rainbow-delimiters
          epkgs.selectrum
          epkgs.selectrum-prescient
          epkgs.solaire-mode
          epkgs.use-package
          epkgs.which-key
          epkgs.yaml-mode
          epkgs.zoom
        ];
        package = pkgs.emacsMacport;
      };
      xdg.configFile."emacs/init.el".source = ./init.el;
    };
  }
