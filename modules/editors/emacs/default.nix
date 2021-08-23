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
          epkgs.cider
          epkgs.clojure-mode
          epkgs.clj-refactor
          epkgs.company
          epkgs.consult
          epkgs.cycle-themes
          epkgs.deft
          epkgs.diff-hl
          epkgs.dockerfile-mode
          epkgs.doom-modeline
          epkgs.doom-themes
          epkgs.evil
          epkgs.evil-collection
          epkgs.evil-nerd-commenter
          epkgs.evil-org
          epkgs.exec-path-from-shell
          epkgs.general
          epkgs.gnus-alias
          epkgs.helpful
          epkgs.hledger-mode
          epkgs.hydra
          epkgs.magit
          epkgs.marginalia
          epkgs.markdown-mode
          epkgs.modus-themes
          epkgs.nix-mode
          epkgs.notmuch
          epkgs.orderless
          epkgs.org
          epkgs.org-contrib
          epkgs.org-roam
          epkgs.projectile
          epkgs.restart-emacs
          epkgs.selectrum
          epkgs.selectrum-prescient
          epkgs.solaire-mode
          epkgs.use-package
          epkgs.utimeclock
          epkgs.which-key
          epkgs.yaml-mode
          epkgs.zoom
        ];
        package = pkgs.emacsMacport;
      };
      xdg.configFile."emacs/init.el".source = ./init.el;
    };
  }
