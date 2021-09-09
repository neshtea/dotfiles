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
          epkgs.crux
          epkgs.cycle-themes
          epkgs.deft
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
          epkgs.lua-mode
          epkgs.magit
          epkgs.marginalia
          epkgs.markdown-mode
          epkgs.modus-themes
          epkgs.nix-mode
          epkgs.ol-notmuch
          epkgs.origami
          epkgs.orderless
          epkgs.org
          epkgs.org-bullets
          epkgs.org-present
          epkgs.org-contrib
          epkgs.org-roam
          epkgs.projectile
          epkgs.rainbow-delimiters
          epkgs.restart-emacs
          epkgs.selectrum
          epkgs.selectrum-prescient
          epkgs.solaire-mode
          epkgs.use-package
          epkgs.utimeclock
          epkgs.vterm
          epkgs.vterm-toggle
          epkgs.which-key
          epkgs.yaml-mode
          epkgs.zoom
        ];
        package = pkgs.emacsMacport;
      };

      # For emacs/vterm
      programs.fish.functions = {

        # https://github.com/akermu/emacs-libvterm#message-passing
        # TODO I can't be bothered to escape this function right now.
        # vterm_cmd.body = ''
        #   set -l vterm_elisp ()
        #   for arg in $argv
        #     set -a vterm_elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
        #   end
        #   vterm_printf '51;E'(string join '' $vterm_elisp)
        # '';

        # See https://github.com/akermu/emacs-libvterm#shell-side-configuration
        vterm_printf.body = ''
          if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
            # tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
          else if string match -q -- "screen*" "$TERM"
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$argv"
          else
            printf "\e]%s\e\\" "$argv"
          end
        '';

        # https://github.com/akermu/emacs-libvterm#vterm-buffer-name-string
        fish_title = ''
          function fish_title
            hostname
            echo ":"
            pwd
          end
        '';
      };
      xdg.configFile."emacs/init.el".source = ./init.el;
    };
  }
