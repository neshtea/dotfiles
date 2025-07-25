{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.modules.shell.zsh;
in
{
  options.modules.shell.zsh = {
    enable = lib.mkEnableOption "zsh";
  };
  config = lib.mkIf cfg.enable {
    programs = {
      zsh = {
        enable = true;
        enableCompletion = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;
        shellGlobalAliases = {
          nrs = "nixos-rebuild switch --flake ~/dotfiles/ --use-remote-sudo";
          serve = "${pkgs.python3}/bin/python3 -m http.server";
          # git
          gls = "git log --oneline";
        };
        defaultKeymap = "emacs";
        # NOTE I need the `TEXINPUTS` variable to build
        # LaTeX-documents with my companies templates.  Since the repo
        # this comes from is in mercurial, I can't set it as an input
        # for my deriviation.  I'm responsible for checking out the
        # repo by hand and put it in the correct location.
        initExtra = ''
          export PATH="$HOME/bin:$PATH"
          export TEXINPUTS="$HOME/repos/ag/howto/tex:$TEXINPUTS"
          . "$HOME/.cargo/env"
        '';
        initExtraBeforeCompInit = ''
          if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
            . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
          fi
        '';
        oh-my-zsh = {
          enable = true;
          plugins = [
            "git"
            "common-aliases"
            "direnv"
            "docker"
            "docker-compose"
            "fzf"
            "gpg-agent"
            "ssh-agent"
          ];
        };
      };
    };
  };
}
