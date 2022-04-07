{ config, pkgs, lib, ... }:

let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh = { enable = lib.mkEnableOption "zsh"; };
  config = lib.mkIf cfg.enable {
    programs = {
      zsh = {
        enable = true;
        enableCompletion = true;
        enableAutosuggestions = true;
        enableSyntaxHighlighting = true;
        oh-my-zsh = {
          enable = true;
          plugins = [
            "git"
            "sudo"
            "brew"
            "cabal"
            "common-aliases"
            "direnv"
            "docker"
            "docker-compose"
            "fzf"
            "gpg-agent"
            "ripgrep"
            "rust"
            "ssh-agent"
            "tmux"
          ];
          theme = "minimal";
        };
      };
    };
  };
}
