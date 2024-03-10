{ config, pkgs, lib, inputs, ... }:
let cfg = config.modules.shell.tmux;
in {
  options.modules.shell.tmux = { enable = lib.mkEnableOption "tmux"; };
  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      extraConfig = ''
        # Make neovim :checkhealth happy
        set-option -sa terminal-features ',xterm-256color:RGB'
        set -sg escape-time 10
        set-option -g renumber-windows on
      '';
      historyLimit = 9999;
      keyMode = "emacs";
      mouse = true;
      newSession = true;
      plugins = with pkgs; [
        # tmuxPlugins.gruvbox
        # { plugin = tmuxPlugins.gruvbox; }
        # tmuxPlugins.nord
        # { plugin = tmuxPlugins.nord; }
        tmuxPlugins.onedark-theme
        { plugin = tmuxPlugins.onedark-theme; }

      ];
      prefix = "C-Space";
      shell = "${pkgs.zsh}/bin/zsh";
      terminal = "screen-256color";
    };
  };
}
