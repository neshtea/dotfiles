{ config, pkgs, lib, ... }:
let cfg = config.modules.shell.tmux;
in {
  options.modules.shell.tmux = { enable = lib.mkEnableOption "tmux"; };
  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      extraConfig = ''
        set-option -g renumber-windows on
      '';
      historyLimit = 10000;
      keyMode = "emacs";
      mouse = true;
      newSession = true;
      plugins = with pkgs; [
        tmuxPlugins.catppuccin
        { plugin = tmuxPlugins.catppuccin; }
      ];
      prefix = "C-Space";
      shell = "${pkgs.zsh}/bin/zsh";
    };
  };
}
