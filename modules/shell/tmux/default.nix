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

        # tpm initialization
        set -g @plugin 'tmux-plugins/tpm'

        # rose-pine
        set -g @plugin 'rose-pine/tmux'
        set -g @rose_pine_variant 'main'
        run '~/.config/tmux/plugins/tpm/tpm'
      '';
      historyLimit = 9999;
      keyMode = "emacs";
      mouse = true;
      newSession = true;
      plugins = with pkgs;
        [
          # tmuxPlugins.catppuccin
          # { plugin = tmuxPlugins.catppuccin; }
          # tmuxPlugins.gruvbox
          # { plugin = tmuxPlugins.gruvbox; }
        ];
      prefix = "C-Space";
      shell = "${pkgs.zsh}/bin/zsh";
      terminal = "screen-256color";
    };

    # rose-pine theme is not in nixpkgs (yet), so I'll install the tpm myself
    # and install rose-pine later (above).
    xdg.configFile."tmux/plugins/tpm" = {
      recursive = true;
      source = "${inputs.tpm-source}";
    };
  };
}
