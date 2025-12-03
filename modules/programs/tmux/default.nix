{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.programs.tmux;
in
{
  options.modules.programs.tmux = {
    enable = lib.mkEnableOption "tmux";
  };
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

        # Fixes tmux-sensible always picking /bin/sh as it's shell
        # See https://github.com/nix-community/home-manager/issues/5952#issuecomment-2409056750
        set -gu default-command
        set -g default-shell "${lib.getExe config.programs.fish.package}"
      '';
      historyLimit = 9999;
      keyMode = "vi";
      mouse = true;
      newSession = true;
      shell = "${lib.getExe config.programs.fish.package}";
      terminal = "screen-256color";
    };
  };
}
