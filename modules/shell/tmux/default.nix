{ config, pkgs, lib, ... }:
let cfg = config.modules.shell.tmux;
in {
  options.modules.shell.tmux = { enable = lib.mkEnableOption "tmux"; };
  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      keyMode = "vi";
      newSession = true;
      shell = "${pkgs.zsh}/bin/zsh";
      prefix = "C-a";
    };
  };
}
