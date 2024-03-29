{ config, pkgs, lib, inputs, ... }:
let cfg = config.modules.programs.kitty;
in {
  options.modules.programs.kitty = { enable = lib.mkEnableOption "kitty"; };
  config = lib.mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      extraConfig = ''
        include ./themes/colors/gruvbox-material-dark-hard.conf
      '';
      font = {
        name = "JetBrains Mono";
        size = 14;
      };
      keybindings = {
        "cmd+d" = "new_window_with_cwd";
        "cmd+t" = "new_tab_with_cwd";
        "cmd+]" = "next_window";
        "cmd+[" = "previous_window";
        "cmd+1" = "goto_tab 1";
        "cmd+2" = "goto_tab 2";
        "cmd+3" = "goto_tab 3";
        "cmd+4" = "goto_tab 4";
        "cmd+5" = "goto_tab 5";
        "cmd+6" = "goto_tab 6";
        "cmd+7" = "goto_tab 7";
        "cmd+8" = "goto_tab 8";
        "cmd+9" = "goto_tab 9";
      };
      settings = { "shell_integration" = "no-cursor"; };
    };
  };
}
