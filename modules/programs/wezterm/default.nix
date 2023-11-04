{ config, pkgs, lib, ... }:

let cfg = config.modules.programs.wezterm;
in {
  options.modules.programs.wezterm = { enable = lib.mkEnableOption "wezterm"; };
  config = lib.mkIf cfg.enable {
    home = { packages = [ pkgs.wezterm ]; };
    xdg.configFile."wezterm/wezterm.lua".source = ./wezterm/wezterm.lua;
  };
}
