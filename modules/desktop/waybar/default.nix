{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.modules.desktop.waybar;
in
{
  options.modules.desktop.waybar = {
    enable = lib.mkEnableOption "waybar";
  };
  config = lib.mkIf cfg.enable {
    home = {
      packages = [ pkgs.waybar ];
    };
    xdg.configFile."waybar/style.css".source = ./waybar/style.css;
    xdg.configFile."waybar/config.jsonc".source = ./waybar/config.jsonc;
  };
}
