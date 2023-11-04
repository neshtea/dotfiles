{ config, pkgs, lib, ... }:

let cfg = config.modules.desktop.hyprland;
in {
  options.modules.desktop.hyprland = {
    enable = lib.mkEnableOption "hyprland";
  };
  config = lib.mkIf cfg.enable {
    home = {
      packages = [
        # We assume hyprland is configured in configuration.nix of the caller
        pkgs.hyprpaper
      ];
    };
    xdg.configFile."hypr/hyprland.conf".source = ./hypr/hyprland.conf;
    xdg.configFile."hypr/hyprpaper.conf".source = ./hypr/hyprpaper.conf;
  };
}
