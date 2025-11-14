# Only contain "additional" configuration for hyprland. Hyprland itself is installed as a nixos module in /flake.nix.
{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.modules.desktop.hyprland;
in
{
  options.modules.desktop.hyprland = {
    enable = lib.mkEnableOption "hyprland";
  };
  config = lib.mkIf cfg.enable {
    home = {
      packages = [
        # We assume hyprland is configured in configuration.nix of the caller
        pkgs.hyprpaper
        pkgs.dunst
        pkgs.waybar
        pkgs.wofi
      ];
    };
  };
}
