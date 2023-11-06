{ config, pkgs, lib, ... }:

let cfg = config.modules.programs.wezterm;
in {
  options.modules.programs.wezterm = {
    enable = lib.mkEnableOption "wezterm";
    installPackage = lib.mkOption {
      type = lib.types.bool;
      example = lib.literalExpression "true";
      description =
        "When enabled, it will install the wezterm program itself as well as it's configuration";
    };
  };
  config = lib.mkIf cfg.enable {
    home = { packages = if cfg.installPackage then [ pkgs.wezterm ] else [ ]; };
    xdg.configFile."wezterm/wezterm.lua".source = ./wezterm/wezterm.lua;
  };
}
