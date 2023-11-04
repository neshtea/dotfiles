{ config, pkgs, lib, ... }:

let cfg = config.modules.desktop.wofi;
in {
  options.modules.desktop.wofi = { enable = lib.mkEnableOption "wofi"; };
  config = lib.mkIf cfg.enable {
    home = { packages = [ pkgs.wofi ]; };
    xdg.configFile."wofi/style.css".source = ./wofi/style.css;
  };
}
