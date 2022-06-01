{ config, pkgs, lib, ... }:

let cfg = config.modules.desktop.xmonad;
in {

  options.modules.desktop.xmonad = { enable = lib.mkEnableOption "xmonad"; };

  config = lib.mkIf cfg.enable {
    xsession.windowManager.xmonad = {
      config = ./xmonad.hs;
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [ hp.dbus hp.monad-logger ];
    };
  };
}
