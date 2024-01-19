{ config, pkgs, lib, inputs, ... }:

let cfg = config.modules.programs.wezterm;
in {
  options.modules.programs.wezterm = {
    enable = lib.mkEnableOption "wezterm";

  };
  config = lib.mkIf cfg.enable {
    home = {
      activation = {
        symlinkWezterm = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -e $XDG_CONFIG_HOME/wezterm ]; then
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/programs/wezterm/wezterm $XDG_CONFIG_HOME/wezterm
          fi
        '';
      };
      packages = [ pkgs.wezterm ];
    };
  };
}
