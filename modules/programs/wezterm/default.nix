{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.modules.programs.wezterm;
in
{
  options.modules.programs.wezterm = {
    enable = lib.mkEnableOption "wezterm";

  };
  config = lib.mkIf cfg.enable {
    xdg.configFile."wezterm".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/modules/programs/wezterm/wezterm";

    home = {
      packages = [ pkgs.wezterm ];
    };
  };
}
