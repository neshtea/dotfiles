{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.modules.shell.zsh;
in
{
  options.modules.shell.zsh = {
    enable = lib.mkEnableOption "zsh";
  };
  config = lib.mkIf cfg.enable {
    home = {
      activation = {
        symlinkZsh = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -e $HOME/.zshrc ]; then
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/shell/zsh/zshrc $HOME/.zshrc
          fi
          if [ ! -c $HOME/.zshenv ]; then
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/shell/zsh/zshenv $HOME/.zshenv
          fi
        '';
      };
      packages = [ pkgs.starship ];
    };
  };
}
