{
  config,
  pkgs,
  lib,
  # inputs,
  ...
}:

let
  cfg = config.modules.programs.ghostty;
in

{
  options.modules.programs.ghostty = {
    enable = lib.mkEnableOption "ghostty";
  };

  config = lib.mkIf cfg.enable {
    # home = {
    #   activation = {
    #     symlinkGhostty = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    #       if [ ! -e $XDG_CONFIG_HOME/ghostty ]; then
    #         $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/programs/ghostty/ghostty $XDG_CONFIG_HOME/ghostty
    #       fi
    #     '';
    #   };
    # };
    programs = {
      ghostty = {
        enable = true;
        enableFishIntegration = true;
        package = if pkgs.stdenv.isDarwin then pkgs.ghostty-bin else pkgs.ghostty;
        settings = {
          command = lib.getExe config.programs.fish.package;
          font-family = "ComicShannsMono Nerd Font";
          font-size = 12;
          term = "xterm-256color";
          theme = "Gruvbox Dark Hard";
        };
      };
    };
  };
}
