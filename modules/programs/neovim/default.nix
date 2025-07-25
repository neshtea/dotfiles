{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.modules.programs.neovim;
in
{
  options.modules.programs.neovim = {
    enable = lib.mkEnableOption "neovim";
  };

  # make config only if someone set enable = true
  config = lib.mkIf cfg.enable {
    home = {
      activation = {
        symlinkNeovim = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -e $XDG_CONFIG_HOME/nvim ]; then
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/programs/neovim/nvim $XDG_CONFIG_HOME/nvim
          fi
        '';
      };

      packages = with pkgs; [
        neovim
        nodejs
        lua
        stylua
        python313Packages.mdformat # Formatter for markdown
        cargo # for parinfer
        jq # Formatter for json

        # Fallback language servers
        clojure-lsp
        lua-language-server
        typescript-language-server
      ];
    };
  };
}
