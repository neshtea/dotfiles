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
    xdg.configFile."nvim".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/modules/programs/neovim/nvim";
    home = {
      packages = with pkgs; [
        neovim
        nodejs
        lua
        stylua
        python313Packages.mdformat # Formatter for markdown
        gcc
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
