{ config, pkgs, lib, ... }:
let cfg = config.modules.editor.neovim;

in 
  {
    options.modules.editor.neovim = {
      enable = lib.mkEnableOption "neovim";
    };

    # make config only if someone set enable = true
    config = lib.mkIf cfg.enable {
      programs.neovim = {
        enable = true;
        extraConfig = ''
          " Source the actual (lua) configuration.
          luafile ${./init.lua}
        '';
        plugins = with pkgs.vimPlugins; [
          ctrlp-vim
          fzf-vim
          telescope-nvim
          neogit
          # nvim-bufferline-lua
          nvim-lspconfig
          onedark-vim
          vim-airline
          vim-gitgutter
          vim-nix
        ];
        viAlias = true;
        vimAlias = true;
        vimdiffAlias = true;
      };
      # Also mount all the nvim lua stuff.
      xdg.configFile."nvim/lua".source = ./lua;
    };
  }
