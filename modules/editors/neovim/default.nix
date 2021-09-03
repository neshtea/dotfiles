{ config, pkgs, lib, ... }:
let cfg = config.modules.editors.neovim;

in 
  {
    options.modules.editors.neovim = {
      enable = lib.mkEnableOption "neovim";
    };

    # make config only if someone set enable = true
    config = lib.mkIf cfg.enable {
      programs.neovim = {
        enable = true;
        # As seen here https://breuer.dev/blog/nixos-home-manager-neovim
        extraConfig = builtins.concatStringsSep "\n" [
          # vimscript config
          (lib.strings.fileContents ./init.vim)

          # lua config
          ''
            lua << EOF
              ${lib.strings.fileContents ./init.lua}
            EOF
          ''
        ];
        plugins = with pkgs.vimPlugins; [
          ctrlp-vim
          fzf-vim
          telescope-nvim
          neogit
          # nvim-bufferline-lua
          nvim-lspconfig
          nvim-treesitter
          onedark-vim
          vim-airline
          vim-elixir
          vim-gitgutter
          vim-nix
          which-key-nvim
        ];
        viAlias = true;
        vimAlias = true;
        vimdiffAlias = true;
      };
      # Also mount all the nvim lua stuff.
      xdg.configFile."nvim/lua".source = ./lua;
    };
  }
