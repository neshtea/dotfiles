{ config, pkgs, lib, ... }:
let cfg = config.modules.programs.neovim;
in {
  options.modules.programs.neovim = { enable = lib.mkEnableOption "neovim"; };

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
        conjure
        fzf-vim
        telescope-nvim
        neogit
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
  };
}
