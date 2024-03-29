local function ensure_packer()
    local fn = vim.fn
    local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({"git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path})
        vim.cmd("packeradd packer.nvim")
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerInstall
  augroup end
]]

return require("packer").startup(function(use)
   use "wbthomason/packer.nvim"

   -- colorschemes
   use 'ellisonleao/gruvbox.nvim'
   use 'ibhagwan/fzf-lua'

   use 'nvim-lua/plenary.nvim' -- required by many other plugins

   use 'tpope/vim-fugitive'
   use 'tpope/vim-commentary'

   use { "folke/which-key.nvim" }
   -- See https://github.com/nvim-treesitter/nvim-treesitter/wiki/Installation#packernvim
   use {
       "nvim-treesitter/nvim-treesitter",
       run = function ()
           local ts_update = require('nvim-treesitter.install').update({with_sync = true })
           ts_update()
       end,
   }
   use "kylechui/nvim-surround"
   use "editorconfig/editorconfig-vim"
   use "neovim/nvim-lspconfig"
   use "hrsh7th/cmp-nvim-lsp"
   use "hrsh7th/cmp-buffer"
   use "hrsh7th/cmp-path"
   use "hrsh7th/nvim-cmp"
   use "onsails/lspkind.nvim"
   use "mrcjkb/haskell-tools.nvim"

   use "l3mon4d3/luasnip"
   use "saadparwaiz1/cmp_luasnip"

   use "kyazdani42/nvim-web-devicons"
   use 'nvim-lualine/lualine.nvim'
   use "Olical/conjure"

   -- Purescript
   use 'purescript-contrib/purescript-vim'

   if packer_bootstrap then
       require("packer").sync()
   end
end)
