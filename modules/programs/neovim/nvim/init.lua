local opt = vim.opt
opt.nu = true
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true
opt.wrap = false
opt.swapfile = false
opt.backup = false
opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
opt.undofile = false
opt.hlsearch = true
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true
opt.termguicolors = true
opt.scrolloff = 8
opt.signcolumn = "yes"
opt.isfname:append("@-@")
opt.updatetime = 50
-- Completion
opt.completeopt = {
    'menuone',  -- always show menu even if there's only one entry
    'noselect', -- don't preselect anything
    'popup'     -- always show extra info if available
}
opt.cmdheight = 0

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
vim.keymap.set("n", "<leader>x", ":.lua<CR>")
vim.keymap.set("v", "<leader>x", ":lua<CR>")

vim.diagnostic.enable()
vim.diagnostic.config({
    virtual_lines = true,
    -- virtual_text = true,
})

-- LSP
--
vim.lsp.enable({ 'clojure', 'elixir', 'haskell', 'lua', 'nix', 'ocaml', 'typescript', })

-- PACKAGES
vim.pack.add({
    'https://github.com/sainnhe/gruvbox-material',
    { src = 'https://github.com/stevearc/conform.nvim',         version = 'v9.0.0', },
    { src = 'https://github.com/Olical/conjure',                version = 'v4.55.0', },
    { src = 'https://github.com/tpope/vim-fugitive',            version = 'v3.7', },
    { src = 'https://github.com/eraserhd/parinfer-rust',        version = 'v0.5.0', },
    { src = 'https://github.com/nvim-telescope/telescope.nvim', version = '0.1.8', },
    'https://github.com/nvim-lua/plenary.nvim',
    'https://github.com/nvim-telescope/telescope-fzf-native.nvim',
    { src = 'https://github.com/nvim-treesitter/nvim-treesitter', version = 'v0.10.0', },
    { src = 'https://github.com/mbbill/undotree',                 version = 'rel_6.1', },
    { src = 'https://github.com/folke/which-key.nvim',            tag = 'v3.17.0', },
})

-- COLORSCHEME
vim.o.background = "dark"
vim.cmd("colorscheme gruvbox-material")

local function toggle_background()
    if vim.o.background == "dark" then
        vim.o.background = "light"
    else
        vim.o.background = "dark"
    end
end

vim.keymap.set("n", "<leader>tt", toggle_background)
