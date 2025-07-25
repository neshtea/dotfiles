vim.opt.nu = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = false

vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.filetype.add({
    pattern = {
        [".*.mlx"] = "ocaml",
    },
})

-- require("config.lazy")

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
vim.keymap.set("n", "<leader>x", ":.lua<CR>")
vim.keymap.set("v", "<leader>x", ":lua<CR>")

vim.diagnostic.enable()
vim.diagnostic.config({
    -- virtual_lines = true,
    virtual_text = true,
})

-- LSP
--
vim.lsp.enable({
    'clojure',
    'elixir',
    'haskell',
    'lua',
    'nix',
    'typescript',
})

-- PACKAGES
--
vim.pack.add({
    'https://github.com/sainnhe/gruvbox-material',
    {
        src = 'https://github.com/stevearc/conform.nvim',
        version = 'v9.0.0',
    },
    {
        src = 'https://github.com/Olical/conjure',
        version = 'v4.55.0',
    },
    {
        src = 'https://github.com/tpope/vim-fugitive',
        version = 'v3.7',
    },
    {
        src = 'https://github.com/eraserhd/parinfer-rust',
        version = 'v0.5.0',
    },
    {
        src = 'https://github.com/nvim-telescope/telescope.nvim',
        version = '0.1.8',
    },
    'https://github.com/nvim-lua/plenary.nvim',
    'https://github.com/nvim-telescope/telescope-fzf-native.nvim',
    {
        src = 'https://github.com/nvim-treesitter/nvim-treesitter',
        version = 'v0.10.0',
    },
    {
        src = 'https://github.com/mbbill/undotree',
        version = 'rel_6.1',

    },
    {
        src = 'https://github.com/folke/which-key.nvim',
        tag = 'v3.17.0',
    },
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

-- TELESCOPE
require('telescope').setup({})
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
-- LSP stuff
vim.keymap.set('n', '<leader>fr', builtin.lsp_references, {})
vim.keymap.set('n', '<leader>fd', builtin.lsp_definitions, {})

-- UNDOTREE
vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)

-- TREESITTER
local tree_sitter = require('nvim-treesitter.configs')
tree_sitter.setup({
    ensure_installed = { "lua", "vim", "vimdoc", "query", "clojure",
        "markdown", "markdown_inline", "ocaml" },
    sync_install = false,
    auto_install = true,
    highlight = { enable = true },
})

-- CONFORM
local conform = require('conform')
conform.setup({
    default_format_opts = {
        lsp_format = 'fallback',
    },
    format_on_save = { timeout_ms = 500 },
})

vim.api.nvim_create_autocmd('BufWritePre', {
    pattern = "*",
    callback = function(args)
        conform.format({ bufnr = args.buf })
    end,
})
