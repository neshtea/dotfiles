local global = vim.o
local window = vim.wo
local buffer = vim.bo

vim.opt.termguicolors = true

-- https://emilienl.medium.com/a-quick-guide-to-configure-neovim-in-lua-7c58a28e624c
global.autochdir = true
global.smartcase = true
global.incsearch = true
global.hlsearch = true
global.lazyredraw = true
global.showmatch = true
global.backspace = [[eol,start,indent]]
global.ruler = true

buffer.autoindent = true
buffer.smartindent = true
buffer.expandtab = true
buffer.softtabstop = 4
buffer.shiftwidth = 4
buffer.tabstop = 4

window.cursorline = true

-- map keys
local map = vim.api.nvim_set_keymap

-- map('n', '<leader>n', ':bn', { noremap = true })
-- map('n', '<leader>p', ':bp', { noremap = true })

-- Space as leader key.
vim.g.mapleader = ' '

-- map cmd
local cmd = vim.cmd

cmd('colorscheme onedark')

-- Neogit
local neogit = require('neogit')
neogit.setup()

-- map('n', '<leader>gs', ':Neogit<cr>', { noremap = true })

-- Clojure LSP and more
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

end

-- {{{ LSP Configuration
-- Clojure LSP
require('lspconfig').clojure_lsp.setup { 
    on_attach = on_attach,
    flags = {
        debouce_text_changes = 150,
    }
}

-- Elixir LSP
require('lspconfig').elixirls.setup {
    on_attach = on_attach,
    cmd = { "/Users/schneider/.nix-profile/bin/elixir-ls" }
}

-- end LSP Configuration }}}
--
-- Find stuff with fzf
-- map('n', '<leader>/', ':Ag<cr>', { noremap = true })

-- telescope
-- Find files using Telescope command-line sugar.
-- map('n', '<leader>ff', '<cmd>Telescope find_files<cr>', { noremap = true })
-- map('n', '<leader>fg', '<cmd>Telescope live_grep<cr>', { noremap = true })
-- map('n', '<leader>fb', '<cmd>Telescope buffers<cr>', { noremap = true })
-- map('n', '<leader>fh', '<cmd>Telescope help_tags<cr>', { noremap = true })

-- nvim-which-key
require('which-key').setup { }

local wk = require('which-key')
wk.register(
   {
      f = {
	 name = "+find",
	 f = { "<cmd>Telescope find_files<cr>", "Find File" },
	 r = { "<cmd>Telescope oldfiles<cr>", "Open Recent Files" },
	 g = { "<cmd>Telescope live_grep<cr>", "Live GREP" },
	 b = { "<cmd>Telescope buffers<cr>", "Find Buffer" },
	 h = { "<cmd>Telescope help_tags<cr>", "Find Help Tags" },
      },
      g = {
	 name = "+git",
	 s = { ":NeoGit<cr>", "NeoGit Status" }
      },
      b = {
	 name = "+buffer",
	 n = { ":bn<cr>", "Next Buffer" },
	 p = { ":bp<cr>", "Previous Buffer" }
      },
   },
   { prefix = "<leader>" }
)

-- nvim-treesitter
require('nvim-treesitter.configs').setup {
	ensure_installed = { "bash", 
	"clojure", 
        -- "elixir", 
	"html", 
	"javascript", 
	"json", 
	"latex", 
	"ledger", 
	"lua", 
	"ruby", 
	-- "vim", 
	"yaml" }
}
