-- after/plugin/fzf-lua.lua

local fzf = require('fzf-lua')

vim.keymap.set('n', '<leader>ff', fzf.files, {})
vim.keymap.set('n', '<leader>fg', fzf.live_grep, {})
vim.keymap.set('n', '<leader>fb', fzf.buffers, {})
vim.keymap.set('n', '<leader>fh', fzf.helptags, {})
vim.keymap.set('n', '<leader>flr', fzf.lsp_references, {})
vim.keymap.set('n', '<leader>fld', fzf.lsp_definitions, {})
vim.keymap.set('n', '<leader>fle', fzf.lsp_workspace_diagnostics, {})
