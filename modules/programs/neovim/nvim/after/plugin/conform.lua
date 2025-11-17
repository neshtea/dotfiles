-- after/plugin/conform.lua
local conform = require('conform')

conform.setup({
    default_format_opts = {
        lsp_format = 'fallback',
    },
    formatters_by_ft = {
        ocaml = { "ocamlformat" },
    },
    format_on_save = { timeout_ms = 500 },
})

-- Format on save
vim.api.nvim_create_autocmd('BufWritePre', {
    pattern = "*",
    callback = function(args)
        conform.format({ bufnr = args.buf })
    end,
})
