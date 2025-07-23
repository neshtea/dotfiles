return {
    'neovim/nvim-lspconfig',
    -- tag = "v2.3.0",
    config = function()
        local lspconfig = require('lspconfig')

        lspconfig.clojure_lsp.setup({})
        lspconfig.elixirls.setup({
            cmd = { 'elixir-ls' }
        })
        lspconfig.ocamlls.setup({
            cmd = { 'ocamllsp' }
        })
        lspconfig.lua_ls.setup({})
        lspconfig.hls.setup({})
        -- https://github.com/oxalica/nil/blob/main/dev/nvim-lsp.nix
        lspconfig.nil_ls.setup({
            autostart = true,
            settings = {
                ['nil'] = {
                    formatting = {
                        command = { "nixfmt" },
                    },
                },
            },
        })
        lspconfig.ts_ls.setup({})
        lspconfig.metals.setup({})
        lspconfig.rust_analyzer.setup({})
        lspconfig.erlangls.setup({})
        lspconfig.hls.setup({
            filetypes = { 'haskell', 'lhaskell', 'cabal' },
        })

        vim.keymap.set("n", "<leader>l=", vim.lsp.buf.format)
        vim.keymap.set("n", "<leader>laa", vim.lsp.buf.code_action)
    end
}
