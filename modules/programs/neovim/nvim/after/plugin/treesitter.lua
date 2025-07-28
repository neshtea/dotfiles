-- after/plugin/treesitter.lua
local tree_sitter = require('nvim-treesitter.configs')

tree_sitter.setup({
    ensure_installed = { "lua", "vim", "vimdoc", "query", "clojure",
        "markdown", "markdown_inline", "ocaml" },
    sync_install = false,
    auto_install = true,
    highlight = { enable = true },
})
