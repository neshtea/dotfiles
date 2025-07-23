return {
    'stevearc/conform.nvim',
    tag = "v9.0.0",
    event = { 'BufWritePre' },
    cmd = { 'ConformInfo' },
    opts = {
        formatters_by_ft = {
            ocaml_mlx = { 'ocamlformat_mlx' },
        },
        default_format_opts = {
            lsp_format = 'fallback',
        },
        format_on_save = { timeout_ms = 500 },
    },
}
