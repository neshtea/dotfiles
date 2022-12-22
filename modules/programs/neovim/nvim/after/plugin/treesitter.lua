require('nvim-treesitter.configs').setup {
    ensure_installed = { 'clojure',
                         'dockerfile',
                         'erlang',
                         'html',
                         'javascript',
                         'json',
                         'lua',
                         'nix',
                         'ocaml',
                         'ocaml_interface',
                         'ruby',
                     },
    sync_install = false,
    auto_install = false,
    ignore_install = {},
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = { 'org' }
    },
}
