return {
    cmd = { 'haskell-language-server-wrapper', '--lsp' },
    filetypes = {
        'haskell',
        'cabal'
    },
    root_markers = {
        'hie.yaml',
        'cabal.project',
        '*.cabal'
    },
}
