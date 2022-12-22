local wk = require ('which-key')

wk.setup ({
    window = {
        border = 'single',
    },
})

wk.register({
    g = {
        name = 'goto',
        d = { 'definition' },
        D = { 'declaration' },
        t = { 'type defintion' },
        i = { 'implementation' },
    },
    l = {
        name = 'lsp',
        a = { 'code actions' },
        d = { 'open float' },
        f = { 'references' },
        n = { 'goto next' },
        p = { 'goto prev' },
        r = { 'rename' },
        w = { 'workspace symbol' }
    },
    f = {
        name = 'fzf',
        b = { 'buffers' },
        f = { 'files' },
        g = { 'live grep native' },
        p = { 'git files' },
        q = { 'quickfix' },
        s = { 'grep cword' },
    },
}, { prefix = '<leader>'} )
