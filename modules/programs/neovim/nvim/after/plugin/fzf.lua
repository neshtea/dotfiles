local fzf = require('fzf-lua')

fzf.setup {
    winopts = {
        border = 'single',
        preview = {
            layout = 'vertical',
            delay = 40,
            scrollbar = false,
        },
    },
    fzf_opts = {
        ['--info'] = 'hidden',
        ['--select-1'] = false,
    },
    files = {
        prompt = 'File: ',
        previewer = false,
        fzf_opts = {
            ['--info'] = 'inline',
        },
    },
    git = {
        files = {
            prompt = 'File (git): ',
            previewer = false,
            fzf_opts = {
                ['--info'] = 'inline',
            },
        },
    },
    grep = {
        prompt = 'rg: ',
        input_prompt = 'Search string: ',
        exec_empty_query = true,
    },
    buffers = {
        prompt = 'Buffer: ',
    },
    oldfiles = {
        prompt = 'Oldfile: ',
    },
    lsp = {
        prompt_postfix = ": ",
    },
}

fzf.register_ui_select()

local nnoremap = require("neshtea.remap").nnoremap
nnoremap("<leader>fp", fzf.git_files)
nnoremap("<leader>ff", fzf.files)
nnoremap("<leader>fb", fzf.buffers)
nnoremap("<leader>fg", fzf.live_grep_native)
nnoremap("<leader>fs", fzf.grep_cword)
nnoremap("<leader>fq", fzf.quickfix)
