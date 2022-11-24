local telescope = require('telescope')

telescope.setup {
    pickers = {
        git_files = {
            theme = 'dropdown',
        },
        find_files = {
            theme = 'dropdown',
        },
        buffers = {
            theme = 'dropdown',
        },
        man_pages = {
            theme = 'dropdown',
        },
        live_grep = {
            theme = 'dropdown',
        },
    },
    extensions = {
        ['ui-select'] = {
            require('telescope.themes').get_cursor {}
        },
        ['fzf'] = {
            fuzzy = false,
        },
    },
}

local nnoremap = require('neshtea.remap').nnoremap

local function project_files()
    local opts = {}
    vim.fn.system('git rev-parse --is-inside-work-tree')
    if vim.v.shell_error == 0 then
        require('telescope.builtin').git_files(opts)
    else
        require('telescope.builtin').find_files(opts)
    end
end



nnoremap('<leader>fp', project_files)
nnoremap('<leader>ff', require('telescope.builtin').find_files)
nnoremap('<leader>bb', require('telescope.builtin').buffers)
nnoremap('<leader>fg', require('telescope.builtin').live_grep)
nnoremap('<leader>fs', require('telescope.builtin').grep_string)
nnoremap('<leader>fq', require('telescope.builtin').quickfix)


nnoremap('<leader>hm', require('telescope.builtin').man_pages)

telescope.load_extension('ui-select')
telescope.load_extension('fzf')
