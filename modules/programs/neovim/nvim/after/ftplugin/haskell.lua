local opt = vim.opt_local
opt.shiftwidth = 2
opt.tabstop = 2
opt.softtabstop = 2
opt.formatprg = 'tee /tmp/fourmolu | fourmolu --stdin-input-file % - 2>/dev/null || cat /tmp/fourmolu'
