vim.g.mapleader = " "  -- SPC key as map-leader.

local opt = vim.opt

opt.nu = true  -- Show line numbers
opt.relativenumber = true  -- Show relative line-numbers.

-- Set tabs to four spaces.
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true

-- Basic in-buffer search configuration.
opt.hlsearch = true
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true

opt.wrap = false

opt.guicursor = ""
opt.termguicolors = true

opt.signcolumn = "yes"

opt.completeopt = "menu,menuone,noselect"

vim.g.autoformat = 1

opt.exrc = true
