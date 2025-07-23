return {
    {
        "sainnhe/gruvbox-material",
        priority = 1000,
    },
    {
        "ellisonleao/gruvbox.nvim",
        priority = 1000,
        config = true
    },
    {
        "rose-pine/neovim",
        name = "rose-pine",
        config = function()
            require("rose-pine").setup({
                variant = "auto",
                dark_variant = "main",
                dim_inactive_windows = true,
            })
        end
    },
    {
        "f-person/auto-dark-mode.nvim",
        config = function()
            require("auto-dark-mode").setup({
                update_interval = 1000,
                set_dark_mode = function()
                    vim.api.nvim_set_option("background", "dark")
                    vim.cmd("colorscheme rose-pine")
                end,
                set_light_mode = function()
                    vim.api.nvim_set_option("background", "light")
                    vim.cmd("colorscheme rose-pine-dawn")
                end,
            })
        end,
        lazy = false,
    }
}
