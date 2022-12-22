local wezterm = require('wezterm')

return {
    -- font
    font = wezterm.font 'Iosevka',
    font_size = 15,
    -- tabs
    use_fancy_tab_bar = false,
    tab_bar_at_bottom = true,
    tab_max_width = 32,
    -- theme
    color_scheme = 'Gruvbox dark, medium (base16)',  -- same as my neovim theme
    -- color_scheme = 'tokyonight',  -- same as my neovim theme
    -- scrollbar
    enable_scoll_bar = true,
}
