local wezterm = require('wezterm')
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- tab bar
config.enable_tab_bar = false

-- local colors = require("lua/rose-pine").colors()
-- local window_frame = require("lua/rose-pine").window_frame()

-- config.color_scheme = 'Gruvbox dark, pale (base16)'
config.color_scheme = 'Horizon Dark (Gogh)'
config.colors = colors
config.window_frame = window_frame
config.font = wezterm.font('Iosevka')
config.font_size = 15.0

local os = wezterm.target_triple

if (os == 'aarch64-apple-darwin') or (os == 'x86_64-apple-darwin') then
    -- behave like a real MacOS app
    config.quit_when_all_windows_are_closed = false
end

return config
