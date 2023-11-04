local wezterm = require('wezterm')
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- tab bar
config.enable_tab_bar = false

config.color_scheme = 'Gruvbox dark, pale (base16)'
config.font = wezterm.font('Iosevka')
config.font_size = 16.0

return config
