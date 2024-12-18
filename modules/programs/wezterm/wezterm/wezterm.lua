local wezterm = require("wezterm")
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- tab bar
config.enable_tab_bar = false

config.color_scheme = 'Gruvbox dark, hard (base16)'
config.font = wezterm.font("JetBrains Mono")
config.font_size = 12.0
config.adjust_window_size_when_changing_font_size = false
config.audible_bell = "Disabled"

local os = wezterm.target_triple

if (os == "aarch64-apple-darwin") or (os == "x86_64-apple-darwin") then
    -- behave like a real MacOS app
    config.quit_when_all_windows_are_closed = false
end

return config
