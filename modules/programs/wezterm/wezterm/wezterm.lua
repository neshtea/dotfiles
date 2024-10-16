local wezterm = require("wezterm")
local config = {}

if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- tab bar
config.enable_tab_bar = false

config.color_scheme = 'Gruvbox dark, pale (base16)'
config.colors = colors
config.window_frame = window_frame
config.font = wezterm.font("Jetbrains Mono")
config.font_size = 14.0
config.adjust_window_size_when_changing_font_size = false

local os = wezterm.target_triple

if (os == "aarch64-apple-darwin") or (os == "x86_64-apple-darwin") then
	-- behave like a real MacOS app
	config.quit_when_all_windows_are_closed = false
end

return config
