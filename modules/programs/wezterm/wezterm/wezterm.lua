local wezterm = require("wezterm")
local config = {}

-- https://wezterm.org/config/lua/wezterm.gui/get_appearance.html
local function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

local function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Catppuccin Macchiato'
  else
    return 'Catppuccin Latte'
  end
end

if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- tab bar
config.enable_tab_bar = false

config.color_scheme = scheme_for_appearance(get_appearance()) -- 'Gruvbox Material (Gogh)'
-- config.font = wezterm.font("JetBrains Mono")
config.font = wezterm.font("Menlo")
config.font_size = 12.0
config.adjust_window_size_when_changing_font_size = false
config.audible_bell = "Disabled"

local os = wezterm.target_triple

if (os == "aarch64-apple-darwin") or (os == "x86_64-apple-darwin") then
    -- behave like a real MacOS app
    config.quit_when_all_windows_are_closed = false
end

return config
