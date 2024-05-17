-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices
config.font = wezterm.font('Hack Nerd Font', {weight = 'Bold'})

config.use_fancy_tab_bar = False
config.enable_tab_bar = False

--config.window_background_opacity=0.9

config.hide_tab_bar_if_only_one_tab = true

-- wezterm.gui is not available to the mux server, so take care to
-- do something reasonable when this config is evaluated by the mux
function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Catppuccin Frappe'
  else
    return 'Catppuccin Latte'
  end
end

-- For example, changing the color scheme:
config.color_scheme = scheme_for_appearance(get_appearance())

-- and finally, return the configuration to wezterm
return config

