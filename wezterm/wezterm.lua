-- boilerplate
local wez = require('wezterm')
local config = {}

if wez.config_builder then
    config = wez.config_builder()
end

-- modules
local font = require('font')
font.apply(config)
local colors = require('colors')
colors.apply(config)
local mappings = require('mappings')
mappings.apply(config)

-- config starts here

config.window_background_opacity = 0.90
config.macos_window_background_blur = 20

config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false

config.window_close_confirmation = 'NeverPrompt'

config.window_frame = {
    font = config.font,
    font_size = 20.0
}
config.window_padding = {
    left = '0.5cell',
    right = '0.5cell',
    top = '0.25cell',
    bottom = '0.25cell',
}

config.inactive_pane_hsb = {
  saturation = 0.9,
  brightness = 0.6,
}



return config
