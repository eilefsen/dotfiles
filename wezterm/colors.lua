local wez = require('wezterm')
local module = {}


function module.apply(config)
    local colors, _ = wez.color.load_scheme(wez.config_dir .. '/base16.toml')
    config.colors = colors
end

return module
