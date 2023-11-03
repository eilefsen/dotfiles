local wez = require('wezterm')
local module = {}

function module.apply(config)
    config.font = wez.font('Cascadia Code')
    config.font_size = 16.0
end

return module
