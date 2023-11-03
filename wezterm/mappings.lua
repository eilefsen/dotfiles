
local wez = require('wezterm')
local module = {}

local mappings = {
    {
        key = 'c',
        mods = 'CMD',
        action = wez.action.CopyTo('Clipboard'),
    },
    {
        key = 'v',
        mods = 'CMD',
        action = wez.action.PasteFrom('Clipboard'),
    },
    {
        key = 'c',
        mods = 'CTRL|SHIFT',
        action = wez.action.CopyTo('Clipboard'),
    },
    {
        key = 'v',
        mods = 'CTRL|SHIFT',
        action = wez.action.PasteFrom('Clipboard'),
    },
    {
        key = 'n',
        mods = 'CMD',
        action = wez.action.SpawnWindow,
    },
    {
        key = 'n',
        mods = 'CTRL|SHIFT',
        action = wez.action.SpawnWindow,
    },
    {
        key = 't',
        mods = 'CMD',
        action = wez.action.SpawnTab('CurrentPaneDomain'),
    },
    {
        key = 't',
        mods = 'CTRL|SHIFT',
        action = wez.action.SpawnTab('CurrentPaneDomain'),
    },
    {
        key = 'f',
        mods = 'CTRL|SHIFT',
        action = wez.action.ToggleFullScreen,
    },
    {
        key = 'f',
        mods = 'CMD|SHIFT',
        action = wez.action.ToggleFullScreen,
    },
    {
        key = '-',
        mods = 'CMD',
        action = wez.action.DecreaseFontSize,
    },
    {
        key = '-',
        mods = 'CTRL',
        action = wez.action.DecreaseFontSize,
    },
    {
        key = '=',
        mods = 'CMD',
        action = wez.action.IncreaseFontSize,
    },
    {
        key = '=',
        mods = 'CTRL',
        action = wez.action.IncreaseFontSize,
    },
    {
        key = '0',
        mods = 'CMD',
        action = wez.action.ResetFontSize,
    },
    {
        key = '0',
        mods = 'CTRL',
        action = wez.action.ResetFontSize,
    },
    {
        key = 'w',
        mods = 'CMD',
        action = wez.action.CloseCurrentTab{confirm=true},
    },
    {
        key = 'w',
        mods = 'CTRL|SHIFT',
        action = wez.action.CloseCurrentTab{confirm=true},
    },
    {
        key = '1',
        mods = 'CMD',
        action = wez.action.ActivateTab(0),
    },
    {
        key = '2',
        mods = 'CMD',
        action = wez.action.ActivateTab(1),
    },
    {
        key = '3',
        mods = 'CMD',
        action = wez.action.ActivateTab(2),
    },
    {
        key = '4',
        mods = 'CMD',
        action = wez.action.ActivateTab(3),
    },
    {
        key = '5',
        mods = 'CMD',
        action = wez.action.ActivateTab(4),
    },
    {
        key = '6',
        mods = 'CMD',
        action = wez.action.ActivateTab(5),
    },
    {
        key = '7',
        mods = 'CMD',
        action = wez.action.ActivateTab(6),
    },
    {
        key = '8',
        mods = 'CMD',
        action = wez.action.ActivateTab(7),
    },
    {
        key = '9',
        mods = 'CMD',
        action = wez.action.ActivateTab(8),
    },
    {
        key = '1',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateTab(0),
    },
    {
        key = '2',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateTab(1),
    },
    {
        key = '3',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateTab(2),
    },
    {
        key = '4',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateTab(3),
    },
    {
        key = '5',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateTab(4),
    },
    {
        key = '6',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateTab(5),
    },
    {
        key = '7',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateTab(6),
    },
    {
        key = '8',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateTab(7),
    },
    {
        key = '9',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateTab(8),
    },
    {
        key = 'r',
        mods = 'CMD',
        action = wez.action.ReloadConfiguration,
    },
    {
        key = 'r',
        mods = 'CTRL|SHIFT',
        action = wez.action.ReloadConfiguration,
    },
    {
        key = 'h',
        mods = 'CMD',
        action = wez.action.HideApplication,
    },
    {
        key = 'PageUp',
        mods = 'SHIFT',
        action = wez.action.ScrollByPage(-1),
    },
    {
        key = 'PageDown',
        mods = 'SHIFT',
        action = wez.action.ScrollByPage(1),
    },
    {
        key = 'P',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateCommandPalette,
    },
    {
        key = 'U',
        mods = 'CTRL|SHIFT',
        action = wez.action.CharSelect,
    },
    {
        key = 'x',
        mods = 'CTRL|SHIFT',
        action = wez.action.ActivateCopyMode,
    },
    {
        key = 'Space',
        mods = 'CTRL|SHIFT',
        action = wez.action.QuickSelect,
    },
    {
        key = 'Z',
        mods = 'CTRL|SHIFT',
        action = wez.action.TogglePaneZoomState,
    },
}

function module.apply(config)
    config.disable_default_key_bindings = true
    config.keys = mappings
end

return module
