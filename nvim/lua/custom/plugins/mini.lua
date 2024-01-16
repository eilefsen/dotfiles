-- Start flavours
-- Felinoid scheme by Emma Eilefsen Glenna (http://eilefsen.net)
local palette = {
    base00 = "#292f37",
    base01 = "#515a65",
    base02 = "#717b87",
    base03 = "#98a0a9",
    base04 = "#c0c5cb",
    base05 = "#f6f8fa",
    base06 = "#dee0e3",
    base07 = "#f3f5f7",
    base08 = "#fb0029",
    base09 = "#fba900",
    base0A = "#fbd300",
    base0B = "#00fb55",
    base0C = "#c379e4",
    base0D = "#00a6fb",
    base0E = "#fb00a6",
    base0F = "#fb5500",
}
-- End flavours

return {
    {
        'echasnovski/mini.base16',
        version = false,
        palette = palette,
        use_cterm = true,
        plugins = {
            default = false,
            ['echasnovski/mini.nvim'] = true,
        },
    },
    {
        'echasnovski/mini.hipatterns',
        config = function()
            local hipatterns = require('mini.hipatterns')
            hipatterns.setup {
                highlighters = {
                    -- Highlight standalone 'FIXME', 'HACK', 'TODO', 'NOTE'
                    fixme     = { pattern = '%f[%w]()FIXME()%f[%W]', group = 'MiniHipatternsFixme' },
                    hack      = { pattern = '%f[%w]()HACK()%f[%W]', group = 'MiniHipatternsHack' },
                    todo      = { pattern = '%f[%w]()TODO()%f[%W]', group = 'MiniHipatternsTodo' },
                    note      = { pattern = '%f[%w]()NOTE()%f[%W]', group = 'MiniHipatternsNote' },
                    -- Highlight hex color strings (`#rrggbb`) using that color
                    hex_color = hipatterns.gen_highlighter.hex_color(),
                },
            }
        end,
    },
    {
        'echasnovski/mini.move',
        opts = {
            mappings = {
                -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
                left = 'H',
                right = 'L',
                down = 'J',
                up = 'K',

                -- Move current line in Normal mode
                line_left = '',
                line_right = '',
                line_down = '',
                line_up = '',
            },

            options = {
                reindent_linewise = true,
            },
        },
    },
}
