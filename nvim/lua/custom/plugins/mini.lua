return {
    {
        'echasnovski/mini.hipatterns',
        config = function()
            local hipatterns = require('mini.hipatterns')
            hipatterns.setup{
                highlighters = {
                    -- Highlight standalone 'FIXME', 'HACK', 'TODO', 'NOTE'
                    fixme = { pattern = '%f[%w]()FIXME()%f[%W]', group = 'MiniHipatternsFixme' },
                    hack  = { pattern = '%f[%w]()HACK()%f[%W]',  group = 'MiniHipatternsHack'  },
                    todo  = { pattern = '%f[%w]()TODO()%f[%W]',  group = 'MiniHipatternsTodo'  },
                    note  = { pattern = '%f[%w]()NOTE()%f[%W]',  group = 'MiniHipatternsNote'  },
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
