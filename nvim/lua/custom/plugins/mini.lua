return {
    {
        'echasnovski/mini.hipatterns',
        opts = {},
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
