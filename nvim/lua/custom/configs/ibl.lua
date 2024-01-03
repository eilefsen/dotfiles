vim.api.nvim_set_hl(0, 'IblIndent', { link = 'Comment', })

require('ibl').setup{
    indent = {
        highlight = {
            "IblIndent",
        },
    },
}
