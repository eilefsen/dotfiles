vim.api.nvim_set_hl(0, 'IblIndent', { fg = Colors.base02})

require('ibl').setup{
    indent = {
        highlight = {
            "IblIndent",
        },
    },
}
