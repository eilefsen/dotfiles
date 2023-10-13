require'nvim-treesitter.configs'.setup {
    modules = {},
    highlight = {enable = true},
    incremental_selection = {enable = true},
    textobjects = {enable = true},
    indent = {
        enable = true,
        disable = {"python"}
    },
}
