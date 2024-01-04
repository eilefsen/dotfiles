return {
    {
        -- Highlight, edit, and navigate code
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            'nvim-treesitter/nvim-treesitter-textobjects',
        },
        build = ':TSUpdate',
    },
    {
        -- Insert characters around textobjects and visual selections
        "kylechui/nvim-surround",
        version = "*",
        event = "VeryLazy",
        opts = {},
        config = function(_, opts)
            require("nvim-surround").setup(opts)
        end,
    },
}
