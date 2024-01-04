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
    {
        -- autopairs
        'windwp/nvim-autopairs',
        event = "InsertEnter",
        opts = {} -- this is equalent to setup({}) function
    },
    {
        "windwp/nvim-ts-autotag",
    },
}
