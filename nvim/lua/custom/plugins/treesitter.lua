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
        "windwp/nvim-ts-autotag",
    },
    {
        'Wansmer/treesj',
        dependencies = {
            'nvim-treesitter/nvim-treesitter',
        },
        config = function()
            local treesj = require('treesj')
            treesj.setup {
                use_default_keymaps = false,
            }
            vim.keymap.set('n', '<leader>m', treesj.toggle)
        end,
    },
}
