return {
    {
        -- saves all undos as a browsable tree
        'mbbill/undotree',
        config = function ()
            vim.keymap.set('n', '<leader>u', vim.cmd.UndotreeToggle)
        end
    },
    {
        -- Error list
        'folke/trouble.nvim',
        dependencies = {
            'kyazdani42/nvim-web-devicons',
        },
        config = function ()
            local trouble = require('trouble')
            vim.keymap.set("n", "<leader>tt", trouble.toggle)
            trouble.setup()
        end
    },
    {
        -- Shows valid keymaps while typing them
        'folke/which-key.nvim',
        opts = {},
    },
    {
        -- manage system clipboard
        'ibhagwan/smartyank.nvim',
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
    {
        -- allows saving file without reopening as superuser
        'lambdalisue/suda.vim',
    },
    {
        -- "gc" to comment visual regions/lines
        'numToStr/Comment.nvim',
        opts = {},
    },
    {
        -- Add indentation guides even on blank lines
        'lukas-reineke/indent-blankline.nvim',
        main = 'ibl',
        opts = {},
    },
}
