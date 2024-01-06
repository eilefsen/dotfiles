return {
    {
        -- Error list
        'folke/trouble.nvim',
        dependencies = {
            'kyazdani42/nvim-web-devicons',
        },
        config = function()
            local trouble = require('trouble')
            vim.keymap.set('n', '<leader>tt', trouble.toggle)
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
