return {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    dependencies = {
        -- Automatically install LSPs to stdpath for neovim
        {
            'williamboman/mason.nvim',
            config = true,
            dependencies = {
                'nvim-lua/plenary.nvim', -- for automatic install of pylsp plugins
            },
            event = "VeryLazy",
        },
        {
            'williamboman/mason-lspconfig.nvim',
        },

        -- Useful status updates for LSP
        {
            'j-hui/fidget.nvim',
            opts = {
                notification = {
                    window = {
                        winblend = 0,
                        border = "rounded",
                    },
                },
            },
        },

        -- Additional lua configuration, makes nvim stuff amazing!
        'folke/neodev.nvim',
    },
}
