return {
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
    {
        "luukvbaal/statuscol.nvim",
        config = function()
            local builtin = require("statuscol.builtin")
            require("statuscol").setup({
                relculright = true,
                setopt = true,
                segments = {
                    {
                        sign = {
                            name = {
                                "Dap",
                                "Coc",
                                "neotest",
                            },
                            maxwidth = 1,
                            colwidth = 2,
                            auto = true,
                        },
                        click = "v:lua.ScSa",
                    },
                    {
                        text = { builtin.lnumfunc },
                        click = "v:lua.ScLa",
                    },
                    {
                        text = { " " },
                    },
                    {
                        sign = {
                            namespace = { "gitsign" },
                            maxwidth = 1,
                            colwidth = 2,
                            auto = true,
                        },
                        click = "v:lua.ScSa",
                    },
                },
            })
        end,
    }
}
