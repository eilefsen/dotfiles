local M = {}

M.lazy_overrides = {
	{
		"nvim-treesitter/nvim-treesitter",
		opts = {
			ensure_installed = {
				"bash",
				"html",
				"javascript",
				"json",
				"lua",
				"markdown",
				"markdown_inline",
				"python",
				"query",
				"regex",
				"tsx",
				"typescript",
				"vim",
				"yaml",
			},
		},
	},
	{
		"windwp/nvim-ts-autotag",
		event = "LazyFile",
		opts = {},
	},
}

M.new_stuff = {
	{
		-- Expand and collapse blocks like functions etc,
		-- Useful for manually formatting arrays etc over multiple lines.
		"Wansmer/treesj",
		keys = {
			{
				"<leader>tm",
				function()
					require("treesj").toggle()
				end,
				desc = "Treesj toggle",
			},
			{
				"<leader>ts",
				function()
					require("treesj").split()
				end,
				desc = "Treesj split",
			},
			{
				"<leader>tj",
				function()
					require("treesj").join()
				end,
				desc = "Treesj join",
			},
		},
		dependencies = { "nvim-treesitter/nvim-treesitter" },
		opts = {
			max_join_length = 150,
			use_default_keymaps = false,
		},
	},
}

return vim.tbl_values(M)
