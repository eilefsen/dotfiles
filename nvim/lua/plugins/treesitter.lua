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
				"<leader>m",
				function()
					require("treesj").toggle()
				end,
				desc = "Treesj toggle",
			},
		},
		dependencies = { "nvim-treesitter/nvim-treesitter" },
		opts = {
			use_default_keymaps = false,
		},
	},
}

return vim.tbl_values(M)
