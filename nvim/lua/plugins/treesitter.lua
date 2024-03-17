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
}

M.new_stuff = {
	{
		-- Expand and collapse blocks like functions etc,
		-- Useful for manually formatting arrays etc over multiple lines.
		"Wansmer/treesj",
		keys = {
			"<space>m",
			"<space>j",
			"<space>s",
		},
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
		},
		config = true,
	},
}

return vim.tbl_values(M)
