local M = {}

M.telescope = {
	{
		"nvim-telescope/telescope-symbols.nvim",
		dependencies = {
			"nvim-telescope/telescope.nvim",
		},
	},
}

M.flash = {
	{
		"folke/flash.nvim",
		event = "VeryLazy",
		---@type Flash.Config
		opts = {
			modes = {
				search = {
					enabled = false,
				},
			},
		},
	},
}

M.spider = {
	{
		"chrisgrieser/nvim-spider",
		lazy = true,
		keys = {
			{
				"w",
				"<cmd>lua require('spider').motion('w')<CR>",
				mode = { "n", "o", "x" },
			},
			{
				"e",
				"<cmd>lua require('spider').motion('e')<CR>",
				mode = { "n", "o", "x" },
			},
			{
				"b",
				"<cmd>lua require('spider').motion('b')<CR>",
				mode = { "n", "o", "x" },
			},
		},
	},
}

return vim.tbl_values(M)
