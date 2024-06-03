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

return vim.tbl_values(M)
