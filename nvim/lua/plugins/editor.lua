local M = {}

M.telescope = {
	{
		"nvim-telescope/telescope-symbols.nvim",
		dependencies = {
			"nvim-telescope/telescope.nvim",
		},
	},
}

return vim.tbl_values(M)
