local M = {}

M.theme = {
	"olimorris/onedarkpro.nvim",
	priority = 1000, -- Ensure it loads first
	opts = {
		styles = {
			types = "NONE",
			methods = "NONE",
			numbers = "NONE",
			strings = "NONE",
			comments = "italic",
			keywords = "bold,italic",
			constants = "bold",
			functions = "italic",
			operators = "NONE",
			variables = "NONE",
			parameters = "italic",
			conditionals = "italic",
			virtual_text = "NONE",
		},
		options = {
			cursorline = true,
		},
	},
}

return vim.tbl_values(M)
