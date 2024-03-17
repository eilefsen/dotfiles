return {
	{
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
				constants = "NONE",
				functions = "italic",
				operators = "NONE",
				variables = "NONE",
				parameters = "NONE",
				conditionals = "italic",
				virtual_text = "NONE",
			},
			options = {
				cursorline = true,
			},
		},
	},
}
