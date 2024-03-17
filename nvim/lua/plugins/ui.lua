return {
	{
		"folke/noice.nvim",
		opts = {
			cmdline = {
				view = "cmdline",
			},
		},
	},
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {
			options = {
				component_separators = { left = "", right = "" },
				section_separators = { left = "", right = "" },
			},
		},
	},
	{
		"echasnovski/mini.indentscope",
		opts = {
			draw = {
				animation = require("mini.indentscope").gen_animation.quadratic({
					easing = "in-out",
					duration = 50,
					unit = "total",
				}),
			},
		},
	},
}
