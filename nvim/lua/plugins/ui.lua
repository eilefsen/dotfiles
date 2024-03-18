local M = {}

M.lazy_overrides = {
	{
		"folke/noice.nvim",
		opts = {
			cmdline = {
				-- move cmdline to bottom (like in regular vim)
				view = "cmdline",
			},
		},
	},
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {
			options = {
				-- round powerline icons
				component_separators = { left = "", right = "" },
				section_separators = { left = "", right = "" },
			},
		},
	},
	{
		"echasnovski/mini.indentscope",
		opts = {
			draw = {
				-- Shorter animation than default
				animation = require("mini.indentscope").gen_animation.quadratic({
					easing = "in-out",
					duration = 50,
					unit = "total",
				}),
			},
		},
	},
	{
		"nvim-neo-tree/neo-tree.nvim",
		opts = {
			filesystem = {
				hijack_netrw_behavior = "open_split",
			},
		},
	},
}

return vim.tbl_values(M)
