local M = {}

M.lazy_overrides = {
	{
		"folke/noice.nvim",
		opts = {
			cmdline = {
				-- move cmdline to bottom (like in regular vim)
				view = "cmdline",
			},
			routes = {
				{
					filter = {
						event = "notify",
						find = "No information available",
					},
					opts = {
						skip = true,
					},
				},
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
		"nvim-neo-tree/neo-tree.nvim",
		opts = {
			filesystem = {
				hijack_netrw_behavior = "open_current",
			},
		},
	},
	{
		"rcarriga/nvim-notify",
		enabled = false,
	},
	{
		"akinsho/bufferline.nvim",
		enabled = false,
		opts = {
			options = {
				max_name_length = 36,
				always_show_bufferline = true,
				show_close_icon = false,
				show_buffer_close_icons = false,
			},
		},
	},
}

M.treesitter_context = {
	{
		"nvim-treesitter/nvim-treesitter-context",
		opts = { mode = "cursor", max_lines = 3, separator = "—" },
		keys = {
			{
				"gC",
				function()
					require("treesitter-context").go_to_context(vim.v.count1)
				end,
				desc = "Go to scope Context",
			},
		},
	},
}

M.flatten = {
	{
		"willothy/flatten.nvim",
		-- Ensure that it runs first to minimize delay when opening file from terminal
		lazy = false,
		priority = 1001,
		opts = {},
	},
}

return vim.tbl_values(M)
