local M = {}

M.lazy_overrides = {
	{
		"folke/noice.nvim",
		opts = {
			cmdline = {
				-- move cmdline to bottom (like in regular vim)
				view = "cmdline",

				format = {
					cmdline = { pattern = "^:", conceal = false, icon = "", lang = "vim" },
					search_down = { kind = "search", conceal = false, pattern = "^/", icon = "", lang = "regex" },
					search_up = { kind = "search", pattern = "^%?", conceal = false, icon = "", lang = "regex" },
					filter = { pattern = "^:%s*!", conceal = false, icon = "", lang = "bash" },
					lua = {
						pattern = { "^:%s*lua%s+", "^:%s*lua%s*=%s*", "^:%s*=%s*" },
						conceal = false,
						icon = "",
						lang = "lua",
					},
					help = { pattern = "^:%s*he?l?p?%s+", conceal = false, icon = "" },
					input = { view = "cmdline_input", conceal = false, icon = "" },
				},
			},
			presets = {
				command_palette = false, -- add a border to hover docs and signature help
				bottom_search = true, -- use a classic bottom cmdline for search
			},
			lsp = {
				hover = {
					silent = true,
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
