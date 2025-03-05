local M = {}

M.lazy_override = {
	{
		"folke/snacks.nvim",
		opts = {
			notifier = {
				enabled = false,
				style = "minimal",
			},
		},
	},
}

M.telescope = {
	{
		"nvim-telescope/telescope-symbols.nvim",
		dependencies = {
			"nvim-telescope/telescope.nvim",
		},
	},
	{
		"neovim/nvim-lspconfig",
		opts = function()
			local Keys = require("lazyvim.plugins.lsp.keymaps").get()
			-- stylua: ignore
			vim.list_extend(Keys, {
				-- override telescope keymaps in order to never jump to lsp refs
				{ "gd", function() require("telescope.builtin").lsp_definitions({ reuse_win = true, jump_type = "never" }) end, desc = "Goto Definition", has = "definition" },
				{ "gr", function() require("telescope.builtin").lsp_references({ reuse_win = true, jump_type = "never" }) end, desc = "References", nowait = true},
				{ "gI", function() require("telescope.builtin").lsp_implementations({ reuse_win = true, jump_type = "never" }) end, desc = "Goto Implementation" },
				{ "gy", function() require("telescope.builtin").lsp_type_definitions({ reuse_win = true, jump_type = "never" }) end, desc = "Goto T[y]pe Definition" },
			})
		end,
	},
}

M.pad = {
	{
		"rafcamlet/nvim-luapad",
		opts = {},
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
