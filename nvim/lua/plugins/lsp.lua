local M = {}

M.typescript = {
	{
		-- Lua "port" of the vscode typescript extension. (NOT a lsp mapping)
		-- Replaces tsserver in lspconfig
		"pmizio/typescript-tools.nvim",
		dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
		opts = {
			settings = {
				separate_diagnostic_server = true,
				expose_as_code_action = "all",
				complete_function_calls = false,
				include_completions_with_insert_text = true,
			},
		},
	},
}

return vim.tbl_values(M)
