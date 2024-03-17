local M = {}

M.typescript = {
	{
		-- Lua "port" of the vscode typescript extension. (NOT a lsp mapping)
		-- Replaces tsserver in lspconfig
		"pmizio/typescript-tools.nvim",
		dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
		opts = {},
	},
}

return vim.tbl_values(M)
