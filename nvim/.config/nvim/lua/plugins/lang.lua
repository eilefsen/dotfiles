local M = {}

M.typescript = {
	{
		"neovim/nvim-lspconfig",
		opts = {
			servers = {
				tsserver = {
					enabled = false,
				},
				vtsls = {
					settings = {
						vtsls = {
							autoUseWorkspaceTsdk = false,
						},
					},
				},
			},
		},
	},
}

return vim.tbl_values(M)
