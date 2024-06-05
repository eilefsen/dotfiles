local M = {}

M.typescript = {
	{
		"neovim/nvim-lspconfig",
		opts = {
			servers = {
				vtsls = {
					settings = {
						vtsls = {
							autoUseWorkspaceTsdk = false,
						},
						typescript = {
							inlayHints = {
								enumMemberValues = { enabled = false },
								functionLikeReturnTypes = { enabled = false },
								parameterNames = { enabled = "none" },
								parameterTypes = { enabled = false },
								propertyDeclarationTypes = { enabled = false },
								variableTypes = { enabled = false },
							},
						},
					},
				},
			},
		},
	},
}

return vim.tbl_values(M)
