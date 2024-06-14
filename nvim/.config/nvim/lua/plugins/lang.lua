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
	{
		"dmmulroy/tsc.nvim",
		opts = {
			use_trouble_qflist = true,
			bin_path = (function()
				local node_modules_tsc_binary = vim.fn.findfile("node_modules/.bin/vue-tsc", ".;")
				if node_modules_tsc_binary ~= "" then
					return node_modules_tsc_binary
				end

				node_modules_tsc_binary = vim.fn.findfile("node_modules/.bin/tsc", ".;")
				if node_modules_tsc_binary ~= "" then
					return node_modules_tsc_binary
				end

				return "tsc"
			end)(),
		},
	},
}

return vim.tbl_values(M)
