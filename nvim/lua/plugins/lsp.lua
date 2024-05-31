local M = {}

M.typescript = {
	{
		enabled = false,
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

M.vue = {
	{
		"neovim/nvim-lspconfig",
		opts = function(_, opts)
			local vue_typescript_plugin = require("mason-registry")
				.get_package("vue-language-server")
				:get_install_path() .. "/node_modules/@vue/language-server" .. "/node_modules/@vue/typescript-plugin"

			opts.servers = vim.tbl_deep_extend("force", opts.servers, {
				volar = {},
				-- Volar 2.0 has discontinued their "take over mode" which in previous version provided support for typescript in vue files.
				-- The new approach to get typescript support involves using the typescript language server along side volar.
				vtsls = {
					filetypes = {
						"javascript",
						"javascriptreact",
						"javascript.jsx",
						"typescript",
						"typescriptreact",
						"typescript.tsx",
						"vue",
					},
					settings = {
						vtsls = {
							tsserver = {
								globalPlugins = {
									{
										name = "@vue/typescript-plugin",
										location = vue_typescript_plugin,
										languages = { "vue" },
										configNamespace = "typescript",
									},
								},
							},
						},
					},
				},
			})
		end,
	},
}

return vim.tbl_values(M)
