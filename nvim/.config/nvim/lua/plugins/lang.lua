local M = {}

local function handleTSDiagnostics(_, result, ctx, config)
	if result.diagnostics == nil then
		return
	end

	-- ignore some tsserver diagnostics
	local idx = 1
	while idx <= #result.diagnostics do
		local entry = result.diagnostics[idx]

		local formatter = require("format-ts-errors")[entry.code]
		entry.message = formatter and formatter(entry.message) or entry.message

		-- codes: https://github.com/microsoft/TypeScript/blob/main/src/compiler/diagnosticMessages.json
		if entry.code == 80001 then
			-- { message = "File is a CommonJS module; it may be converted to an ES module.", }
			table.remove(result.diagnostics, idx)
		else
			idx = idx + 1
		end
	end

	vim.lsp.diagnostic.on_publish_diagnostics(_, result, ctx, config)
end

M.typescript = {
	{
		"neovim/nvim-lspconfig",
		opts = {
			servers = {
				vtsls = {
					handlers = {
						["textDocument/publishDiagnostics"] = handleTSDiagnostics,
					},
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
							tsserver = {
								nodePath = "/usr/local/n/versions/node/20.15.0/bin/node",
							},
						},
					},
				},
			},
		},
	},
	{
		"davidosomething/format-ts-errors.nvim",
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

M.vue = {
	{
		"neovim/nvim-lspconfig",
		opts = {
			servers = {
				volar = {
					handlers = {
						["textDocument/publishDiagnostics"] = handleTSDiagnostics,
					},
					init_options = {
						vue = {
							hybridMode = true,
						},
					},
				},
			},
		},
	},
}

M.clangd = {
	{
		"neovim/nvim-lspconfig",
		opts = {
			servers = {
				clangd = {
					cmd = {
						-- use environment variable, so that esp-idf can override clangde with its own clangd binary
						os.getenv("CLANGD_BIN"),
						"--background-index",
						"--clang-tidy",
						"--header-insertion=iwyu",
						"--completion-style=detailed",
						"--fallback-style=llvm",
						"--query-driver=/opt/homebrew/bin/arm-none-eabi-g*", -- allow clangd to query arm specific compiler (for embedded)
						"--function-arg-placeholders=0", -- disable annoying insertion of argument names when completing functions
					},
				},
			},
		},
	},
}

M.cmake = {
	{
		"Civitasv/cmake-tools.nvim",
		lazy = true,
		init = function()
			local loaded = false
			local function check()
				local cwd = vim.uv.cwd()
				if vim.fn.filereadable(cwd .. "/CMakeLists.txt") == 1 then
					require("lazy").load({ plugins = { "cmake-tools.nvim" } })
					loaded = true
				end
			end
			check()
			vim.api.nvim_create_autocmd("DirChanged", {
				callback = function()
					if not loaded then
						check()
					end
				end,
			})
		end,
		opts = {},
	},
}

return vim.tbl_values(M)
