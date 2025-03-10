---@type vim.lsp.Config
return {
    cmd = { "typescript-language-server", "--stdio" },
    root_markers = { "tsconfig.json", "jsconfig.json", "package.json", ".git" },
    filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx", "vue" },
    init_options = {
		plugins = {
			{
				name = "@vue/typescript-plugin",
				location = "/usr/local/lib/node_modules/@vue/language-server",
				languages = {"vue"},
				configNamespace = "typescript",
				enableForWorkspaceTypeScriptVersions = true,
			},
		},
    },
}

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
