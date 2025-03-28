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
				languages = {"typescript", "javascript","vue"},
			},
		},
		tsserver = {
			path = "/usr/local/lib/node_modules/typescript/lib/tsserver.js"
		},
    },
	on_attach = function(client,bufnr) 
		vim.lsp.completion.enable(true, client.id, bufnr, {
			autotrigger = false,
		})
	end
}

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
