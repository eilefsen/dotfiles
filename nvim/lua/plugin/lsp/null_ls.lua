local mason_null_ls = require('mason-null-ls')
local null_ls = require('null-ls')

mason_null_ls.setup({
    ensure_installed = {
        "black",
    },
    automatic_installation = true,
    handlers = {},
})

-- configure null-ls
local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
null_ls.setup({
	on_attach = function(client, bufnr)
        -- enable formatting
		if client.supports_method("textDocument/formatting") then
			vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
			vim.api.nvim_create_autocmd("BufWritePre", {
				group = augroup,
				buffer = bufnr,
				callback = function()
					vim.lsp.buf.format()
				end,
			})
		end
	end,
    sources = {
        -- python formatter "Black"
        null_ls.builtins.formatting.black,
    }
})
