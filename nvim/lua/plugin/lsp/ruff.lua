local lspconfig = require('lspconfig')

local augroup = vim.api.nvim_create_augroup("LspFormatting", {
    clear = false,
})
local on_attach = function(client, bufnr)
    -- disable hover in favor of pyright
    client.server_capabilities.hoverProvider = false
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
end

lspconfig.ruff_lsp.setup {
    on_attach = on_attach,
    init_options = {
        settings = {
            -- Any extra CLI arguments for `ruff` go here.
            args = {},
        }
    }
}
