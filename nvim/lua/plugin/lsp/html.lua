local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

require('lspconfig').html.setup {
    opts = {
        settings = {
            html = {
                format = {
                    templating = true,
                },
                hover = {
                    documentation = true,
                    references = true,
                },
            },
        },
    },
    capabilities = capabilities
}
