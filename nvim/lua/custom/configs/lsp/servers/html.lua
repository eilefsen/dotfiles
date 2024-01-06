local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

local html = {
    settings = {
        html = {
            format = {
                templating = true,
                wrapLineLength = 120,
                wrapAttributes = 'auto',
                indentHandlebars = true,
                indentInnerHTML = true,
            },
            hover = {
                documentation = true,
                references = true,
            },
        },
    },
    init_option = {
        configurationSection = { "html", "css", "javascript" },
        embeddedLanguages = {
            css = true,
            javascript = true
        },
        provideFormatter = true
    },
    filetypes = { "html", "htmldjango" },
    capabilities = capabilities
}

return html
