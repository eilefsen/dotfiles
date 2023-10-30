local lspconfig = require('lspconfig')

local on_attach = function(client, _)
    -- disable hover in favor of pyright
    client.server_capabilities.hoverProvider = false
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
