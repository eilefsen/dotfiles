-- mason-lspconfig requires that these setup functions are called in this order
-- before setting up the servers.
require('mason').setup()
require('mason-lspconfig').setup()
require('neodev').setup()

local on_attach = require 'custom.configs.lsp.on_attach'

-- Make default capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

local servers = require 'custom.configs.lsp.servers'

local mason_lspconfig = require 'mason-lspconfig'
mason_lspconfig.setup {
    ensure_installed = vim.tbl_keys(servers),
}



mason_lspconfig.setup_handlers {
    function(server_name)
        local tbl = {
            capabilities = capabilities,
            on_attach = on_attach,
            filetypes = (servers[server_name] or {}).filetypes,
        }
        tbl = vim.tbl_extend("keep", tbl, servers[server_name])
        require('lspconfig')[server_name].setup(tbl)
    end,
}

require 'custom.configs.lsp.cmp'
