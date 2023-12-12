require("neodev").setup({})

local lspconfig = require('lspconfig')
local lsp_defaults = lspconfig.util.default_config
local mason = require('mason')
local mason_config = require('mason-lspconfig')

mason.setup()
mason_config.setup({
    ensure_installed = {
        'lua_ls',
        'pyright',
        'clangd',
    }
})


-- Extend default capabilities
lsp_defaults.capabilities = vim.tbl_deep_extend(
    'force',
    lsp_defaults.capabilities,
    require('cmp_nvim_lsp').default_capabilities()
)
-- Automatically setup LSP servers from mason
mason_config.setup_handlers({
    function(server_name)
        lspconfig[server_name].setup({})
    end,
})

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local client = vim.lsp.get_client_by_id(args.data.client_id)
        if client.server_capabilities.inlayHintProvider then
            vim.lsp.inlay_hint(args.buf, true)
        end
    end,
})
