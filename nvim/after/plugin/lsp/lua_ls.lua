local lspconfig = require('lspconfig')

-- individually configure lua_ls
lspconfig.lua_ls.setup({
    settings = {
        Lua = {
            completion = {
                callSnippet = "Replace",
            },
            diagnostics = {
                -- Fix undefined global 'vim'
                globals = {'vim',}
            },
            workspace = {
					checkThirdParty = false,
            },
        }
    }
})
