local servers = {
    lua_ls = require 'custom.configs.lsp.servers.lua_ls',
    html = require 'custom.configs.lsp.servers.html',
    cssls = require 'custom.configs.lsp.servers.cssls',
    stylelint_lsp = require 'custom.configs.lsp.servers.stylelint_lsp',
    pylsp = require 'custom.configs.lsp.servers.pylsp',
    clangd = require 'custom.configs.lsp.servers.clangd',
    gopls = require 'custom.configs.lsp.servers.gopls',
}

return servers
