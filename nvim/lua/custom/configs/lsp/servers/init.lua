local servers = {
    lua_ls = require 'custom.configs.lsp.servers.lua_ls',
    html = require 'custom.configs.lsp.servers.html',
    cssls = require 'custom.configs.lsp.servers.cssls',
    stylelint_lsp = require 'custom.configs.lsp.servers.stylelint_lsp',
    jedi_language_server = require 'custom.configs.lsp.servers.jedi_language_server',
    ruff_lsp = require 'custom.configs.lsp.servers.ruff_lsp',
    clangd = require 'custom.configs.lsp.servers.clangd',
    gopls = require 'custom.configs.lsp.servers.gopls',
}

return servers
