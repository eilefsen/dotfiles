local servers = {
    lua_ls = require 'custom.configs.lsp.servers.lua_ls',
    html = require 'custom.configs.lsp.servers.html',
    jedi_language_server = require 'custom.configs.lsp.servers.jedi',
    ruff_lsp = require 'custom.configs.lsp.servers.ruff',
    jedi_language_server = require 'custom.configs.lsp.servers.jedi_language_server',
    ruff_lsp = require 'custom.configs.lsp.servers.ruff_lsp',
    clangd = require 'custom.configs.lsp.servers.clangd',
}

return servers
