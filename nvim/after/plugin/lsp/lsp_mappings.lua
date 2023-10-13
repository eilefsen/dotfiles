-- Mappings
vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function()
        local bufmap = function(mode, lhs, rhs)
            local opts = {buffer = true}
            vim.keymap.set(mode, lhs, rhs, opts)
        end
        -- hover info under cursor
        bufmap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>')
        -- jump to definition
        bufmap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>')
        -- jump to declaration
        bufmap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>')
        -- jump to type definition
        bufmap('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>')
        -- list all implementations
        bufmap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>')
        -- lists all references 
        bufmap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>')
        -- displays function signature info
        bufmap('n', 'gs', '<cmd>lua vim.lsp.buf.signature_help()<cr>')
        -- renames all references to word under cursor
        bufmap('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>')
        -- selects code action available at cursor position
        bufmap('n', '<F4>', '<cmd>lua vim.lsp.buf.code_action()<cr>')
        bufmap('x', '<F4>', '<cmd>lua vim.lsp.buf.range_code_action()<cr>')
        -- formats
        bufmap('n', '<leader>f', '<cmd>lua vim.lsp.buf.format({async = true})<cr>')
        -- shows diagnostic in floating window
        bufmap('n', '<leader>e', '<cmd>lua vim.diagnostic.open_float()<cr>')
        -- go to previous diagnostic
        bufmap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>')
        -- go to next diagnostic
        bufmap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>')
    end,
})
