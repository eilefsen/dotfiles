-- Mappings
vim.api.nvim_create_autocmd('LspAttach', {

    desc = 'LSP actions',
    callback = function()
        local map = vim.keymap.set
        local opts = { noremap = true, silent = true, buffer = true, }
        map('n', 'K', vim.lsp.buf.hover, opts)                                        -- hover info under cursor
        map('n', 'gd', vim.lsp.buf.definition, opts)                                  -- jump to definition
        map('n', 'gD', vim.lsp.buf.declaration, opts)                                 -- jump to declaration
        map('n', 'gi', vim.lsp.buf.implementation, opts)                              -- list all implementations
        map('n', 'go', vim.lsp.buf.type_definition, opts)                             -- jump to type definition
        map('n', 'gs', vim.lsp.buf.signature_help, opts)                              -- displays function signature info
        map('n', '<F2>', vim.lsp.buf.rename, opts)                                    -- renames all references to word under cursor
        map({'n', 'x'}, '<F4>', vim.lsp.buf.code_action, opts)                        -- selects code action available at cursor position
        map('n', '[d', vim.diagnostic.goto_prev, opts)                                -- go to previous diagnostic
        map('n', ']d', vim.diagnostic.goto_next, opts)                                -- go to next diagnostic
        map('n', '<leader>e', vim.diagnostic.open_float, opts)                        -- shows diagnostic in floating window
        map('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, opts) -- formats buffer
    end,
})
