local on_attach = function(_, bufnr)

  -- run keymaps function in other file
  require('custom.configs.lsp.keymaps')(bufnr)

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

return on_attach
