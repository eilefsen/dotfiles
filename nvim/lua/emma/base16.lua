if os.getenv('BASE16_THEME') then
    vim.cmd('colorscheme base16-' .. os.getenv('BASE16_THEME'))
else
    vim.cmd('colorscheme base16-helios')
end
