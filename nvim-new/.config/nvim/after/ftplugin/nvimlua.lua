-- add parser registration
vim.treesitter.language.register('lua', { 'nvimlua' })

vim.cmd('runtime! ftplugin/lua.lua')

vim.bo.keywordprg = ':help'

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
