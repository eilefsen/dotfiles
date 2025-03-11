vim.treesitter.language.register('lua', { 'nvimlua' }) -- register ts parser
vim.cmd('runtime! ftplugin/lua.lua') -- extend lua ftplugin

vim.bo.keywordprg = ':help'

vim.opt_local.suffixesadd:prepend('init.lua')
vim.opt_local.path:prepend(vim.fn.stdpath('config')..'/lua')

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
