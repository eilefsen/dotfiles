vim.opt_local.suffixesadd:prepend('.lua')

vim.opt_local.includeexpr = [[substitute(v:fname,'\.','/','g')]]

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
