vim.bo.tags = vim.go.tags

if vim.g.taginclude_config ~= nil then
	for _, gp in ipairs(vim.g.taginclude_config.c.global_include_paths) do
		vim.opt_local.tags:append { gp .. '/tags'}
	end
end


-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
