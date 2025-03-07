local M = {
	qf = {},
}

function M.qf.git_ls()
	local files = vim.fn.split(vim.fn.system('git ls-files'), '\n')
	vim.fn.setqflist(vim.tbl_map(
		function(val) 
			return {
				filename = val,
				valid = true,
			} 
		end, files))
end

function M.qf.text_filter(pat)
	return vim.fn.matchfuzzy(vim.fn.getqflist(), pat, {
		text_cb = function(val) return val.text end,
		matchseq = true
	})
end

function M.qf.filename_filter(pat)
	return vim.fn.matchfuzzy(vim.fn.getqflist(), pat, {
		text_cb = function(val) return vim.fn.bufname(val.bufnr) end,
		matchseq = true
	})
end

vim.api.nvim_create_user_command('Cfuzzy', function(opts) 
	local list = {}
	if opts.bang then
		list = M.qf.filename_filter(opts.args)
	else
		list = M.qf.text_filter(opts.args)
		if #list == 0 then 
			list = M.qf.filename_filter(opts.args)
		end
	end
	vim.fn.setqflist(list)
end, {nargs = '+', bang = true,})

function M.find_git_files()
	M.qf.git_ls()	
	vim.cmd.copen()
	vim.fn.feedkeys(':Cfuzzy! ')
end

vim.api.nvim_create_user_command('GitFiles', M.find_git_files, {})

vim.keymap.set('n', '<leader>fg', M.find_git_files)

vim.api.nvim_create_autocmd("FileType", {
	pattern = "qf",
	callback = function(args)
		vim.keymap.set('n', '<leader>fz', ':Cfuzzy ', {buffer = args.buf})
		vim.keymap.set('n', '<Left>', ':colder<CR>', {buffer = args.buf})
		vim.keymap.set('n', '<Right>', ':cnewer<CR>', {buffer = args.buf})
		vim.keymap.set('n', '<C-k>', ':colder<CR>', {buffer = args.buf})
		vim.keymap.set('n', '<C-j>', ':cnewer<CR>', {buffer = args.buf})
	end
})

return M
-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
