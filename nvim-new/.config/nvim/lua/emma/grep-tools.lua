local M = {
	qf = {},
	loc = {},
	_config = {},
}

local function git_files()
	return vim.fn.substitute(vim.fn.system('git ls-files'), '\n', ' ', 'g')
end

local function cwd_files()
	local files = {}
	if vim.fn.executable('rg') then
		files = vim.fn.system("rg --files | sed 's|^./||'")
	else
		files = vim.fn.system("find -type f | sed 's|^./||'")
	end
	return vim.fn.substitute(files, '\n', ' ', 'g')
end

function M.setup(cfg) 
	if cfg ~= nil then
		M._config = vim.tbl_deep_extend('force', M._config, cfg)
	end

	vim.api.nvim_create_user_command('Grep', function(opts)
		vim.cmd([[silent grep! ]] .. opts.args)
		vim.fn.setqflist({}, 'a', {title = 'Grep ' .. opts.args })
		vim.cmd.copen()
	end, {nargs = '+'})
	vim.api.nvim_create_user_command('LGrep', function(opts)
		vim.cmd([[silent lgrep! ]] .. opts.args)
		vim.fn.setloclist(
			vim.api.nvim_get_current_win(), 
			{},  'a', {title = 'LGrep ' .. opts.args}
		)
		vim.cmd.lopen()
	end, {nargs = '+'})

	vim.api.nvim_create_user_command('GrepCwd', function(opts)
		local files = cwd_files()
		vim.cmd([[silent grep! ]] .. opts.args .. ' ' .. files)
		vim.fn.setqflist({}, 'a', {title = 'GrepCwd ' .. opts.args})
		vim.cmd.copen()
	end, {nargs = '+'})
	vim.api.nvim_create_user_command('LGrepCwd', function(opts)
		local files = cwd_files()
		vim.cmd([[silent lgrep! ]] .. opts.args .. ' ' .. files)
		vim.fn.setloclist(
			vim.api.nvim_get_current_win(),
			{}, 'a', {title = 'LGrepCwd ' .. opts.args}
		)
		vim.cmd.lopen()
	end, {nargs = '+'})

	vim.api.nvim_create_user_command('GrepGit', function(opts)
		local files = git_files()
		vim.cmd([[silent grep! ]] .. opts.args .. ' ' .. files)
		vim.fn.setqflist({}, 'a', {title = 'GrepGit ' .. opts.args})
		vim.cmd.copen()
	end, {nargs = '+'})
	vim.api.nvim_create_user_command('LGrepGit', function(opts)
		local files = git_files()
		vim.cmd([[silent lgrep! ]] .. opts.args .. ' ' .. files)
		vim.fn.setloclist(
			vim.api.nvim_get_current_win(), 
			{}, 'a', {title = 'LGrepGit ' .. opts.args}
		)
		vim.cmd.lopen()
	end, {nargs = '+'})
end

return M
-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
