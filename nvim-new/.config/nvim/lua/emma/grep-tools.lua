local M = {
	qf = {},
	loc = {},
	_config = {},
}

local function git_files()
	return vim.fn.substitute(vim.fn.system('git ls-files'), '\n', ' ', 'g')
end

local function cwd_files()
	return vim.fn.substitute(
		vim.fn.system("find -type f | sed 's|^./||'"), '\n', ' ', 'g')
end

function M.setup(cfg) 
	if cfg ~= nil then
		M._config = vim.tbl_deep_extend('force', M._config, cfg)
	end

	vim.api.nvim_create_user_command('Grep', function(opts)
		vim.cmd([[silent grep! ]].. opts.args)
		vim.cmd.copen()
	end, {nargs = '+'})
	vim.api.nvim_create_user_command('LGrep', function(opts)
		vim.cmd([[silent lgrep! ]] .. opts.args)
		vim.cmd.lopen()
	end, {nargs = '+'})

	vim.api.nvim_create_user_command('GrepCwd', function(opts)
		local files = grep_cwd()
		vim.cmd([[Grep ]] .. opts.args .. ' ' .. files)
		vim.cmd.copen()
	end, {nargs = '+'})
	vim.api.nvim_create_user_command('LGrepCwd', function(opts)
		local files = grep_cwd()
		vim.cmd([[LGrep ]] .. opts.args .. ' ' .. files)
		vim.cmd.lopen()
	end, {nargs = '+'})

	vim.api.nvim_create_user_command('GrepGit', function(opts)
		local files = git_files()
		vim.cmd([[Grep ]] .. opts.args .. ' ' .. files)
		vim.cmd.copen()
	end, {nargs = '+'})
	vim.api.nvim_create_user_command('LGrepGit', function(opts)
		local files = git_files()
		vim.cmd([[LGrep ]] .. opts.args .. ' ' .. files)
		vim.cmd.lopen()
	end, {nargs = '+'})
end

return M
-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
