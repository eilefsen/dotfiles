local M = {
	qf = {},
	loc = {},
	_config = {
		on_loc_ft = function(args) end, -- autocommand to run when opening loclist
		on_qf_ft = function(args) end, -- autocommand to run when opening qflist
	},
}

function M.git_ls()
	local files = vim.fn.split(vim.fn.system('git ls-files'), '\n')
	return vim.tbl_map(
		function(val) 
			return {
				filename = val,
				valid = true,
			} 
		end, files)
end

function M.ls()
	local files = vim.fn.split(vim.fn.system("find -type f | sed 's|^./||'"), '\n')
	return vim.tbl_map(
		function(val) 
			return {
				filename = val,
				valid = true,
			} 
		end, files)
end

function M.text_filter(pat, list)
	return vim.fn.matchfuzzy(list, pat, {
		text_cb = function(val) return val.text end,
		matchseq = true
	})
end

function M.filename_filter(pat, list)
	return vim.fn.matchfuzzy(list, pat, {
		text_cb = function(val) return vim.fn.bufname(val.bufnr) end,
		matchseq = true
	})
end

function M.qf.find_git_files()
	vim.fn.setqflist({}, ' ', {title = 'git ls-files', items = M.git_ls()} )
	vim.cmd.copen()
	vim.fn.feedkeys(':Cfuzzy! ')
end
function M.loc.find_git_files()
	vim.fn.setloclist(0, {}, ' ', {title = 'git ls-files', items = M.git_ls()} )
	vim.cmd.lopen()
	vim.fn.feedkeys(':Lfuzzy! ')
end

function M.qf.find_files()
	vim.fn.setqflist({}, ' ', {title = 'find -type f', items = M.ls()} )
	vim.cmd.copen()
	vim.fn.feedkeys(':Cfuzzy! ')
end
function M.loc.find_files()
	vim.fn.setloclist(0, {}, ' ', {title = 'find -type f', items = M.ls()} )
	vim.cmd.lopen()
	vim.fn.feedkeys(':Lfuzzy! ')
end

local function get_title(opts)
	local title = opts.name
	if opts.bang then
		title = title .. '!'
	end
	title = title .. [[ ]] .. opts.args
	return title
end

function M.setup(cfg) 
	if cfg ~= nil then
		M._config = vim.tbl_deep_extend('force', M._config, cfg)
	end

	vim.api.nvim_create_user_command('Cfuzzy', function(opts)
		if opts.args == '' then
			return
		end

		local list = {}
		local qfl = vim.fn.getqflist()
		if opts.bang then
			list = M.filename_filter(opts.args, qfl)
		else
			list = M.text_filter(opts.args, qfl)
		end

		vim.fn.setqflist({}, ' ', {title = get_title(opts), items = list})
	end, {nargs = '*', bang = true})

	vim.api.nvim_create_user_command('Lfuzzy', function(opts)
		if opts.args == '' then
			return
		end
		local list = {}
		local locl = vim.fn.getloclist(0)
		if opts.bang then
			list = M.filename_filter(opts.args, locl)
		else
			list = M.text_filter(opts.args, locl)
		end

		vim.fn.setloclist(0, {}, ' ', {title = get_title(opts), items = list})
	end, {nargs = '*', bang = true})

	vim.api.nvim_create_user_command('GitFiles', function(opts)
		M.qf.find_git_files() 
	end, {})
	vim.api.nvim_create_user_command('LGitFiles', function(opts)
		M.loc.find_git_files() 
	end, {})

	vim.api.nvim_create_user_command('Files', function(opts)
		M.qf.find_files() 
	end, {})
	vim.api.nvim_create_user_command('LFiles', function(opts)
		M.loc.find_files() 
	end, {})

	vim.api.nvim_create_autocmd("FileType", {
		pattern = "qf",
		callback = function(args)
			-- abstraction to make up for missing loclist filetype plugin
			if vim.fn.win_gettype(vim.fn.win_getid()) == 'loclist' then
				M._config.on_loc_ft(args)
			else
				M._config.on_qf_ft(args)
			end
		end
	})
end

return M
-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
