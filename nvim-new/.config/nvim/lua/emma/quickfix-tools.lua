local M = {
	qf = {},
	loc = {},
	_config = {
		on_loc_ft = function(args) end, -- autocommand to run when opening loclist
		on_qf_ft = function(args) end, -- autocommand to run when opening qflist
		list_files_cmd = "find -type f | sed 's|^./||'"
	},
}

local function git_ls()
	local files = vim.fn.split(vim.fn.system('git ls-files'), '\n')
	return vim.tbl_map(
		function(val) 
			return {
				filename = val,
				valid = true,
			} 
		end, files)
end

local function ls()
	local files = vim.fn.split(vim.fn.system(M._config.cmd), '\n')
	return vim.tbl_map(
		function(val) 
			return {
				filename = val,
				valid = true,
			} 
		end, files)
end

local function ls_buffers()
	local bufs = vim.fn.getbufinfo({buflisted = 1})
	return vim.tbl_map(
		function(val) 
			return {
				bufnr = val.bufnr,
				lnum = val.lnum,
				valid = true,
			} 
		end, bufs)
end

local function fzy_file_filter(pat,list)
	local files = {}
	for idx, val in ipairs(list) do
		table.insert(files, val.bufnr .. '\t' .. vim.fn.bufname(val.bufnr) .. '\n')
	end

	local tmp = vim.fn.tempname()
	local fh = io.open(tmp, 'w')
	fh:write(unpack(files))
	fh:flush()
	fh:close()
	local matches = vim.fn.split(
		vim.fn.system([[cat ]] .. tmp .. [[ | fzy -d\t -F1 -f2 -e ]] .. pat), '\n')

	local new_list = vim.iter(list):filter(function(v)
		return vim.iter(matches):map(function(b)
			return tonumber(b)
		end):find(v.bufnr) ~= nil
	end)

	return new_list:totable()
end

local function fzy_text_filter(pat, list)
	local texts = {}
	for idx, val in ipairs(list) do
		if val.valid == 1 then
			table.insert(texts, val.text .. '\n')
		end
	end

	local tmp = vim.fn.tempname()
	local fh = io.open(tmp, 'w')
	fh:write(unpack(texts))
	fh:flush()
	local cmd = [[cat ]] .. tmp .. " | fzy -e " .. pat
	local matches = vim.fn.split(vim.fn.system(cmd), '\n')
	fh:close()
	print(vim.inspect(matches))

	local new_list = vim.iter(list):filter(function(v)
		return vim.iter(matches):find(v.text) ~= nil
	end)

	return new_list:totable()
end


function M.text_filter(pat, list)
	if vim.fn.executable('fzy') == 1 then
		return fzy_text_filter(pat, list)
	else
		return vim.fn.matchfuzzy(list, pat, {
			text_cb = function(v) return v.text end
		})
	end
end

function M.filename_filter(pat, list)
	if vim.fn.executable('fzy') == 1 then
		return fzy_file_filter(pat, list)
	else
		return vim.fn.matchfuzzy(list, pat, {
			text_cb = function(v) return vim.fn.bufname(v.bufnr) end
		})
	end
end

function M.qf.find_git_files()
	vim.fn.setqflist({}, ' ', {title = 'git ls-files', items = git_ls()} )
	vim.cmd.copen()
	vim.fn.feedkeys(':Cfuzzy! ')
end
function M.loc.find_git_files()
	vim.fn.setloclist(0, {}, ' ', {title = 'git ls-files', items = git_ls()} )
	vim.cmd.lopen()
	vim.fn.feedkeys(':Lfuzzy! ')
end

function M.qf.find_files()
	vim.fn.setqflist({}, ' ', {title = 'find -type f', items = ls()} )
	vim.cmd.copen()
	vim.fn.feedkeys(':Cfuzzy! ')
end
function M.loc.find_files()
	vim.fn.setloclist(0, {}, ' ', {title = 'find -type f', items = ls()} )
	vim.cmd.lopen()
	vim.fn.feedkeys(':Lfuzzy! ')
end

function M.qf.find_buffers()
	vim.fn.setqflist({}, ' ', {title = 'Listed buffers', items = ls_buffers() })
	vim.cmd.copen()
	vim.fn.feedkeys(':Cfuzzy! ')
end
function M.loc.find_buffers()
	vim.fn.setloclist(0, {}, ' ', {title = 'Listed buffers', items = ls_buffers() })
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

local function setup_commands()
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

	vim.api.nvim_create_user_command('Buffers', function(opts)
		M.qf.find_buffers() 
	end, {})
	vim.api.nvim_create_user_command('LBuffers', function(opts)
		M.loc.find_buffers() 
	end, {})

end

function M.setup(cfg) 
	if vim.fn.executable('rg') == 1 then 
		M._config.list_files_cmd = "rg --files"
	end
	if cfg ~= nil then
		M._config = vim.tbl_deep_extend('force', M._config, cfg)
	end

	setup_commands()

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
