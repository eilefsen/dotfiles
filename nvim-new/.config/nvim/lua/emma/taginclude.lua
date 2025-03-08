local M = {
	c = {},
	util = {},
	_config = {
		c = {
			global_include_paths = {'/usr/include'},
		},
	},
	cmd = {
		-- table of functions that take an `opts` argument, for user commands.
	},
}

function M.util.trim_trailing_slashes(tbl)
	return vim.tbl_map(function(val)
		return vim.fn.trim(val, '/', 2) 
	end, tbl)
end

function M.util.process_config(cfg) 
	-- Remove trailing slashes from paths
	cfg.c.global_include_paths = M.util.trim_trailing_slashes(
		cfg.c.global_include_paths
	)

	return cfg
end

function M.setup(config)
	if config ~= nil then
		local merged = vim.tbl_deep_extend('force', M._config, config)
		M._config = M.util.process_config(merged)
	end

	vim.api.nvim_create_autocmd("FileType", {
		pattern = {"c", "cpp"},
		callback = function(args)
			vim.bo.tags = vim.go.tags
			for _, gp in ipairs(M._config.c.global_include_paths) do
				vim.opt_local.tags:append({ gp .. '/tags'})
			end
		end
	})

	vim.api.nvim_create_user_command('TagInclude', 
		function(opts) 
			if opts.fargs[1] ~= nil then
				M.include_tag_wrapper(opts.fargs[1])
				return
			end

			mode = vim.fn.mode()
			if mode == 'v' or mode == 'vs' or mode == 'V' or mode == 'Vs' then
				-- yank into register then reset the register to its prior value
				local a_save = vim.fn.getreg('a')
				vim.cmd.normal([["ay]])
				M.include_tag_wrapper(vim.fn.getreg('a'))
				vim.fn.setreg('a', a_save)
				return
			end

			M.include_tag_wrapper(vim.fn.expand("<cword>"))
			return
		end, {nargs = '?', range = true})


	return M._config
end

function M.cmd.tag_include(opts)
	if opts.fargs[1] ~= nil then
		M.include_tag_wrapper(opts.fargs[1])
		return
	end

	mode = vim.fn.mode()
	if mode == 'v' or mode == 'vs' or mode == 'V' or mode == 'Vs' then
		-- yank into register then reset the register to its prior value
		local a_save = vim.fn.getreg('a')
		vim.cmd.normal([["ay]])
		M.include_tag_wrapper(vim.fn.getreg('a'))
		vim.fn.setreg('a', a_save)
		return
	end

	M.include_tag_wrapper(vim.fn.expand("<cword>"))
	return
end

function M.c.find_tag_filenames(tagname) 
	local oldtr = vim.o.tagrelative
	vim.o.tagrelative = true
	local tl = vim.fn.taglist("^" .. tagname .. '$')
	tl = vim.tbl_map(function(val) return val.filename end, tl)
	vim.o.tagrelative = oldtr

	local gp = vim.fn.join(M._config.c.global_include_paths, [[\|]])
	gp = vim.fn.substitute(gp, '/', [[\\/]], 'g')

	local local_tl = {}
	if #M._config.c.global_include_paths > 0 then 
		local_tl = vim.tbl_map(function(val) return val.text end, vim.fn.matchstrlist(tl, [[^\(]] .. gp  .. [[/.*\)\@!.*$]]))
		local_tl = vim.tbl_filter(function (val) return vim.fn.expand('%') ~= val end , local_tl)
	end

	tl = vim.fn.uniq(tl)

	if #local_tl > 0 then
		local_tl = vim.fn.uniq(tl)
		local curdir = vim.fn.expand('%:p:h')
		filenames = vim.tbl_map(
			function(val) 
				local rel = vim.fn.system('realpath -s --relative-to=' .. curdir .. ' ' .. val)
				return vim.fn.trim(rel) 
			end, filenames)
		if #filenames > 0 then 
			return filenames, false
		end
	end
	if #M._config.c.global_include_paths then
		local filenames = vim.fn.matchstrlist(tl, '^' .. gp .. [[/\zs.*\ze$]])
		filenames = vim.tbl_map(function(val) return val.text end, filenames)
		if #filenames > 0 then 
			return filenames, true
		end
	end

	return {}, false
end

function M.c.include_tag(tagname, onlyHeaders)
	local filenames, is_global = M.c.find_tag_filenames(tagname)
	if #filenames == 0 then
		vim.cmd.echom([["Could not find a file to include for: ]] .. tagname .. [["]] )
		return 
	end

	if onlyHeaders then
		local matched = vim.fn.matchstrlist(filenames, [[^.*\(\.h\|\.hpp\)$]])
		filenames = vim.tbl_map(function (val) return val.text end, matched)
	elseif #filenames > 1 then
		filenames = vim.fn.sort(filenames, M.util.sort_headers_first)
	end

	if #filenames == 1 then
		M.c.append_include(filenames[1], is_global, false)
		return
	end

	vim.ui.select(filenames, {
		prompt = "Select file to include '" .. tagname .. "' from:",
	}, function(f, idx)
			if idx == nil then
				return
			else
				M.c.append_include(f, is_global, false)
			end
		end)
end

function M.include_tag_wrapper(name)
	local n = vim.fn.shellescape(name)
	local ft = vim.bo.ft
	if ft == "c" or ft == "cpp" then
		M.c.include_tag(name, true)
	else
		vim.notify("No taginclude function available for filetype: " .. ft, vim.log.levels.INFO) 
	end
end

function M.c.append_include(filename, is_global, allow_duplicate)
	local to_append = nil
	if is_global then
		to_append = '#include <' .. filename .. '>'
	else 
		to_append = '#include "' .. filename .. '"' 
	end
	if allow_duplicate then
		vim.fn.append(0, to_append)
	end
	local search_result = vim.fn.search("^" .. to_append .. "$", 'n')
	if search_result == 0 then
		vim.fn.append(0, to_append)
	else
		vim.notify([[']] .. filename .. [[' is already included, at line: ]] .. search_result, vim.log.levels.INFO)
	end
end

local function string_is_empty(s)
	return s == " " or s == nil
end
local function string_is_not_empty(s)
	return s ~= " " or s ~= nil
end

function M.util.sort_headers_first(a, b)
	local ah = vim.fn.matchstr([[.*\(\.hpp\|h\|H\)]])
	local bc = vim.fn.matchstr([[.*\(\.cpp\|c\|C\)]])
	local ac = vim.fn.matchstr([[.*\(\.cpp\|c\|C\)]])
	local bh = vim.fn.matchstr([[.*\(\.hpp\|h\|H\)]])

	if string_is_not_empty(ah) and string_is_not_empty(bc) then
		return -1
	elseif string_is_not_empty(ac) and string_is_not_empty(bh) then
		return 1
	else
		return 0
	end
end

return M

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
