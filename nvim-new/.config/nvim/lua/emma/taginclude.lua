local M = {
	c = {},
	util = {},
	_config = {
		c = {
			global_include_paths = {'/usr/include'}, -- must be without trailing slash
		},
	}
}
vim.g.taginclude_config = M._config

function M.setup(config)
	local merged = vim.tbl_deep_extend('force', M._config, config)
	vim.g.taginclude_config = merged
	return merged
end

function M.c.find_tag_filenames(tagname) 
	local oldtr = vim.o.tagrelative
	vim.o.tagrelative = true
	local tl = vim.fn.taglist("^" .. tagname .. '$')
	tl = vim.tbl_map(function(val) return val.filename end, tl)
	vim.o.tagrelative = oldtr

	local gp = vim.fn.join(M._config.c.global_include_paths, [[\|]])
	gp = vim.fn.substitute(gp, '/', [[\\/]], 'g')

	local is_local = false
	if #M._config.c.global_include_paths > 0 then 
		local mls = vim.fn.matchstrlist(tl, [[^\(]] .. gp  .. [[/.*\)\@!.*$]])
		is_local = #mls > 0
		if is_local then
			tl = vim.tbl_map(function(val) return val.text end, mls)
		end
	end

	tl = vim.fn.uniq(tl)

	if is_local then
		local curdir = vim.fn.expand('%:p:h')
		local filenames = vim.tbl_filter(function (val) return vim.fn.expand('%') ~= val end , tl)
		filenames = vim.tbl_map(
			function(val) 
				local rel = vim.fn.system('realpath -s --relative-to=' .. curdir .. ' ' .. val)
				return vim.fn.trim(rel) 
			end, filenames)
		return filenames, false
	elseif #M._config.c.global_include_paths then
		local filenames = vim.fn.matchstrlist(tl, '^' .. gp .. [[/\zs.*\ze$]])
		filenames = vim.tbl_map(function(val) return val.text end, filenames)
		if #filenames > 0 then 
			return filenames, true
		end
	end

	vim.api.nvim_echo({{ "Could not find a file to include for " .. tagname }}, true, {})
	return {}, false
end

function M.c.include_tag(tagname, onlyHeaders)
	local filenames, is_global = M.c.find_tag_filenames(tagname)
	if onlyHeaders then
		local matched = vim.fn.matchstrlist(filenames, [[^.*\(\.h\|\.hpp\)$]])
		filenames = vim.tbl_map(function (val) return val.text end, matched)
	else
		filenames = vim.fn.sort(filenames, M.util.sort_headers_first)
	end

	if #filenames == 0  then
	return 
	elseif #filenames == 1 then
		M.c.append_include(filenames[1], is_global, false)
		return
	end
	vim.ui.select(filenames, {
		prompt = "Select file to include:",
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


function M.file_menu(filenames)
	return filenames[1]
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

vim.api.nvim_create_user_command('TagInclude', 
		function(opts) M.include_tag_wrapper(opts.fargs[1]) end, {nargs = 1})
vim.api.nvim_create_user_command('TagIncludeAtPoint', 
		function() M.include_tag_wrapper(vim.fn.expand('<cword>')) end, {})

return M

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
