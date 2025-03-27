local api, fn = vim.api, vim.fn

local filetypes = {
	git = 'Git',
	lazygit = 'Lazygit',
}

--- @param name string
--- @return {bg?:integer, fg?:integer}
local function get_hl(name)
	return api.nvim_get_hl(0, { name = name })
end

local buftypes = {
	help = function(file)
		return 'help:' .. fn.fnamemodify(file, ':t:r')
	end,
	quickfix = function(file) 
		return 'quickfix'
	end,
	terminal = function(file)
		local mtch = string.match(file, 'term:.*:(%a+)')
		local s = mtch or fn.fnamemodify(vim.env.SHELL, ':t')
		return 'term:' .. s
	end,
}

local function title(bufnr)
	local filetype = vim.bo[bufnr].filetype

	if filetypes[filetype] then
		return filetypes[filetype]
	end

	local file = fn.bufname(bufnr)
	local buftype = vim.bo[bufnr].buftype

	local bt = buftypes[buftype]
	if bt then
		if type(bt) == 'function' then
			return bt(file)
		end
		return bt
	end

	if file == '' then
		return '[No Name]'
	end
	return fn.pathshorten(fn.fnamemodify(file, ':p:~:t'))
end

local function flags(bufnr)
	local ret = {} --- @type string[]
	if vim.bo[bufnr].modified then
		ret[#ret + 1] = '[+]'
	end
	if not vim.bo[bufnr].modifiable then
		ret[#ret + 1] = '[RO]'
	end
	return table.concat(ret)
end

--- @param index integer
--- @param selected boolean
--- @return string
local function cell(index, selected)
	local buflist = fn.tabpagebuflist(index)
	local winnr = fn.tabpagewinnr(index)
	local bufnr = buflist[winnr]

	local bufnrs = vim.tbl_filter(function(b)
		return vim.bo[b].buftype ~= 'nofile'
	end, buflist)

	local tabtitle = ''
	if vim.api.nvim_tabpage_is_valid(index) and vim.t[index].tabpage_title ~= nil then
		tabtitle = vim.t[index].tabpage_title
	else
		tabtitle = title(bufnr)
	end

	local hl = not selected and '%#TabLine#' or '%#TabLineSel#'
	local numberhl = not selected and '%#TabLineFill#' or '%#Title#'
	local ret = string.format(
		'%s%d%%#TabLine#:%s%s%s ', numberhl, index, hl, tabtitle, flags(bufnr)
	)

	if #bufnrs > 1 then
		ret = string.format('%s%s(%d) ', ret, common, #bufnrs)
	end

	return ret .. '%T '
end

local M = {
	_cfg = {},
}

M.tabline = function()
	local parts = {} --- @type string[]
	local len = 0
	local sel_start --- @type integer

	for i = 1, fn.tabpagenr('$') do
		local selected = fn.tabpagenr() == i
		local part = cell(i, selected)
		--- @type integer
		local width = api.nvim_eval_statusline(part, { use_tabline = true }).width

		if selected then
			sel_start = len
		end

		len = len + width

		-- Make sure the start of the selected tab is always visible
		if sel_start and len > sel_start + vim.o.columns then
			break
		end

		parts[#parts + 1] = part
	end
	return table.concat(parts) .. '%#TabLineFill#%='
end

function M.setup(cfg) 
	if cfg ~= nil then
		M._cfg = vim.tbl_deep_extend('force', M._cfg, cfg)
	end

	vim.api.nvim_create_user_command('Tabtitle', function(opts)
		if opts.args == '' then 
			M.set_tab_title(nil)
		else
			M.set_tab_title(opts.args)
		end
	end, {nargs = '?'})
	function _G.EmmaTabline() 
		return M.tabline()
	end

	vim.opt.tabline = "%!v:lua.EmmaTabline()"
end

function M.set_tab_title(title, index) 
	if index == nil then
		vim.t.tabpage_title = title
	else
		vim.t[index].tabpage_title = title
	end
	vim.cmd.redrawtabline()
end

return M

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
