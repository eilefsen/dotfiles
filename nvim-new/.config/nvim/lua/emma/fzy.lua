local M = {
	_config = {},
}

function M.setup(cfg)
	if vim.fn.executable('fzy') == 0 then
		vim.api.nvim_echo({
			{"[Fzy] executable 'fzy' not found on system, aborting setup..."}
		}, true, {err = true})
		return
	end
	if cfg ~= nil then
		M._config = vim.tbl_deep_extend('force', M._config, cfg)
	end
end


local function format_items(items, format_fn, prompt)
	local formatted = {}
	for idx, val in ipairs(items) do
		if format_fn then
			table.insert(formatted, tostring(idx) .. [[:]] .. format_fn(val))
		else
			table.insert(formatted, tostring(idx) .. [[:]] .. tostring(val))
		end
	end
	return formatted
end

function M.select(raw_items, opts, on_choice)
	local items = format_items(raw_items, opts.format_item, opts.prompt)

	local tmpfile = vim.fn.tempname()
	local height = 10
	vim.cmd([[botright ]] .. height .. [[ split]])
	-- NOTE: Depends on a yet unmerged feature of fzy to return the index rather than the item itself
	-- https://github.com/jhawthorn/fzy/pull/185
	-- NOTE: Also depends on a bug fix i wrote for the aforementioned pull request.
	-- It is available in this repo, branch: fields
	-- https://github.com/eilefsen/fzy
	local items_tmpfile = vim.fn.tempname()
	local fh = io.open(items_tmpfile, 'w')
	items = vim.iter(items):map(function(v)
		return v .. '\n' 
	end)
	fh:write(unpack(items:totable()))
	fh:flush()
	vim.cmd.terminal([[cat ]] .. items_tmpfile .. [[ | fzy -d: -f2 -F1 --lines ]] .. height-1 .. [[ > ]] .. tmpfile)
	fh:close()

	local winnr = vim.api.nvim_get_current_win()
	local bufnr = vim.api.nvim_get_current_buf()

	if opts.prompt then 
		vim.api.nvim_buf_set_name(bufnr, opts.prompt)
	end

	vim.api.nvim_create_autocmd({ 'TermClose' }, {
		buffer = bufnr,
		callback = function(ev)
			if vim.v.event.status == 0 or vim.v.event.status == 1 then
				vim.api.nvim_win_close(winnr, true)
				vim.api.nvim_buf_delete(bufnr, {force=true})
				if on_choice then
					local idx = tonumber(vim.fn.system('cat ' .. tmpfile))
					local val = raw_items[idx]
					on_choice(val, idx)
				end
			end
		end,
	})

	vim.api.nvim_create_autocmd({ 'BufLeave' }, {
		buffer = bufnr,
		callback = function()
			if vim.api.nvim_win_is_valid(winnr) then
				vim.api.nvim_win_close(winnr, true)
			end
			if vim.api.nvim_buf_is_valid(bufnr) then
				vim.api.nvim_buf_delete(bufnr, {force=true})
			end
			if on_choice then
				on_choice(nil, nil)
			end
		end,
	})
end

return M

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
