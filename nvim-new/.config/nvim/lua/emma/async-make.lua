local M = {
	_config = {},
}

function M.make(args, use_loclist)
	local lines = {}
	local winnr = vim.fn.win_getid()
	local bufnr = vim.api.nvim_win_get_buf(winnr)

	local makeprg = vim.api.nvim_buf_get_option(bufnr, "makeprg")
	if not makeprg then return end

	local cmd = vim.fn.expandcmd(makeprg)
	if args ~= nil and args ~= '' then
		cmd = cmd .. ' ' .. args
	end
	local list_opts = {
		title = cmd,
		lines = lines,
	}
	if use_loclist then
		vim.fn.setloclist(winnr, {}, " ", list_opts)
	else
		vim.fn.setqflist({}, " ", list_opts)
	end

	local function on_event(job_id, data, event)
		if event == "stdout" or event == "stderr" then
			if data then
				vim.list_extend(lines, data)
			end
		end

		if event == "exit" then
			local list_opts = {
				title = cmd,
				lines = lines,
				efm = vim.api.nvim_buf_get_option(bufnr, "errorformat")
			}
			if use_loclist then
				vim.fn.setloclist(winnr, {}, "a", list_opts)
			else
				vim.fn.setqflist({}, "a", list_opts)
			end
			print("Make finished!")
			vim.api.nvim_command("doautocmd QuickFixCmdPost")
		end
	end

	local job_id = vim.fn.jobstart(
		cmd,
		{
			on_stderr = on_event,
			on_stdout = on_event,
			on_exit = on_event,
			stdout_buffered = true,
			stderr_buffered = true,
		}
	)
end

function M.setup(cfg)
	if cfg ~= nil then
		local merged = vim.tbl_deep_extend('force', M._config, cfg)
		M._config = M.util.process_config(merged)
	end

	vim.api.nvim_create_user_command('Make', function(opts)
		M.make(opts.args, false)
		vim.cmd.copen()
	end, {nargs = '*'})
	vim.api.nvim_create_user_command('LMake', function(opts)
		M.make(opts.args, true)
		vim.cmd.lopen()
	end, {nargs = '*'})
end

return M

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
