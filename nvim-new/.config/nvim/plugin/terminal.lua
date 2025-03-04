local M = {}

M.term_window = nil
M.term_buffer = nil
M.term_opened = false

M.open_unique_terminal = function()
    if term_opened and term_window ~= nil then
	    vim.api.nvim_set_current_win(term_window)
    elseif term_buffer ~= nil then
	    vim.api.nvim_command("botright 10 split")
	    vim.api.nvim_set_current_buf(term_buffer)
	    term_opened = true
	    term_window = vim.api.nvim_get_current_win()
    else
        vim.api.nvim_command("botright 10 split")
        -- vim.api.nvim_command("terminal")
        vim.api.nvim_command("terminal" .. vim.fn.getenv("SHELL"))
		term_opened = true
        term_buffer = vim.api.nvim_get_current_buf()
        term_window = vim.api.nvim_get_current_win()
        -- vim.opt.buflisted = false

	-- reset term_opened and window, but not buffer. Executes before TermClose
	vim.api.nvim_create_autocmd({"WinClosed"}, {
		buffer = term_buffer, -- buffer local, self-destroys when buffer is closed.
		callback = function(ev) 
			term_opened = false
			term_window = nil
		end,
	})
	-- reset all state, the terminal is finished. Executes after WinClosed.
	vim.api.nvim_create_autocmd({"TermClose"}, {
		buffer = term_buffer,
		callback = function(ev) 
			term_opened = false
			term_buffer = nil
			term_window = nil
			if vim.v.event.status == 0 then
				-- skip "process exited" text output when status is ok.
				vim.cmd("q")
			end
		end,
	})
    end
    vim.cmd("startinsert")
end

vim.api.nvim_create_user_command('TermUnique', M.open_unique_terminal, {})

return M
