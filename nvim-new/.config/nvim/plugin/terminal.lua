local M = {
	term_window = nil,
	term_buffer = nil,
	term_opened = false,
}

function M.open_unique_terminal()
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
				vim.fn.feedkeys(" ")
			end
		end,
	})
    end
end

vim.api.nvim_create_user_command('TermUnique', M.open_unique_terminal, {})
vim.keymap.set({'n'}, '<Leader>tu', '<Cmd>TermUnique<CR>')
vim.api.nvim_create_autocmd({"TermOpen"}, {
	callback = function() 
		vim.cmd('startinsert')
		vim.wo.relativenumber = false
		vim.wo.number = false
	end,
})

if vim.fn.executable('lazygit') then
	function M.open_lazygit()
        vim.cmd.terminal("lazygit")
		vim.api.nvim_create_autocmd({"TermClose"}, {
			buffer = vim.api.nvim_get_current_buf(),
			callback = function(ev) 
				if vim.v.event.status == 0 then
					-- skip "process exited" text output when status is ok.
					vim.fn.feedkeys(" ")
				end
			end,
		})
	end

	vim.api.nvim_create_user_command('LazyGit', M.open_lazygit, {})
	vim.keymap.set({'n'}, '<Leader>gg', '<Cmd>LazyGit<CR>')
end

return M

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
