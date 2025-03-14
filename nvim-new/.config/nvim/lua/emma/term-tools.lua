local M = {
	term_window = nil,
	term_buffer = nil,
	term_opened = false,
	_config = {
		disable_line_numbers = true, -- disables line number options in terminal window
		startinsert = true, -- starts all terminals in insert mode
	},
}

-- skip "process exited" text output in terminal when status is ok.
local function autocmd_skip_exit_msg(buf)
	if buf == nil then
		buf = vim.api.nvim_get_current_buf()
	end
	vim.api.nvim_create_autocmd({"TermClose"}, {
		buffer = buf,
		callback = function(ev) 
			if vim.v.event.status == 0 then
				vim.fn.feedkeys(" ")
			end
		end,
	})
end

function M.open_lazygit()
	vim.cmd([[tab terminal lazygit]])
	vim.wo.winfixbuf = true
	autocmd_skip_exit_msg()
end

function M.open_unique_terminal()
	if term_opened and term_window ~= nil then
		vim.api.nvim_set_current_win(term_window)
	elseif term_buffer ~= nil then
		vim.api.nvim_command("botright 10 split")
		vim.api.nvim_set_current_buf(term_buffer)
		vim.wo.winfixheight = true
		vim.wo.winfixbuf = true
		term_opened = true
		term_window = vim.api.nvim_get_current_win()
	else
		vim.api.nvim_command("botright 10 split")
		-- vim.api.nvim_command("terminal")
		vim.api.nvim_command("terminal" .. vim.fn.getenv("SHELL"))
		vim.wo.winfixbuf = true
		vim.wo.winfixheight = true
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
			end,
		})
		autocmd_skip_exit_msg(term_buffer)
	end
end

function M.setup(cfg)
	if cfg ~= nil then
		M._config = vim.tbl_deep_extend('force', M._config, cfg)
	end

	if M._config.disable_line_numbers then
		vim.api.nvim_create_autocmd({"TermOpen"}, {
			callback = function() 
				vim.wo.relativenumber = false
				vim.wo.number = false
			end,
		})
	end
	if M._config.startinsert then
		vim.api.nvim_create_autocmd({"TermOpen"}, {
			callback = function() 
				vim.cmd('startinsert')
			end,
		})
	end

	vim.api.nvim_create_user_command('TermUnique', M.open_unique_terminal, {})
	if M._config.enable_lazygit then
		vim.api.nvim_create_user_command('LazyGit', M.open_lazygit, {})
	end 
end

return M

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
