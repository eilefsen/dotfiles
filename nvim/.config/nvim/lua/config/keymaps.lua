-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local map = vim.keymap.set
local unmap = vim.keymap.del

-- window controls
unmap("n", "<C-h>")
unmap("n", "<C-j>")
unmap("n", "<C-k>")
unmap("n", "<C-l>")
unmap("n", "<C-Up>")
unmap("n", "<C-Down>")
unmap("n", "<C-Left>")
unmap("n", "<C-Right>")

-- Resize window to fit buffer + <count>. It works the same as <c-w>_ if there
-- are too many lines to fit.

-- command -count WinFitBuf call s:win_fit_buf(<count>)
-- 	nnoremap <expr> <c-w>0 printf(':<c-u>%dWinFitBuf<cr>', v:count)
--

local function win_fit_buf(extra_lines)
	-- max columns on any single line
	local cols = vim.fn.max(vim.fn.map(vim.fn.range(1, vim.fn.line("$")), "col([v:val, '$'])"))
	-- resize to fit
	vim.api.nvim_win_set_width(0, cols + 8 + extra_lines)
end

map("n", "<C-w>0", function()
	win_fit_buf(vim.v.count)
end)

-- buffer controls
unmap("n", "H")
unmap("n", "L")

-- stupid save thing
unmap({ "i", "x", "n", "s" }, "<C-s>")
-- open lazy
unmap("n", "<leader>l")
-- new file
unmap("n", "<leader>fn")
-- quit all
unmap("n", "<leader>qq")
-- LazyVim changelog
unmap("n", "<leader>L")

-- float diagnostics
unmap("n", "<leader>cd")
map("n", "<leader>lf", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
