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
