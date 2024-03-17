-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local map = vim.keymap.set
local unmap = vim.keymap.del

unmap("n", "<C-h>")
unmap("n", "<C-j>")
unmap("n", "<C-k>")
unmap("n", "<C-l>")
unmap({ "i", "x", "n", "s" }, "<C-s>")
unmap("n", "<leader>l")
unmap("n", "<leader>fn")
unmap("n", "<leader>qq")
unmap("n", "<leader>L")
