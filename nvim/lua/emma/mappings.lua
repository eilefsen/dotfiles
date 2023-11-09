vim.g.mapleader = ' '

-- NOPE
    -- Unmap space so it only acts as leader
    vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })
    -- unmap s (substitute) to play nice with mini.surround
    vim.keymap.set({"x", "n"}, "s", '<nop>')
    vim.keymap.set({"x", "n"}, "S", '<nop>')
    -- unmap q (start recording) because it is annoying and messes with cmp
    vim.keymap.set('n', 'q', '<nop>')
    vim.keymap.set('c', '<C-f>', '<nop>')

-- center cursor
    -- while scrolling
    vim.keymap.set("n", "<C-d>", "<C-d>zz")
    vim.keymap.set("n", "<C-u>", "<C-u>zz")
    -- next/previous search
    vim.keymap.set("n", "n", "nzzzv")
    vim.keymap.set("n", "N", "Nzzzv")
    -- keep cursor in same column while joining lines
    vim.keymap.set("n", "J", "mzJ`z")

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- delete to void clipboard
vim.keymap.set({"n", "v"}, "<leader>d", "\"_d")
vim.keymap.set({"n", "v"}, "x", "\"_x")
vim.keymap.set({"n", "v"}, "<del>", "\"_x")
vim.keymap.set({"n", "v"}, "<leader>c", "\"_c")

-- TODO: probably remove these:
-- navigating splits
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-l>", "<C-w>l")
