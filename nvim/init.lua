-- Set <space> as the leader key
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- [[ Install `lazy.nvim` plugin manager ]]
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        'git',
        'clone',
        '--filter=blob:none',
        'https://github.com/folke/lazy.nvim.git',
        '--branch=stable', -- latest stable release
        lazypath,
    }
end
vim.opt.rtp:prepend(lazypath)

-- [[ Configure plugins ]]
require('lazy').setup({
    { import = 'custom.plugins.lsp' },
    { import = 'custom.plugins.git' },
    { import = 'custom.plugins.theme' },
    { import = 'custom.plugins' },
    -- require 'kickstart.plugins.autoformat',
    -- require 'kickstart.plugins.debug',
}, {})

-- Basic lua stuff
require 'custom.colors'
require 'custom.options'
require 'custom.keymaps'
require 'custom.functions'

-- Plugin configs
require 'custom.configs.lsp'
require 'custom.configs.treesitter'
require 'custom.configs.telescope'
require 'custom.configs.which_key'
