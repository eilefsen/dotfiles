vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

vim.opt.termguicolors = false
vim.cmd('colorscheme noctu')

vim.opt.clipboard="unnamedplus"
vim.opt.wrap = false
vim.opt.ignorecase = true
vim.opt.splitbelow = true
vim.opt.relativenumber = true

vim.opt.backupdir = vim.fn.getenv('XDG_STATE_HOME') .. "/nvim/backup//"
vim.opt.undofile = true

vim.g.ft_man_open_mode = 'vert'

-- remap space to Leader
vim.keymap.set('n', '<Space>', '<Nop>')
vim.g.mapleader = " "

-- unmap some annoying defaults
vim.keymap.set({'n', 'v'}, 'q:', '<Nop>') -- use Ctrl+F in Ex mode instead
vim.keymap.set({'n', 'x'}, 's', '<Nop>') -- disable redundant replace map

vim.api.nvim_create_user_command('E', 'Explore', {})

-- Wildmenu {{{
vim.opt.wildignore= {
	'*.swp', '*.bak', '*.pyc','*.class', '*.sln','*.Master', '*.csproj',
	'*.csproj.user','*.cache','*.dll','*.pdb','*.min.*', '*/.git/**/*',
	'*/.hg/**/*', '*/.svn/**/*' , 'tags', '*.tar.*'
}
vim.opt.wildignorecase = true
vim.opt.wildoptions = {'fuzzy', 'tagfile'}

-- }}}

vim.api.nvim_create_autocmd({"TextYankPost"}, {
	callback = function(ev) 
		vim.highlight.on_yank({higroup='CurSearch', timeout=150})
	end,
})

vim.api.nvim_create_autocmd({"BufReadPost"}, { 
	once = true,
	callback = function(ev) 
		vim.cmd.packadd('nvim-treesitter')
		require'nvim-treesitter.configs'.setup({
			ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "markdown", "markdown_inline" },
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = false,
			},
			indent = {
				enable = true
			}
		})
	end,
})

vim.api.nvim_create_autocmd({"BufReadPost"}, { 
	once = true,
	callback = function(ev) 
		local taginclude = require('emma.taginclude')
		taginclude.setup({})
	end,
})

require('emma.quickfix-tools')

-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
