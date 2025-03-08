vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

vim.opt.termguicolors = false
vim.cmd.colorscheme('noctu')

vim.opt.clipboard="unnamedplus"
vim.opt.wrap = false
vim.opt.ignorecase = true
vim.opt.splitbelow = true
vim.opt.relativenumber = true

vim.opt.backupdir = vim.fn.getenv('XDG_STATE_HOME') .. "/nvim/backup//"
vim.opt.undofile = true

-- remap space to Leader
vim.keymap.set('n', '<Space>', '<Nop>')
vim.g.mapleader = " "
-- unmap some annoying defaults
vim.keymap.set({'n', 'v'}, 'q:', '<Nop>') -- use Ctrl+F in Ex mode instead
vim.keymap.set({'n', 'x'}, 's', '<Nop>') -- disable redundant replace map

 -- remap C-c to Esc
vim.keymap.set({'n', 'v', 'i', 'o'}, '<C-c>', '<Esc>')
vim.keymap.set({'n', 'v'}, '<C-w><C-c>', '<Nop>') -- preserve default behaviour

vim.api.nvim_create_user_command('E', 'Explore', {})

if vim.fn.executable('rg') == 1 then 
	vim.opt.grepformat:append('%f:%l:%c:%m')
	vim.opt.grepprg='rg --vimgrep --smart-case --no-heading'
end

-- highlight yanked text
vim.api.nvim_create_autocmd({"TextYankPost"}, {
	callback = function(ev) 
		vim.highlight.on_yank({higroup='CurSearch', timeout=150})
	end,
})

-- Wildmenu {{{
vim.opt.wildignore= {
	'*.swp', '*.bak', '*.pyc','*.class', '*.sln','*.Master', '*.csproj',
	'*.csproj.user','*.cache','*.dll','*.pdb','*.min.*', '*/.git/**/*',
	'*/.hg/**/*', '*/.svn/**/*' , 'tags', '*.tar.*'
}
vim.opt.wildignorecase = true
vim.opt.wildoptions = {'fuzzy', 'tagfile'}

-- }}}

-- Treesitter {{{
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
				enable = true,
			},
		})
	end,
})
--}}}

-- Tags {{{
vim.api.nvim_create_autocmd({"BufReadPost"}, { 
	once = true,
	callback = function(ev) 
		local taginclude = require('emma.taginclude')
		taginclude.setup()
		vim.keymap.set({'n', 'v'}, '<Leader>ti', '<Cmd>TagInclude<CR>')
	end,
})
--}}}

-- Quickfix {{{
vim.cmd.packadd('cfilter')
local qf_tools = require('emma.quickfix-tools')
qf_tools.setup({
	on_qf_ft = function(args) 
		vim.keymap.set('n', '<leader>fz', ':Cfuzzy ', {buffer = args.buf})
		vim.keymap.set('n', '<Left>', '<Cmd>colder<CR>', {buffer = args.buf})
		vim.keymap.set('n', '<Right>', '<Cmd>cnewer<CR>', {buffer = args.buf})
		vim.keymap.set('n', '<C-o>', '<Cmd>colder<CR>', {buffer = args.buf})
		vim.keymap.set('n', '<C-i>', '<Cmd>cnewer<CR>', {buffer = args.buf})
	end,
	on_loc_ft = function(args) 
		vim.keymap.set('n', '<leader>fz', ':Lfuzzy ', {buffer = args.buf})
		vim.keymap.set('n', '<Left>', '<Cmd>lolder<CR>', {buffer = args.buf})
		vim.keymap.set('n', '<Right>', '<Cmd>lnewer<CR>', {buffer = args.buf})
		vim.keymap.set('n', '<C-o>', '<Cmd>lolder<CR>', {buffer = args.buf})
		vim.keymap.set('n', '<C-i>', '<Cmd>lnewer<CR>', {buffer = args.buf})
	end,
})
vim.keymap.set('n', '<leader>fg', '<Cmd>GitFiles<CR>')
vim.keymap.set('n', '<leader>flg', '<Cmd>LGitFiles<CR>')
--}}}

-- Terminal {{{
local term_tools = require('emma.term-tools')
local lazygit_avail = vim.fn.executable('lazygit') == 1
term_tools.setup({enable_lazygit = lazygit_avail})
if lazygit_avail then 
	vim.keymap.set({'n'}, '<Leader>gg', '<Cmd>LazyGit<CR>')
end
vim.keymap.set({'n'}, '<Leader>tu', '<Cmd>TermUnique<CR>')
--}}}




-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
