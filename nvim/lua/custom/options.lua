-- [[ Setting options ]]
-- Global variables
vim.g.python3_host_prog = "/usr/bin/python3"
vim.g.base16_background_transparent = 1

-- Enable mouse mode
vim.o.mouse = 'a'

-- indentation
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.breakindent = true

-- various UI
vim.opt.termguicolors = true
vim.opt.guicursor = ""
vim.opt.cursorline = true
vim.opt.cursorlineopt = "screenline"
vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.cmdheight=1

-- Search
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- line numbers
vim.opt.nu = true
vim.opt.rnu = true

-- wrap
vim.opt.wrap = false

-- Decrease update time
vim.opt.updatetime = 250
vim.opt.timeoutlen = 500

-- folding
vim.opt.foldmethod = 'marker'
vim.opt.foldenable = false

-- tmp files
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("XDG_STATE_HOME") .. "/nvim/undodir"
vim.opt.undofile = true

-- Diagnostics
vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    severity_sort = true,
    underline = true,
    float = {
        border = 'rounded',
        source = 'always',
    },
})

-- command aliases
vim.api.nvim_create_user_command('E', 'Explore', {}) -- open explore mode
vim.api.nvim_create_user_command('V', 'Vexplore', {}) -- open a vertical split in explore mode

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'
