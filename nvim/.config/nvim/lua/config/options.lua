-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua

-- indentation
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = false

vim.g.editorconfig = true

-- wrap
vim.opt.wrap = false

-- tmp files
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("XDG_STATE_HOME") .. "/nvim/undodir"
vim.opt.undofile = true

-- spell
vim.opt.spelllang = "en_us,en_gb,nb,nn"

-- Diagnostics
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		vim.diagnostic.config({
			virtual_text = false,
			source = true,
		})
	end,
})

vim.api.nvim_create_user_command("H", "sp | Neotree current", {})
vim.api.nvim_create_user_command("V", "vsp | Neotree current", {})

vim.g.lazyvim_python_lsp = "basedpyright"

vim.g.rustaceanvim = {
	---@type RustaceanLspClientOpts
	server = {
		default_settings = {
			-- rust-analyzer language server configuration
			["rust-analyzer"] = {
				files = {
					excludeDirs = { ".embuild" },
				},
			},
		},
	},
}
