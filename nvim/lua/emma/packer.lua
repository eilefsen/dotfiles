local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
		vim.cmd [[packadd packer.nvim]]
		return true
	end
	return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
    -- packer
	use ('wbthomason/packer.nvim')
    -- essential basics
	use('nvim-treesitter/nvim-treesitter', {run = ':TSUpdate'})
	use('tpope/vim-commentary')
	use('tpope/vim-fugitive')
    use{'folke/trouble.nvim',
        requires = {'kyazdani42/nvim-web-devicons'}
    }
    -- theme
    use('git@github.com:eilefsen/nvim-base16')
    -- lsp
    use {'neovim/nvim-lspconfig'}
    use {'folke/neodev.nvim'}
        -- null-ls
    use {'jose-elias-alvarez/null-ls.nvim',
        requires = {
            {'nvim-lua/plenary.nvim'},
        }
    }
        -- mason
    use {'williamboman/mason.nvim'}
    use {'williamboman/mason-lspconfig.nvim'}
    use {'jay-babu/mason-null-ls.nvim',
        requires = {
            {'williamboman/mason.nvim'},
            {'jose-elias-alvarez/null-ls.nvim'},
        }
    }
    -- autocompletion
    use {'hrsh7th/cmp-nvim-lsp'}
    use {'hrsh7th/cmp-buffer'}
    use {'hrsh7th/cmp-path'}
    use {'hrsh7th/cmp-cmdline'}
    use {'hrsh7th/cmp-nvim-lua'}
    use {'hrsh7th/nvim-cmp'}
    -- Snippets
    use {'L3MON4D3/LuaSnip'}
    use {'saadparwaiz1/cmp_luasnip'}
    use {'rafamadriz/friendly-snippets'}

    -- nice to have
    use {'ibhagwan/smartyank.nvim'}
    use('mbbill/undotree')
    use {"windwp/nvim-autopairs",
        config = function() require("nvim-autopairs").setup {} end
    }
    use('lambdalisue/suda.vim')
    use {'nvim-lualine/lualine.nvim',
        requires = {'kyazdani42/nvim-web-devicons', opt = true }
    }
	use {'nvim-telescope/telescope.nvim', tag = '0.1.1',
		requires = {
			{'nvim-lua/plenary.nvim'},
			{'nvim-telescope/telescope-ui-select.nvim'}}
		}

    -- mini.nvim
	use {'echasnovski/mini.indentscope', branch = 'stable'}
	use {'echasnovski/mini.cursorword', branch = 'stable'}
	use {'echasnovski/mini.surround', branch = 'stable'}
	use {'echasnovski/mini.move', branch = 'stable'}

    if packer_bootstrap then
        require('packer').sync()
    end
end)
