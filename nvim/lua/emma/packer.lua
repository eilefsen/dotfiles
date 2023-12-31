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

return require('packer').startup(function(use) -- packer
	use {'wbthomason/packer.nvim'}
    -- essential basics
	use {'nvim-treesitter/nvim-treesitter', run = {':TSUpdate'}}
	use {'tpope/vim-fugitive'}
    use {'folke/trouble.nvim',
        requires = {'kyazdani42/nvim-web-devicons'}
    }
    -- theme
    use {'eilefsen/bradcush-nvim-base16'}
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
        use {'onsails/lspkind.nvim'}
        -- Snippets
        use {
            'L3MON4D3/LuaSnip',
            run = "make install_jsregexp",
        }
        use {'saadparwaiz1/cmp_luasnip'}
        use {'rafamadriz/friendly-snippets'}

    -- nice to have
    use {'ibhagwan/smartyank.nvim'}
    use {'mbbill/undotree'}
    use {"windwp/nvim-autopairs"}
    use {"windwp/nvim-ts-autotag"}
    use {"lewis6991/gitsigns.nvim"}
    use {'lambdalisue/suda.vim'}
    use {'nvim-lualine/lualine.nvim',
        requires = {'kyazdani42/nvim-web-devicons', opt = true }
    }
	use {'nvim-telescope/telescope.nvim', tag = '0.1.1',
		requires = {
			{'nvim-lua/plenary.nvim'},
			{'nvim-telescope/telescope-ui-select.nvim'}}
		}

    -- mini.nvim
    use {'echasnovski/mini.ai'}             -- better around/in textobjects
    use {'echasnovski/mini.align'}          -- interactively align text
    use {'echasnovski/mini.bracketed'}      -- navigate with brackets
    use {'echasnovski/mini.clue'}           -- keybind clues
    use {'echasnovski/mini.comment'}        -- vim-commentary replacement
    use {'echasnovski/mini.cursorword'}     -- highlights words matching the one under your cursor
    use {'echasnovski/mini.hipatterns'}     -- highlights specified patterns
	use {'echasnovski/mini.indentscope'}    -- shows scope of indents dynamically
    use {'echasnovski/mini.map'}            -- shows a minimap of the current buffer
    use {'echasnovski/mini.move'}           -- moves selected text around
	use {'echasnovski/mini.surround'}       -- add, replace, delete, etc. brackets and such

    if packer_bootstrap then
        require('packer').sync()
    end
end)
