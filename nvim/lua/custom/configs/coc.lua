vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.signcolumn = "yes"

local keyset = vim.keymap.set

-- Helper Function
keyset = function(mode, lhs, rhs, opts)
    opts = opts or {silent = true}
    vim.keymap.set(mode,lhs, rhs, opts)
end
-- Autocomplete
function _G.check_back_space()
    local col = vim.fn.col('.') - 1
    return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end



-- Diagnostic Keymaps
keyset("n", "<leader>e", "<plug>(coc-diagnostic-info)")
keyset("n", "[g", "<plug>(coc-diagnostic-next)")
keyset("n", "]g", "<plug>(coc-diagnostic-prev)")

-- Language movement
keyset("n", "gi", "<plug>(coc-implementation)")
keyset("n", "gd", "<plug>(coc-definition)")
keyset("n", "gD", "<plug>(coc-declaration)")
keyset("n", "gr", "<plug>(coc-referances)")
keyset("n", "gR", "<plug>(coc-referances-used)")

-- Refactor and Format
keyset("n", "<leader>rn", "<plug>(coc-rename)")
keyset("n", "<leader>fm", "<plug>(coc-format)")
keyset("x", "<leader>fm", "<plug>(coc-format-selected)")
keyset("n", "<leader>rf", "<plug>(coc-refactor)", opts)
keyset("n", "<leader>rF", "<plug>(coc-codeaction-refactor)")
-- Code Actions
local opts = {silent = true, nowait = true}
keyset("x", "<leader>a", "<Plug>(coc-codeaction-selected)", opts)
keyset("n", "<leader>a", "<Plug>(coc-codeaction-selected)", opts)
keyset("n", "<leader>ac", "<Plug>(coc-codeaction-cursor)", opts)
keyset("n", "<leader>as", "<Plug>(coc-codeaction-source)", opts)
keyset("n", "<leader>qf", "<Plug>(coc-fix-current)", opts)
keyset("n", "<leader>cl", "<Plug>(coc-codelens-action)", opts)

local opts = {silent = true, nowait = true, expr = true}
keyset("n", "<C-f>", 'coc#float#has_scroll() ? coc#float#scroll(1) : "<C-f>"', opts)
keyset("n", "<C-b>", 'coc#float#has_scroll() ? coc#float#scroll(0) : "<C-b>"', opts)
keyset("i", "<C-f>",
       'coc#float#has_scroll() ? "<c-r>=coc#float#scroll(1)<cr>" : "<Right>"', opts)
keyset("i", "<C-b>",
       'coc#float#has_scroll() ? "<c-r>=coc#float#scroll(0)<cr>" : "<Left>"', opts)
keyset("v", "<C-f>", 'coc#float#has_scroll() ? coc#float#scroll(1) : "<C-f>"', opts)
keyset("v", "<C-b>", 'coc#float#has_scroll() ? coc#float#scroll(0) : "<C-b>"', opts)

-- Use CTRL-S for selections ranges
-- Requires 'textDocument/selectionRange' support of language server
keyset("n", "<C-s>", "<Plug>(coc-range-select)")
keyset("x", "<C-s>", "<Plug>(coc-range-select)")

-- Tab confirm
function _G.confirm()
    if vim.api.nvim_eval("coc#pum#visible()") then
        vim.cmd("call coc#pum#confirm()")
    elseif vim.cmd([[call coc#expandableOrJumpable()]]) then
        return "<Plug>(coc-snippets-expand-jump)"
    end
end

-- Completions Keymaps
local opts = {silent = true, noremap = true, expr = true, replace_keycodes = false}
keyset("i", "<cr>", [[coc#pum#visible() ? coc#pum#confirm() : "\<CR>"]], opts)
keyset("i", "<Tab>", _G.confirm, opts)
keyset("i", "<C-n>", [[coc#pum#visible() ? coc#pum#next(1) : v:lua.check_back_space() ? "<C-n>" : coc#refresh()]], opts)
keyset("i", "<C-p>", [[coc#pum#visible() ? coc#pum#prev(1) : "\<C-p>"]], opts)

-- Use K to show documentation in preview window
function _G.show_docs()
    local cw = vim.fn.expand('<cword>')
    if vim.fn.index({'vim', 'help'}, vim.bo.filetype) >= 0 then
        vim.api.nvim_command('h ' .. cw)
    elseif vim.api.nvim_eval('coc#rpc#ready()') then
        vim.fn.CocActionAsync('doHover')
    else
        vim.api.nvim_command('!' .. vim.o.keywordprg .. ' ' .. cw)
    end
end

keyset("n", "K", _G.show_docs)

-- Add `:Format` command to format current buffer
vim.api.nvim_create_user_command("Format", "call CocAction('format')", {})

-- " Add `:Fold` command to fold current buffer
vim.api.nvim_create_user_command("Fold", "call CocAction('fold', <f-args>)", {nargs = '?'})

-- Add `:OR` command for organize imports of the current buffer
vim.api.nvim_create_user_command("OR", "call CocActionAsync('runCommand', 'editor.action.organizeImport')", {})

-- Add (Neo)Vim's native statusline support
-- NOTE: Please see `:h coc-status` for integrations with external plugins that
-- provide custom statusline: lightline.vim, vim-airline
vim.opt.statusline:prepend("%{coc#status()}%{get(b:,'coc_current_function','')}")

