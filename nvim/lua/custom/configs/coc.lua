vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.signcolumn = "yes"

-- Set default key for snippet jumping.
-- Takes priority over other tab binds. But only when snippet can be jumped.
vim.g.coc_snippet_next = "<Tab>"
vim.g.coc_snippet_prev = "<S-Tab>"

-- Helper Functions
local keyset = function(mode, lhs, rhs, opts)
    opts = opts or { silent = true }
    vim.keymap.set(mode, lhs, rhs, opts)
end

local function ternary_str(condition, action, else_action)
    local str = condition .. " ? " .. action .. " : " .. else_action
    return str
end

local function ternary_keyset(mode, key, condition, command, opts, else_cmd)
    else_cmd = else_cmd or ('"\\' .. key .. '"')
    opts = opts or { expr = true }
    keyset(mode, key, ternary_str(condition, command, else_cmd), opts)
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
keyset("n", "gs", "call CocAction('showSignatureHelp')", { desc = "[g]et [s]ignature" })
keyset("n", "gD", "<plug>(coc-declaration)")
keyset("n", "gr", "<plug>(coc-referances)")
keyset("n", "gR", "<plug>(coc-referances-used)")

-- Refactor and Format
keyset("n", "<leader>rn", "<plug>(coc-rename)")
keyset("n", "<leader>fm", "<plug>(coc-format)")
keyset("x", "<leader>fm", "<plug>(coc-format-selected)")
keyset("n", "<leader>rf", "<plug>(coc-refactor)")
keyset("n", "<leader>rF", "<plug>(coc-codeaction-refactor)")

-- Use CTRL-S for selections ranges
-- Requires 'textDocument/selectionRange' support of language server
keyset("n", "<C-s>", "<Plug>(coc-range-select)")
keyset("x", "<C-s>", "<Plug>(coc-range-select)")

-- Code Actions
local opts = { silent = true, nowait = true }
keyset("x", "<leader>a", "<Plug>(coc-codeaction-selected)", opts)
keyset("n", "<leader>a", "<Plug>(coc-codeaction-selected)", opts)
keyset("n", "<leader>ac", "<Plug>(coc-codeaction-cursor)", opts)
keyset("n", "<leader>as", "<Plug>(coc-codeaction-source)", opts)
keyset("n", "<leader>qf", "<Plug>(coc-fix-current)", opts)
keyset("n", "<leader>cl", "<Plug>(coc-codelens-action)", opts)

local opts = { silent = true, nowait = true, expr = true }
-- Scroll in floating windows
ternary_keyset("n", "<C-f>", 'coc#float#has_scroll()', 'coc#float#scroll(1)', opts)
ternary_keyset("n", "<C-b>", 'coc#float#has_scroll()', 'coc#float#scroll(0)', opts)

-- Completions Keymaps
local opts = { silent = true, noremap = true, expr = true, replace_keycodes = false }
ternary_keyset("i", "<CR>", "coc#pum#visible()", "coc#pum#confirm()", opts)
ternary_keyset("i", "<Tab>", "coc#pum#visible()", "coc#pum#confirm()", opts)
ternary_keyset("i", "<C-p>", "coc#pum#visible()", "coc#pum#prev(1)", opts)
ternary_keyset("i", "<C-n>", "coc#pum#visible()", "coc#pum#next(1)", opts,
    ternary_str("v:lua.check_back_space()", '"<C-n>"', "coc#refresh()")) -- Two ternaries chained

-- Use K to show documentation in preview window
function _G.show_docs()
    local cw = vim.fn.expand('<cword>')
    if vim.fn.index({ 'vim', 'help' }, vim.bo.filetype) >= 0 then
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
vim.api.nvim_create_user_command("Fold", "call CocAction('fold', <f-args>)", { nargs = '?' })

-- Add `:OR` command for organize imports of the current buffer
vim.api.nvim_create_user_command("OR", "call CocActionAsync('runCommand', 'editor.action.organizeImport')", {})

-- Add (Neo)Vim's native statusline support
-- NOTE: Please see `:h coc-status` for integrations with external plugins that
-- provide custom statusline: lightline.vim, vim-airline
vim.opt.statusline:prepend("%{coc#status()}%{get(b:,'coc_current_function','')}")

-- Highlight wo
-- autocmd CursorHold * silent call CocActionAsync('highlight')
vim.api.nvim_create_autocmd("CursorHold", {
    pattern = "*",
    command = "silent call CocActionAsync('highlight')",
})
