-- base16-nvim (https://github.com/wincent/base16-nvim)
-- by Greg Hurrell (https://github.com/wincent)
-- based on
-- base16-vim (https://github.com/chriskempson/base16-vim)
-- by Chris Kempson (https://github.com/chriskempson)
-- Felinoid scheme by Emma Eilefsen Glenna (http://eilefsen.net)

local cterm00 = 0
local cterm03 = 8
local cterm05 = 7
local cterm07 = 15
local cterm08 = 1
local cterm0A = 3
local cterm0B = 2
local cterm0C = 6
local cterm0D = 4
local cterm0E = 5
local cterm01 = 10
local cterm02 = 11
local cterm04 = 12
local cterm06 = 13
local cterm09 = 9
local cterm0F = 14

vim.cmd [[
  highlight clear
  syntax reset
]]
vim.g.colors_name = "base16-felinoid"

Colors = {
    base00 = "#232629",
    base01 = "#3e4349",
    base02 = "#474d53",
    base03 = "#626b73",
    base04 = "#c9cdd1",
    base05 = "#f8f8f2",
    base06 = "#fcfcf6",
    base07 = "#fcfcfc",
    base08 = "#fb0029",
    base09 = "#fba900",
    base0A = "#fbd300",
    base0B = "#00fb55",
    base0C = "#c379e4",
    base0D = "#00a6fb",
    base0E = "#fb00a6",
    base0F = "#fb5500",
}

-- Vim editor colors                    fg bg ctermfg ctermbg attr Colors.basesp
vim.api.nvim_set_hl(0, 'Normal', { fg = Colors.base05, bg = Colors.base00, ctermfg = cterm05, ctermbg = cterm00 })
vim.api.nvim_set_hl(0, 'Bold', { bold = true })
vim.api.nvim_set_hl(0, 'Debug', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'Directory', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'Error', { fg = Colors.base00, bg = Colors.base08, ctermfg = cterm00, ctermbg = cterm08 })
vim.api.nvim_set_hl(0, 'ErrorMsg', { fg = Colors.base08, bg = Colors.base00, ctermfg = cterm08, ctermbg = cterm00 })
vim.api.nvim_set_hl(0, 'Exception', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'FoldColumn', { fg = Colors.base0C, bg = Colors.base01, ctermfg = cterm0C, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'Folded', { fg = Colors.base03, bg = Colors.base01, ctermfg = cterm03, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'IncSearch', { fg = Colors.base01, bg = Colors.base09, ctermfg = cterm01, ctermbg = cterm09 })
vim.api.nvim_set_hl(0, 'Italic', {})
vim.api.nvim_set_hl(0, 'Macro', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'MatchParen', { bg = Colors.base03, ctermbg = cterm03 })
vim.api.nvim_set_hl(0, 'ModeMsg', { fg = Colors.base0B, ctermfg = cterm0B })
vim.api.nvim_set_hl(0, 'MoreMsg', { fg = Colors.base0B, ctermfg = cterm0B })
vim.api.nvim_set_hl(0, 'Question', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'Search', { fg = Colors.base01, bg = Colors.base0A, ctermfg = cterm01, ctermbg = cterm0A })
vim.api.nvim_set_hl(0, 'Substitute', { fg = Colors.base01, bg = Colors.base0A, ctermfg = cterm01, ctermbg = cterm0A })
vim.api.nvim_set_hl(0, 'SpecialKey', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'TooLong', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'Underlined', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'Visual', { bg = Colors.base02, ctermbg = cterm02 })
vim.api.nvim_set_hl(0, 'VisualNOS', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'WarningMsg', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'WildMenu', { fg = Colors.base08, bg = Colors.base0A, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'Title', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'Conceal', { fg = Colors.base0D, bg = Colors.base00, ctermfg = cterm0D, ctermbg = cterm00 })
vim.api.nvim_set_hl(0, 'Cursor', { fg = Colors.base00, bg = Colors.base05, ctermfg = cterm00, ctermbg = cterm05 })
vim.api.nvim_set_hl(0, 'NonText', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'LineNr', { fg = Colors.base03, bg = Colors.base01, ctermfg = cterm03, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'SignColumn', { fg = Colors.base03, bg = Colors.base01, ctermfg = cterm03, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'StatusLine', { fg = Colors.base04, bg = Colors.base02, ctermfg = cterm04, ctermbg = cterm02 })
vim.api.nvim_set_hl(0, 'StatusLineNC', { fg = Colors.base03, bg = Colors.base01, ctermfg = cterm03, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'VertSplit', { fg = Colors.base02, bg = Colors.base02, ctermfg = cterm02, ctermbg = cterm02 })
vim.api.nvim_set_hl(0, 'ColorColumn', { bg = Colors.base01, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'CursorColumn', { bg = Colors.base01, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'CursorLine', { bg = Colors.base01, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = Colors.base04, bg = Colors.base01, ctermfg = cterm04, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'QuickFixLine', { bg = Colors.base01, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'PMenu', { fg = Colors.base05, bg = Colors.base01, ctermfg = cterm05, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'PMenuSel', { fg = Colors.base01, bg = Colors.base05, ctermfg = cterm01, ctermbg = cterm05 })
vim.api.nvim_set_hl(0, 'TabLine', { fg = Colors.base03, bg = Colors.base01, ctermfg = cterm03, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'TabLineFill', { fg = Colors.base03, bg = Colors.base01, ctermfg = cterm03, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'TabLineSel', { fg = Colors.base0B, bg = Colors.base01, ctermfg = cterm0B, ctermbg = cterm01 })

-- Standard syntax highlighting
vim.api.nvim_set_hl(0, 'Boolean', { fg = Colors.base09, ctermfg = cterm09 })
vim.api.nvim_set_hl(0, 'Character', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'Comment', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'Conditional', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'Constant', { fg = Colors.base09, ctermfg = cterm09 })
vim.api.nvim_set_hl(0, 'Define', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'Delimiter', { fg = Colors.base0F, ctermfg = cterm0F })
vim.api.nvim_set_hl(0, 'Float', { fg = Colors.base09, ctermfg = cterm09 })
vim.api.nvim_set_hl(0, 'Function', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'Identifier', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'Include', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'Keyword', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'Label', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'Number', { fg = Colors.base09, ctermfg = cterm09 })
vim.api.nvim_set_hl(0, 'Operator', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'PreProc', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'Repeat', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'Special', { fg = Colors.base0C, ctermfg = cterm0C })
vim.api.nvim_set_hl(0, 'SpecialChar', { fg = Colors.base0F, ctermfg = cterm0F })
vim.api.nvim_set_hl(0, 'Statement', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'StorageClass', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'String', { fg = Colors.base0B, ctermfg = cterm0B })
vim.api.nvim_set_hl(0, 'Structure', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'Tag', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'Todo', { fg = Colors.base0A, bg = Colors.base01, ctermfg = cterm0A, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'Type', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'Typedef', { fg = Colors.base0A, ctermfg = cterm0A })

-- C highlighting
vim.api.nvim_set_hl(0, 'cOperator', { fg = Colors.base0C, ctermfg = cterm0C })
vim.api.nvim_set_hl(0, 'cPreCondit', { fg = Colors.base0E, ctermfg = cterm0E })

-- C# highlighting
vim.api.nvim_set_hl(0, 'csClass', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'csAttribute', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'csModifier', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'csType', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'csUnspecifiedStatement', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'csContextualStatement', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'csNewDecleration', { fg = Colors.base08, ctermfg = cterm08 })

-- CSS highlighting
vim.api.nvim_set_hl(0, 'cssBraces', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'cssClassName', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'cssColor', { fg = Colors.base0C, ctermfg = cterm0C })

-- Diff highlighting
vim.api.nvim_set_hl(0, 'DiffAdd', { fg = Colors.base0B, bg = Colors.base01, ctermfg =  cterm0B, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'DiffChange', { fg = Colors.base03, bg = Colors.base01, ctermfg =  cterm03, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'DiffDelete', { fg = Colors.base08, bg = Colors.base01, ctermfg =  cterm08, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'DiffText', { fg = Colors.base0D, bg = Colors.base01, ctermfg =  cterm0D, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'DiffAdded', { fg = Colors.base0B, bg = Colors.base00, ctermfg =  cterm0B, ctermbg = cterm00 })
vim.api.nvim_set_hl(0, 'DiffFile', { fg = Colors.base08, bg = Colors.base00, ctermfg =  cterm08, ctermbg = cterm00 })
vim.api.nvim_set_hl(0, 'DiffNewFile', { fg = Colors.base0B, bg = Colors.base00, ctermfg =  cterm0B, ctermbg = cterm00 })
vim.api.nvim_set_hl(0, 'DiffLine', { fg = Colors.base0D, bg = Colors.base00, ctermfg =  cterm0D, ctermbg = cterm00 })
vim.api.nvim_set_hl(0, 'DiffRemoved', { fg = Colors.base08, bg = Colors.base00, ctermfg =  cterm08, ctermbg = cterm00 })

-- Git highlighting
vim.api.nvim_set_hl(0, 'gitcommitOverflow', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'gitcommitSummary', { fg = Colors.base0B, ctermfg = cterm0B })
vim.api.nvim_set_hl(0, 'gitcommitComment', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'gitcommitUntracked', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'gitcommitDiscarded', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'gitcommitSelected', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'gitcommitHeader', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'gitcommitSelectedType', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'gitcommitUnmergedType', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'gitcommitDiscardedType', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'gitcommitBranch', { fg = Colors.base09, ctermfg = cterm09, bold = true })
vim.api.nvim_set_hl(0, 'gitcommitUntrackedFile', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'gitcommitUnmergedFile', { fg = Colors.base08, ctermfg = cterm08, bold = true })
vim.api.nvim_set_hl(0, 'gitcommitDiscardedFile', { fg = Colors.base08, ctermfg = cterm08, bold = true })
vim.api.nvim_set_hl(0, 'gitcommitSelectedFile', { fg = Colors.base0B, ctermfg = cterm0B, bold = true })

-- GitGutter highlighting
vim.api.nvim_set_hl(0, 'GitGutterAdd', { fg = Colors.base0B, bg = Colors.base01, ctermfg = cterm0B, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'GitGutterChange', { fg = Colors.base0D, bg = Colors.base01, ctermfg = cterm0D, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'GitGutterDelete', { fg = Colors.base08, bg = Colors.base01, ctermfg = cterm08, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'GitGutterChangeDelete', { fg = Colors.base0E, bg = Colors.base01, ctermfg = cterm0E, ctermbg = cterm01 })

-- HTML highlighting
vim.api.nvim_set_hl(0, 'htmlBold', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'htmlItalic', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'htmlEndTag', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'htmlTag', { fg = Colors.base05, ctermfg = cterm05 })

-- JavaScript highlighting
vim.api.nvim_set_hl(0, 'javaScript', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'javaScriptBraces', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'javaScriptNumber', { fg = Colors.base09, ctermfg = cterm09 })

-- pangloss/vim-javascript highlighting
vim.api.nvim_set_hl(0, 'jsOperator', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'jsStatement', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'jsReturn', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'jsThis', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'jsClassDefinition', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'jsFunction', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'jsFuncName', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'jsFuncCall', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'jsClassFuncName', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'jsClassMethodType', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'jsRegexpString', { fg = Colors.base0C, ctermfg = cterm0C })
vim.api.nvim_set_hl(0, 'jsGlobalObjects', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'jsGlobalNodeObjects', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'jsExceptions', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'jsBuiltins', { fg = Colors.base0A, ctermfg = cterm0A })

-- LSP highlighting
vim.api.nvim_set_hl(0, 'LspDiagnosticsDefaultError', { fg = Colors.base08, ctermfg = cterm08})
vim.api.nvim_set_hl(0, 'LspDiagnosticsDefaultWarning', { fg = Colors.base09, ctermfg = cterm09 })
vim.api.nvim_set_hl(0, 'LspDiagnosticsDefaultInformation', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'LspDiagnosticsDefaultHint', { fg = Colors.base03, ctermfg = cterm03 })
-- Diagnostics undercurl
vim.api.nvim_set_hl(0, 'LspDiagnosticsUnderlineError', { fg = nil, ctermfg = nil, sp=Colors.base08, undercurl = true })
vim.api.nvim_set_hl(0, 'LspDiagnosticsUnderlineWarning', { fg = nil, ctermfg = nil, sp=Colors.base09, undercurl = true  })
vim.api.nvim_set_hl(0, 'LspDiagnosticsUnderlineInformation', { fg = nil, ctermfg = nil, sp=Colors.base05, undercurl = true })
vim.api.nvim_set_hl(0, 'LspDiagnosticsUnderlineHint', { fg = nil, ctermfg = nil, sp=Colors.base03, undercurl = true })
vim.api.nvim_set_hl(0, 'DiagnosticUnderlineError', { fg = nil, ctermfg = nil, sp=Colors.base08, undercurl = true })
vim.api.nvim_set_hl(0, 'DiagnosticUnderlineWarn', { fg = nil, ctermfg = nil, sp=Colors.base09, undercurl = true })
vim.api.nvim_set_hl(0, 'DiagnosticUnderlineInfo', { fg = nil, ctermfg = nil, sp=Colors.base05, undercurl = true })
vim.api.nvim_set_hl(0, 'DiagnosticUnderlineHint', { fg = nil, ctermfg = nil, sp=Colors.base03, undercurl = true })

-- Mail highlighting
vim.api.nvim_set_hl(0, 'mailQuoted1', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'mailQuoted2', { fg = Colors.base0B, ctermfg = cterm0B })
vim.api.nvim_set_hl(0, 'mailQuoted3', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'mailQuoted4', { fg = Colors.base0C, ctermfg = cterm0C })
vim.api.nvim_set_hl(0, 'mailQuoted5', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'mailQuoted6', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'mailURL', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'mailEmail', { fg = Colors.base0D, ctermfg = cterm0D })

-- Markdown highlighting
vim.api.nvim_set_hl(0, 'markdownCode', { fg = Colors.base0B, ctermfg = cterm0B })
vim.api.nvim_set_hl(0, 'markdownError', { fg = Colors.base05, bg = Colors.base00, ctermfg = cterm05, ctermbg = cterm00 })
vim.api.nvim_set_hl(0, 'markdownCodeBlock', { fg = Colors.base0B, ctermfg = cterm0B })
vim.api.nvim_set_hl(0, 'markdownHeadingDelimiter', { fg = Colors.base0D, ctermfg = cterm0D })

-- NERDTree highlighting
vim.api.nvim_set_hl(0, 'NERDTreeDirSlash', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'NERDTreeExecFile', { fg = Colors.base05, ctermfg = cterm05 })

-- PHP highlighting
vim.api.nvim_set_hl(0, 'phpMemberSelector', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'phpComparison', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'phpParent', { fg = Colors.base05, ctermfg = cterm05 })
vim.api.nvim_set_hl(0, 'phpMethodsVar', { fg = Colors.base0C, ctermfg = cterm0C })

-- Python highlighting
vim.api.nvim_set_hl(0, 'pythonOperator', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'pythonRepeat', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'pythonInclude', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'pythonStatement', { fg = Colors.base0E, ctermfg = cterm0E })

-- Ruby highlighting
vim.api.nvim_set_hl(0, 'rubyAttribute', { fg = Colors.base0D, ctermfg = cterm0D })
vim.api.nvim_set_hl(0, 'rubyConstant', { fg = Colors.base0A, ctermfg = cterm0A })
vim.api.nvim_set_hl(0, 'rubyInterpolationDelimiter', { fg = Colors.base0F, ctermfg = cterm0F })
vim.api.nvim_set_hl(0, 'rubyRegexp', { fg = Colors.base0C, ctermfg = cterm0C })
vim.api.nvim_set_hl(0, 'rubySymbol', { fg = Colors.base0B, ctermfg = cterm0B })
vim.api.nvim_set_hl(0, 'rubyStringDelimiter', { fg = Colors.base0B, ctermfg = cterm0B })

-- SASS highlighting
vim.api.nvim_set_hl(0, 'sassidChar', { fg = Colors.base08, ctermfg = cterm08 })
vim.api.nvim_set_hl(0, 'sassClassChar', { fg = Colors.base09, ctermfg = cterm09 })
vim.api.nvim_set_hl(0, 'sassInclude', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'sassMixing', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'sassMixinName', { fg = Colors.base0D, ctermfg = cterm0D })

-- Signify highlighting
vim.api.nvim_set_hl(0, 'SignifySignAdd', { fg = Colors.base0B, bg = Colors.base01, ctermfg = cterm0B, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'SignifySignChange', { fg = Colors.base0D, bg = Colors.base01, ctermfg = cterm0D, ctermbg = cterm01 })
vim.api.nvim_set_hl(0, 'SignifySignDelete', { fg = Colors.base08, bg = Colors.base01, ctermfg = cterm08, ctermbg = cterm01 })

-- Spelling highlighting
vim.api.nvim_set_hl(0, 'SpellBad', { undercurl = true })
vim.api.nvim_set_hl(0, 'SpellLocal', { undercurl = true })
vim.api.nvim_set_hl(0, 'SpellCap', { undercurl = true })
vim.api.nvim_set_hl(0, 'SpellRare', { undercurl = true })

-- Startify highlighting
vim.api.nvim_set_hl(0, 'StartifyBracket', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'StartifyFile', { fg = Colors.base07, ctermfg = cterm07 })
vim.api.nvim_set_hl(0, 'StartifyFooter', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'StartifyHeader', { fg = Colors.base0B, ctermfg = cterm0B })
vim.api.nvim_set_hl(0, 'StartifyNumber', { fg = Colors.base09, ctermfg = cterm09 })
vim.api.nvim_set_hl(0, 'StartifyPath', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'StartifySection', { fg = Colors.base0E, ctermfg = cterm0E })
vim.api.nvim_set_hl(0, 'StartifySelect', { fg = Colors.base0C, ctermfg = cterm0C })
vim.api.nvim_set_hl(0, 'StartifySlash', { fg = Colors.base03, ctermfg = cterm03 })
vim.api.nvim_set_hl(0, 'StartifySpecial', { fg = Colors.base03, ctermfg = cterm03 })

-- Java highlighting
vim.api.nvim_set_hl(0, 'javaOperator', { fg = Colors.base0D, ctermfg = cterm0D })

-- vim: filetype=lua