local myBase16 = {
    normal = {
        a = { fg = Colors.base05, bg = Colors.base0D, gui = 'bold' },
        b = { fg = Colors.base05, bg = Colors.base03 },
        c = { fg = Colors.base04, bg = Colors.base01 },
    },
    insert = { a = { fg = Colors.base01, bg = Colors.base0B, gui = 'bold' } },
    visual = { a = { fg = Colors.base05, bg = Colors.base0E, gui = 'bold' } },
    replace = { a = { fg = Colors.base05, bg = Colors.base08, gui = 'bold' } },
    inactive = {
        a = { fg = Colors.base04, bg = Colors.base03, gui = 'bold' },
        b = { fg = Colors.base03, bg = Colors.base02 },
        c = { fg = Colors.base04, bg = Colors.base02 },
    },
}


require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = myBase16,
    component_separators = { left = '|', right = '|'},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    }
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {}
}
