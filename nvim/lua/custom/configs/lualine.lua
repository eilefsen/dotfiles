local colors = {
  black   = vim.g.terminal_color_0,
  maroon  = vim.g.terminal_color_1,
  green   = vim.g.terminal_color_2,
  olive   = vim.g.terminal_color_2,
  navy    = vim.g.terminal_color_4,
  purple  = vim.g.terminal_color_5,
  teal    = vim.g.terminal_color_6,
  silver  = vim.g.terminal_color_8,
  gray    = vim.g.terminal_color_8,
  red     = vim.g.terminal_color_1,
  lime    = vim.g.terminal_color_2,
  yellow  = vim.g.terminal_color_3,
  blue    = vim.g.terminal_color_4,
  fuchsia = vim.g.terminal_color_5,
  aqua    = vim.g.terminal_color_6,
  white   = vim.g.terminal_color_15,
}

local myBase16 = {
    normal = {
        a = { fg = colors.white, bg = colors.blue, gui = 'bold' },
        b = { fg = colors.white, bg = colors.gray },
        c = { fg = colors.silver, bg = colors.black },
    },
    insert = { a = { fg = colors.white, bg = colors.green, gui = 'bold' } },
    visual = { a = { fg = colors.white, bg = colors.purple, gui = 'bold' } },
    replace = { a = { fg = colors.white, bg = colors.red, gui = 'bold' } },
    inactive = {
        a = { fg = colors.silver, bg = colors.gray, gui = 'bold' },
        b = { fg = colors.gray, bg = colors.black },
        c = { fg = colors.silver, bg = colors.black },
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
