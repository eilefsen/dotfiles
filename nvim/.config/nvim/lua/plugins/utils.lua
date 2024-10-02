local M = {}

M.productivity = {
	{
		"epwalsh/pomo.nvim",
		version = "*",
		lazy = true,
		cmd = { "TimerStart", "TimerRepeat", "TimerSession" },
		opts = {
			notifiers = {
				{
					name = "Default",
					opts = {
						sticky = false,
						title_icon = "󱎫",
						text_icon = "󰄉",
					},
				},
			},
			sessions = {
				pomodoro = {
					{ name = "Work", duration = "25m" },
					{ name = "Short Break", duration = "5m" },
					{ name = "Work", duration = "25m" },
					{ name = "Short Break", duration = "5m" },
					{ name = "Work", duration = "25m" },
					{ name = "Long Break", duration = "15m" },
				},
			},
		},
	},
	{

		"nvim-lualine/lualine.nvim",
		opts = {
			sections = {
				lualine_x = {
					function()
						local ok, pomo = pcall(require, "pomo")
						if not ok then
							return ""
						end

						local timer = pomo.get_first_to_finish()
						if timer == nil then
							return ""
						end

						return "󰄉 " .. tostring(timer)
					end,
					"encoding",
					"fileformat",
					"filetype",
				},
			},
		},
	},
}

return vim.tbl_values(M)
