local pylsp = require("mason-registry").get_package("python-lsp-server")
pylsp:on("install:success", function()
    local function mason_package_path(package)
        local path = vim.fn.resolve(vim.fn.stdpath("data") .. "/mason/packages/" .. package)
        return path
    end

    local path = mason_package_path("python-lsp-server")
    local command = path .. "/venv/bin/pip"
    local args = {
        "install",
        "python-lsp-ruff",
    }

    require("plenary.job")
        :new({
            command = command,
            args = args,
            cwd = path,
        })
        :start()
end)

local pylsp = {
    settings = {
        pylsp = {
            plugins = {
                jedi_completion = {
                    enabled = true,
                    include_params = true,
                    fuzzy = true,
                },
                ruff = {
                    enabled = true,                                        -- Enable the plugin
                    path = os.getenv('HOME') .. "/.config/ruff/ruff.toml", -- Custom config for ruff to use
                    select = {
                        "E",
                        "F",
                        "I",   -- isort

                        "ARG", -- Unused arguments
                        "COM", -- commas
                        "DTZ", -- datetimez
                        "EM",  -- errmsg
                        "EXE", -- executable
                        "ISC", -- implicit str concat
                    },
                    severities = {
                        ["F401"] = "W",
                    },
                },
                autopep8 = {
                    enabled = false,
                },
                flake8 = {
                    enabled = false,
                },
                mccabe = {
                    enabled = false,
                },
                preload = {
                    enabled = false,
                },
                pycodestyle = {
                    enabled = false,
                },
                pydocstyle = {
                    enabled = false,
                },
                pyflakes = {
                    enabled = false,
                },
                pylint = {
                    enabled = false,
                },
                rope_autoimport = {
                    enabled = false,
                },
                rope_completions = {
                    enabled = false,
                },
                yapf = {
                    enabled = false,
                },
            },
        },
    },
}

return pylsp
