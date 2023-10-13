# neovim_config

## Dependencies

* Base16 themes configured in your shell.
  * Alternatively: (Replace base16 with your own colorscheme, (base16.lua, nvim-base16 in packer.lua, lines in options.lua))
* Packer

### Prerequisite configs

Put this in your .zshenv, or locate any mention of these variables in the config and replace with absolute paths

    export XDG_CONFIG_HOME="$HOME/.config"
    export XDG_DATA_HOME="$HOME/.local/share"
    export XDG_CACHE_HOME="$HOME/.cache"
    export XDG_STATE_HOME=$HOME/.local/state

## Setup

open `nvim` and run `:PackerSync` to install the packages.
