# General variables
export EDITOR=nvim
export VISUAL=nvim
# Python
export VIRTUAL_ENV_DISABLE_PROMPT=1

export CARGO_HOME="$XDG_DATA_HOME"/cargo
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export LIBCLANG_PATH="$RUSTUP_HOME/toolchains/esp/xtensa-esp32-elf-clang/esp-16.0.4-20231113/esp-clang/lib"
export GOPATH="$XDG_DATA_HOME"/go
export LESSHISTFILE="$XDG_STATE_HOME"/less/history
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export PYTHONSTARTUP="${XDG_CONFIG_HOME}/python/pythonrc"
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
export MYSQL_HISTFILE="$XDG_DATA_HOME"/mysql_history
export W3M_DIR="$XDG_DATA_HOME"/w3m
export ANDROID_HOME="$XDG_DATA_HOME"/android
export FLAVOURS_DATA_DIRECTORY="${XDG_DATA_HOME}/flavours"
export FLAVOURS_CONFIG_FILE="${XDG_CONFIG_HOME}/flavours/config.toml"
export PICO_SDK_PATH="$HOME/Code/pico/pico-sdk"

# Various
MOZ_ENABLE_WAYLAND=1
. "/home/emma/.local/share/cargo/env"
