# vim: ft=zsh
path+=$HOME/.cargo/env:$PATH
path+=$HOME/.local/bin:$PATH
path+=$HOME/.poetry/bin:$PATH
path+=$CARGO_HOME/bin/:$PATH
path+=$GOPATH/bin/
path+=$XDG_CONFIG_HOME/emacs/bin/


PATH=/opt/homebrew/bin/:/opt/homebrew/sbin:$PATH

PATH="/Users/emma/.local/share/rustup/toolchains/esp/xtensa-esp-elf/esp-13.2.0_20230928/xtensa-esp-elf/bin:$PATH"
export LIBCLANG_PATH="/Users/emma/.local/share/rustup/toolchains/esp/xtensa-esp32-elf-clang/esp-16.0.4-20231113/esp-clang/lib"
export CLANGD_BIN="clangd"


fpath=(":$ZDOTDIR/prompts" "$fpath[@]")
fpath=(":$ZDOTDIR/functions" "$fpath[@]")
if [[ $(uname) == "Darwin" ]]; then # Checks if OS is MacOS
    fpath+=(":/opt/homebrew/share/zsh/site-functions")
else # Linux etc
    fpath+=(":/usr/local/share/zsh/site-functions")
fi

export PATH
