# vim: ft=zsh
typeset -U path
path=(
	$HOME/.cargo/env
	$HOME/.local/bin:
	$HOME/.poetry/bin
	$CARGO_HOME/bin/
	$GOPATH/bin/   
	$XDG_CONFIG_HOME/emacs/bin/
	$HOME/.local/opt/emacs/bin/
	/Library/TeX/texbin
	/opt/homebrew/bin/
	/opt/homebrew/sbin
	/Users/emma/.local/share/rustup/toolchains/esp/xtensa-esp-elf/esp-13.2.0_20230928/xtensa-esp-elf/bin
	$path
)

fpath=(":$ZDOTDIR/prompts" "$fpath[@]")
fpath=(":$ZDOTDIR/functions" "$fpath[@]")

if [[ $(uname) == "Darwin" ]]; then # Checks if OS is MacOS
    fpath+=(":/opt/homebrew/share/zsh/site-functions")
else # Linux etc
    fpath+=(":/usr/local/share/zsh/site-functions")
fi
