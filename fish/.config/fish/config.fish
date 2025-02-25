set PATH $PATH "$HOME/.local/bin"
set XDG_CONFIG_HOME "$HOME/.config"
set XDG_DATA_HOME "$HOME/.local/share"
set XDG_CACHE_HOME "$HOME/.cache"
set XDG_STATE_HOME "$HOME/.local/state"

if status is-interactive
    # Commands to run in interactive sessions can go here
	set -g fish_greeting "$(set_color -i brgreen; fish --version)"

	# fzf --fish | source # Uncomment for fzf integration
end

if status is-login
    exec sh -c ". /etc/profile; exec fish"
end
