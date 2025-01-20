# simple plugin management (unplugged)
ZPLUGINDIR=${XDG_DATA_HOME}/zsh/plugins

function plugin-clone {
	local repo plugdir initfile initfiles=()
	ZPLUGINDIR=${ZPLUGINDIR:-${ZDOTDIR:-$HOME/.config/zsh}/plugins}
	for repo in $@; do
		plugdir=$ZPLUGINDIR/${repo:t}
		initfile=$plugdir/${repo:t}.plugin.zsh
		if [[ ! -d $plugdir ]]; then
			echo "Cloning $repo..."
			git clone -q --depth 1 --recursive --shallow-submodules \
				https://github.com/$repo $plugdir
		fi
		if [[ ! -e $initfile ]]; then
			initfiles=($plugdir/*.{plugin.zsh,zsh-theme,zsh,sh}(N))
			(( $#initfiles )) && ln -sf $initfiles[1] $initfile
		fi
done
}

function plugin-compile {
  ZPLUGINDIR=${ZPLUGINDIR:-$HOME/.config/zsh/plugins}
  autoload -U zrecompile
  local f
  for f in $ZPLUGINDIR/**/*.zsh{,-theme}(N); do
    zrecompile -pq "$f"
  done
}

function plugin-source {
	local plugdir
	ZPLUGINDIR=${ZPLUGINDIR:-${ZDOTDIR:-$HOME/.config/zsh}/plugins}
	for plugdir in $@; do
		[[ $plugdir = /* ]] || plugdir=$ZPLUGINDIR/$plugdir
		fpath+=$plugdir
		local initfile=$plugdir/${plugdir:t}.plugin.zsh
		(( $+functions[zsh-defer] )) && zsh-defer . $initfile || . $initfile
done
}

function get-plugin-name-from-repo {
	for repo ($@); echo ${repo:t};
}

# use this with plugins that are not nested within a repo
function plugin-load {
	plugin-clone $@
	local plugins=($(get-plugin-name-from-repo $@));
	plugin-source $plugins;
}

