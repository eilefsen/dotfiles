if status is-interactive
	bind \ep 'history-token-search-backward'
	bind \en 'history-token-search-forward'
	bind \ej 'history-prefix-search-forward'
	bind \ek 'history-prefix-search-backward'
	bind \cO 'commandline -a \' &| less\''

	# replace Alt+S sudo substitution with doas
	function doas-sub
		set -f cmd $(commandline)
		if [ -z $cmd ] && [ -z $(string match --regex '^doas.*$' $history[1]) ]
			commandline "doas $history[1]";
		else if [ -z $(string match --regex '^doas.*$' $cmd) ]
			commandline -r "doas $cmd";
		end
	end
	bind \es "doas-sub"
end
