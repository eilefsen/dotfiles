if status is-interactive
	# ls
	abbr -a -- ll 'lsd -lah'
	#ctags
	abbr -a -- renewtags 'ctags --kinds-c=+p -R'
	#cmake
	abbr -a -- cmk 'cmake -B build -S'
	# git
	abbr -a -- gs 'git status'
	abbr -a -- ga 'git add'
	abbr -a -- gc 'git commit'
	# docker
	abbr -a -- dcu 'docker compose up -d'
	abbr -a -- dcd 'docker compose down'
	abbr -a -- dcr 'docker compose run --rm'
	abbr -a -- dcrs 'docker compose restart'
	abbr -a -- dcdu 'docker compose down && docker compose up -d'
end
