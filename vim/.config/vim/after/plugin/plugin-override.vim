vim9script
# clipboard (necessary for wayland)
if exists('g:loaded_system_copy')
	nmap sy <Plug>SystemCopy
	xmap sy <Plug>SystemCopy
	nmap sY <Plug>SystemCopyLine
	nmap sp <Plug>SystemPaste
	xmap sp <Plug>SystemPaste
	nmap sP <Plug>SystemPasteLine
endif

if exists('g:loaded_fzf')
	g:fzf_layout = { 'window': 'botright 10new' }
	g:fzf_history_dir = $XDG_STATE_HOME .. "/fzf-history"

	def g:FzfFiles()
		var src = split(system('find ./ -type f'), '\n')

		var curfile = './' .. expand('%')
		var idx = 0
		if (src[0] == curfile)
			var tmp = src[1]
			src[1] = src[0]
			src[0] = tmp
		endif

		fzf#run(fzf#wrap({'source': src, 'sink': 'e'}))
	enddef

	nmap <Leader>fz :call FzfFiles()<CR>
	nmap <Leader>fg :call fzf#run(fzf#wrap({'source': 'git ls-files'}))<CR>

	# first word (separated by space) is buffer number
	def Openbuf(s: string)
		:exec $"buffer {split(s, ' ')[0]}"
	enddef

	def g:Fzfbuf()
		# first word (separated by space) is buffer number
		var src = getbufinfo({'buflisted': 1})->map(
			(_, val) => $"{val.bufnr} {g:Relpath(val.name)}"
		)

		var bpnr = string(bufnr('#'))
		var idx = 0
		while idx < len(src)
			if (split(src[idx], ' ')[0] == bpnr)
				var tmp = src[idx]
				src[idx] = src[0]
				src[0] = tmp
			endif
			idx += 1
		endwhile

		fzf#run(fzf#wrap({'source': src, 'sink': Openbuf}))
	enddef

	nmap <Leader>, :call g:Fzfbuf()<CR>
endif
if exists('g:loaded_fugitive')
	set statusline=%<%f\ %h%m%r%{FugitiveStatusline()}%=%-14.(%l,%c%V%)\ %P
endif

if exists('g:loaded_taginclude')
	g:globalIncludePathsC = ['/usr/include']
endif
