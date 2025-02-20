set tags+=/usr/include/tags

function s:parsetagfile(tagname, tagsource)
	let tfiles = split(system("cat " . a:tagsource . " | rg \'^" . a:tagname . "\\b\' | cut -f2 | sort | uniq"), '\n')
	let arr = []
	for tfile in tfiles
		call add(arr, tfile . ' ' . a:tagsource )
	endfor
	return arr
endfunction

function GetTagFiles(tagname)
	" Cat all tagfiles into ripgrep which matches for exact tagname, then gets
	" the associated filename using cut. Finally, sort and remove duplicates
	" let tagsource = join(tagfiles(), " ")
	let tagfiles = []
	for tsource in tagfiles()
		call add(tagfiles, s:parsetagfile(a:tagname, tsource))
	endfor
	" let tagfiles = system("cat " . tagsource . " | rg \'^" . a:tagname . "\\b\' | cut -f2 | sort | uniq")
	" Make an array out of the tagfiles
	return flattennew(tagfiles)
endfunction

function s:getrelativetagpath(tagsfile, tagpath)
	let tagsfiledirabs = system('dirname $(realpath ' . a:tagsfile . ')')
	" let tagsfiledirabs = expand(. ':p:h')
	let fileabs = expand('%:p:h')
	let tagabs = trim(tagsfiledirabs) . '/' . a:tagpath
	let relpath = system('realpath -s --relative-to=' . fileabs . ' ' . tagabs)
	return relpath
endfunction

function AppendInclude(tagfiles, surround1, surround2, userelative = v:false)
	if empty(a:tagfiles)
		return 0
	endif

	func! s:appendToBuffer(idx) closure
		let splitfile = split(a:tagfiles[a:idx], ' ')
		let tfile = splitfile[0]
		let tagspath = splitfile[1]
		:if a:userelative
			let tfile = trim(s:getrelativetagpath(tagspath, tfile))
		:endif
		let toAppend = a:surround1 . tfile . a:surround2
		let search_result = search('^' . toAppend . '$', 'n')
		if !search_result
			:call append(0, toAppend)
		else
			echo tfile . ' is already included, line: ' . search_result 
		endif
	endfunc
	:if len(a:tagfiles) == 1
	:call s:appendToBuffer(0)
	return 1
	:endif

	func! s:selectedTag(id, result) closure
		:call s:appendToBuffer(a:result-1)
	endfunc

	let menuitems = []
	for tfile in a:tagfiles
		let item = split(tfile, ' ')[0]
		call add(menuitems, item)
	endfor

	call popup_menu(a:tagfiles, #{
				\ callback: function('s:selectedTag'),
				\ })
	return 1
endfunction

function s:getmatchtext(key,val)
	return a:val.text
endfunction

function s:sortheadersfirst(a,b)
	let ah = !empty(a:a->matchstr('.*\.hpp')) || !empty(a:a->matchstr('.*\.h'))
	let bc = !empty(a:b->matchstr('.*\.cpp')) || !empty(a:b->matchstr('.*\.c'))
	let ac = !empty(a:a->matchstr('.*\.cpp')) || !empty(a:a->matchstr('.*\.c'))
	let bh = !empty(a:b->matchstr('.*\.hpp')) || !empty(a:b->matchstr('.*\.h'))

	if ah && bc
		return -1
	elseif ac && bh
		return 1
	else
		return 0
	endif
endfunction

function TagIncludeC(tagname, filter_header)
	set tags-=/usr/include/tags
	let tagfiles = GetTagFiles(a:tagname)
	if a:filter_header
		let tagfiles = matchstrlist(tagfiles, '\<\w\+\(\.h\|\.hpp\)\>')->map(function('s:getmatchtext'))
	else
		let tagfiles = tagfiles->sort(function('s:sortheadersfirst'))
	endif
	let first_append = AppendInclude(tagfiles,"#include \"", '"', v:true)

	let old_tags_option=&tags
	:if first_append != 0
	return
	:endif

	set tags=/usr/include/tags
	let tagfiles = GetTagFiles(a:tagname)
	:if a:filter_header
		let tagfiles = matchstrlist(tagfiles, '\<\w\+\(\.h\|\.hpp\)\>')->map(function('s:getmatchtext'))
	:endif
	:if !AppendInclude(tagfiles,"#include <", '>', v:false)
	echo 'No tags found for ' . a:tagname
	:endif


	let &tags = old_tags_option
	set tags+=/usr/include/tags
endfunction

function TagInclude(name)
	let n = shellescape(a:name)
	if &ft == "c" || &ft == "cpp"
		return TagIncludeC(a:name, v:false)
	else
		echo "No TagInclude function available for filetype: " . &ft
	endif
endfunction

command -nargs=1 TagInclude :call TagInclude(<q-args>)
command TagIncludeAtPoint :call TagInclude(expand('<cword>'))

nnoremap <Leader>ti <Cmd>TagIncludeAtPoint<CR>


" vim:foldmethod=marker:foldlevel=0
