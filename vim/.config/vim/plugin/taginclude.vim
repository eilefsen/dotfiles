set tags+=/usr/include/tags

function GetTagFiles(tagname)
	" Cat all tagfiles into ripgrep which matches for exact tagname, then gets
	" the associated filename using cut. Finally, sort and remove duplicates
	let tagsource = join(tagfiles(), " ")
	let tagfiles = system("cat " . tagsource . " | rg ^" . a:tagname . "\\t | cut -f2 | sort | uniq")
	" Make an array out of the tagfiles
	return split(tagfiles, '\n')
endfunction

function AppendInclude(tagname, surround1, surround2)
	let tagfiles = GetTagFiles(a:tagname)
	if empty(tagfiles)
		return 0
	endif

	func! s:appendToBuffer(idx) closure
		let tfile = tagfiles[a:idx]
		let toAppend = a:surround1 . tfile . a:surround2
		let search_result = search('^' . toAppend . '$', 'n')
		if !search_result
			:call append(0, toAppend)
		else
			echo tfile . ' is already included, line: ' . search_result 
		endif
	endfunc
	:if len(tagfiles) == 1
	:call s:appendToBuffer(0)
	return 1
	:endif

	func! s:selectedTag(id, result) closure
		:call s:appendToBuffer(a:result-1)
	endfunc

	call popup_menu(tagfiles, #{
				\ callback: function('s:selectedTag'),
				\ })
	return 1
endfunction

function TagIncludeC(tagname)
	set tags-=/usr/include/tags
	let first_append = AppendInclude(a:tagname,"#include \"",'"')

	let old_tags_option=&tags
	:if first_append != 0
	return
	:endif

	set tags=/usr/include/tags
	:if !AppendInclude(a:tagname,"#include <",'>')
	echo 'No tags found for "' . a:tagname . '"'
	:endif


	let &tags = old_tags_option
	set tags+=/usr/include/tags
endfunction

function TagInclude(name)
	let n = shellescape(a:name)
	if &ft == "c"
		return TagIncludeC(n)
	else
		echo "No TagInclude function available for filetype: " . &ft
	endif
endfunction

command -nargs=1 TagInclude :call TagInclude(<q-args>)
command TagIncludeAtPoint :call TagInclude(expand('<cword>'))

nnoremap <Leader>ti <Cmd>TagIncludeAtPoint<CR>


" vim:foldmethod=marker:foldlevel=0
