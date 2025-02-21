vim9script

def ParseTagFile(tagname: string, tagsfile: string): list<string>
	var grepcmd = !!system('command -v rg') ? 'rg' : 'grep'
	var tfiles = split(system($"cat {tagsfile} | {grepcmd} \'^{tagname}\\b\' | cut -f2 | sort | uniq"), '\n')

	return tfiles->map((_, val): string => $"{val} {tagsfile}")
enddef

# Cat all tagfiles into ripgrep which matches for exact tagname, then gets
# the associated filename using cut. Finally, sort and remove duplicates
# Flatten the result.
def GetTagFiles(tagname: string): list<string>
	return tagfiles()->map((_, val) => ParseTagFile(tagname, val))->flattennew()
enddef

def GetRelativeIncludePath(tagsfile: string, includefile: string): string
	var tagsfiledirabs = system($"dirname $(realpath {tagsfile})")
	var fileabs = expand('%:p:h')
	var includeabs = $"{trim(tagsfiledirabs)}/{includefile}"
	var relpath = system($"realpath -s --relative-to={fileabs} {includeabs}")
	return relpath
enddef

def AppendInclude(includefiles: list<string>, surround1: string, surround2: string, userelative: bool): bool
	if !includefiles
		return 0
	endif

	def AppendToBuffer(idx: number)
		var splitfile = split(includefiles[idx], ' ')
		var tfile = splitfile[0]
		var tagspath = splitfile[1]

		if userelative
		   tfile = trim(GetRelativeIncludePath(tagspath, tfile))
		endif

		var toAppend = surround1 .. tfile .. surround2
		var search_result = search($"^{toAppend}$", 'n')
		if !search_result
			append(0, toAppend)
		else
			echo $"'{tfile}' is already included, line: {search_result}"
		endif
	enddef

	if len(includefiles) == 1
		AppendToBuffer(0)
		return 1
	endif

	var menuitems = copy(includefiles)->map((_, val): string => split(val, ' ')[0])

	call popup_menu(menuitems, {'callback': (id, result) => AppendToBuffer(result - 1)})
	return 1
enddef

def SortHeadersFirst(a: string, b: string): number
	var ah = !!a->matchstr('.*\(\.hpp\|h\|H\)')
	var bc = !!b->matchstr('.*\(\.cpp\|c\|C\)')
	var ac = !!a->matchstr('.*\(\.cpp\|c\|C\)')
	var bh = !!b->matchstr('.*\(\.hpp\|h\|H\)')

	if ah && bc
		return -1
	elseif ac && bh
		return 1
	else
		return 0
	endif
enddef

def TagIncludeC(tagname: string, filter_header: bool): void
	set tags-=/usr/include/tags
	var tagfiles = GetTagFiles(tagname)
	if filter_header
		tagfiles = matchstrlist(tagfiles, '\<\w\+\(\.h\|\.hpp\).*\>')->map((_, val): string => val.text)
	else
		tagfiles = tagfiles->sort(SortHeadersFirst)
	endif

	var first_append = AppendInclude(tagfiles, '#include "', '"', true)
	if first_append
		return
	endif

	var old_tags_option = &tags
	set tags=/usr/include/tags
	tagfiles = GetTagFiles(tagname)
	if filter_header
		tagfiles = matchstrlist(tagfiles, '\<\w\+\(\.h\|\.hpp\)\>')->map((_, val): string => val.text)
	endif
	if !AppendInclude(tagfiles, '#include <', '>', false)
		echo $"No tags found for {tagname}"
	endif

	&tags = old_tags_option
	set tags+=/usr/include/tags
enddef

def TagInclude(name: string): void
	var n = shellescape(name)
	if &ft == "c" || &ft == "cpp"
		TagIncludeC(name, false)
	else
		echom $"No TagInclude function available for filetype: {&ft}" 
	endif
enddef

defcompile

set tags+=/usr/include/tags
command -nargs=1 TagInclude :call TagInclude(<q-args>)
command TagIncludeAtPoint :call TagInclude(expand('<cword>'))
nnoremap <Leader>ti <Cmd>TagIncludeAtPoint<CR>

# vim:foldmethod=marker:foldlevel=0
