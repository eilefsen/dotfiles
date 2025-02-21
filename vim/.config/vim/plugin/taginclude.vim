vim9script

class TupleFindTagFilenames
	public var filenames: list<string>
	public var isGlobal: bool
	public var error: bool
	def new(filenames: list<string>, isGlobal: bool, error: bool = false)
		this.filenames = filenames
		this.isGlobal = isGlobal
		this.error = error
	enddef
	def newError()
		this.filenames = []
		this.isGlobal = false
		this.error = true
	enddef
endclass

def g:FindTagFilenames(tagname: string, globalTagsFiles: list<string> = []): TupleFindTagFilenames
	# This might be a bug, but tagrelative for some reason gives us *absolute* filenames,
	# this option must be set for taglist() to behave as expected
	var oldtr = &tagrelative
	set tagrelative
	var tl = taglist($"^{tagname}$")->map((key, val) => val.filename)
	&tagrelative = oldtr # Set tagrelative back to what it was

	var gp = join(globalTagsFiles, '\|')
	gp = substitute(gp, '/', '\\/', 'g')
	var local = matchstrlist(tl, '^\(' .. gp  .. '/.*\)\@!.*$')->map((_, val) => val.text)

	if !empty(local)
		var curfile = expand('%:p:h')
		# Filter out choices which match the current file,
		# then map with relative paths
		var filenames = local->filter((_, val) => expand('%') != val)
		filenames->map((_, val): string => system($"realpath -s --relative-to={curfile} {val}")->trim())
		return TupleFindTagFilenames.new(filenames, false)
	elseif !!globalTagsFiles
		var filenames = matchstrlist(tl, '^' .. gp .. '\/\zs.*\ze$')->map((_, val) => val.text)
		return TupleFindTagFilenames.new(filenames, true)
	endif

	echom $"Could not find a file to include for {tagname}"
	return TupleFindTagFilenames.newError()
enddef

def AppendInclude(filenames: list<string>, surround1: string, surround2: string): bool
	if !filenames
		return false
	endif

	def AppendToBuffer(idx: number)
		var tfile = filenames[idx]

		var toAppend = surround1 .. tfile .. surround2
		var search_result = search($"^{toAppend}$", 'n')
		if !search_result
			append(0, toAppend)
		else
			echom $"'{tfile}' is already included, line: {search_result}"
		endif
	enddef

	if len(filenames) == 1
		AppendToBuffer(0)
		return true
	endif

	call popup_menu(filenames, {'callback': (id, result) => AppendToBuffer(result - 1)})
	return true
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

def TagIncludeC(tagname: string, onlyHeaders: bool): void
	var tuple = g:FindTagFilenames(tagname, g:globalIncludePaths)
	if tuple.error
		return
	endif
	var filenames = tuple.filenames
	if onlyHeaders
		filenames = matchstrlist(filenames, '\<\w\+\(\.h\|\.hpp\).*\>')->map((_, val): string => val.text)
	else
		filenames = filenames->sort(SortHeadersFirst)
	endif

	if tuple.isGlobal
		AppendInclude(filenames, '#include <', '>')
	else
		AppendInclude(filenames, '#include "', '"')
	endif
enddef

def TagInclude(name: string): void
	var n = shellescape(name)
	if &ft == "c" || &ft == "cpp"
		TagIncludeC(name, false)
	else
		echom $"No TagInclude function available for filetype: {&ft}" 
	endif
enddef

# List of paths to consider for global tags. Path cannot have a trailing slash
g:globalIncludePaths = ['/usr/include']
&tags =  &tags .. join(g:globalIncludePaths->copy()->map((_, val) => $",{val}/tags"), ',')

command -nargs=1 TagInclude :call TagInclude(<q-args>)
command TagIncludeAtPoint :call TagInclude(expand('<cword>'))
nnoremap <Leader>ti <Cmd>TagIncludeAtPoint<CR>

# vim:foldmethod=marker:foldlevel=0
