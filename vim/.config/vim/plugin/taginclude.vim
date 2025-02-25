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

	var local = !!globalTagsFiles ? matchstrlist(tl, '^\(' .. gp  .. '/.*\)\@!.*$')->map((_, val) => val.text) : tl

	if !!local
		local = uniq(local)
		var curdir = expand('%:p:h')
		# Filter out choices which match the current file,
		# then map with relative paths
		var filenames = local->filter((_, val) => expand('%') != val)
		filenames->map((_, val): string => system($"realpath -s --relative-to={curdir} {val}")->trim())
		return TupleFindTagFilenames.new(filenames, false)
	elseif !!globalTagsFiles
		var filenames = matchstrlist(tl, '^' .. gp .. '\/\zs.*\ze$')->map((_, val) => val.text)
		if !!filenames
			filenames = uniq(filenames)
			return TupleFindTagFilenames.new(filenames, true)
		endif
	endif

	echom $"Could not find a file to include for {tagname}"
	return TupleFindTagFilenames.newError()
enddef



def AppendInclude(filenames: list<string>, AppendFunc: func): bool
	if !filenames
		return false
	endif

	if len(filenames) == 1
		AppendFunc(filenames[0])
		return true
	endif

	call popup_menu(filenames, {
		'callback': (id, result) => AppendFunc(filenames[result - 1])
	})
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

def AppendFuncC(isGlobal: bool, allowDuplicate: bool = false): func
	return (filename: string) => {
		var toAppend = isGlobal ? '#include <' .. filename .. '>' : '#include "' .. filename .. '"'
		if allowDuplicate
			append(0, toAppend)
		endif
		var search_result = search($"^{toAppend}$", 'n')
		if !search_result
			append(0, toAppend)
		else
			if !filename
				echom $"'{toAppend}' already exists, at line: {search_result}"
			else
				echom $"'{filename}' is already included, line: {search_result}"
			endif
		endif
	}
enddef

def AppendFuncTypescript(tagname: string): func
	return (filename: string) => {
		var rx = $"^import \{.*\} from \\(\'{filename}\'\\|\"{filename}\"\\);\\?$"
		var res = matchbufline(bufnr('%'), rx, 1, '$')
		if !!res
			var existingImport = matchstr(res[0].text, '^import {\zs.*\ze} from .*$')
			var toAppend = $"import \{ {existingImport->trim()}, {tagname} \} from \'{filename}\';"
			setline(res[0].lnum, toAppend)
		else
			var toAppend = $"import \{ {tagname} \} from \'{filename}\';"
			append(0, toAppend)
		endif
	}
enddef

def TagIncludeC(tagname: string, onlyHeaders: bool): void
	var tuple = g:FindTagFilenames(tagname, g:globalIncludePathsC)
	if tuple.error
		return
	endif
	var filenames = tuple.filenames
	if onlyHeaders
		filenames = matchstrlist(filenames, '^.*\(\.h\|\.hpp\)$')->map((_, val): string => val.text)
	else
		filenames = filenames->sort(SortHeadersFirst)
	endif
	AppendInclude(filenames, AppendFuncC(tuple.isGlobal))
enddef

def TagIncludeTypescript(tagname: string): void
	var tuple = g:FindTagFilenames(tagname, ['node_modules'])
	if tuple.error
		return
	endif
	var filenames = tuple.filenames
	AppendInclude(filenames, AppendFuncTypescript(tagname))
enddef

def TagInclude(name: string): void
	var n = shellescape(name)
	if &ft == "c" || &ft == "cpp"
		TagIncludeC(name, true)
	elseif &ft == "javascript" || &ft == "typescript" || &ft == 'javascriptreact' || &ft == 'typescriptreact'
		TagIncludeTypescript(name)
	else
		echom $"No TagInclude function available for filetype: {&ft}" 
	endif
enddef

command -nargs=1 TagInclude :call TagInclude(<q-args>)
command TagIncludeAtPoint :call TagInclude(expand('<cword>'))
nnoremap <Leader>ti <Cmd>TagIncludeAtPoint<CR>

# vim:foldmethod=marker:foldlevel=0
