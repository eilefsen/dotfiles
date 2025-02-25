let &l:tags = &g:tags
for gp in g:globalIncludePathsC
	let &l:tags .= ',' .. gp .. '/tags'
endfor
