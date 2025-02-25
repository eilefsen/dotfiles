" Go to the previous location and stay in the quickfix window
nnoremap <buffer> K :cprev<CR>zz<C-w>w
nnoremap <buffer> <C-p> :cprev<CR>zz<C-w>w
" Go to the next location and stay in the quickfix window
nnoremap <buffer> J :cnext<CR>zz<C-w>w
nnoremap <buffer> <C-n> :cnext<CR>zz<C-w>w

" Make the quickfix list modifiable
nnoremap <buffer> <leader>u :set modifiable<CR>

" Save the changes in the quickfix window
nnoremap <buffer> <leader>w :cgetbuffer<CR>:cclose<CR>:copen<CR>

" Begin the search and replace
nnoremap <buffer> <leader>r :cdo s/// \| update<C-Left><C-Left><Left><Left><Left>
