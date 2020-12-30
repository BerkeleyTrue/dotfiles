" ==================================================
" General Key Bindings
" ================================================== {{{


" Folding {{{


" function! FormatFoldText() " {{{
"   " get the line beginning the fold
"   let l:line = getline(v:foldstart)

"   " get the left column length?
"   let l:nucolwidth = &foldcolumn + &number * &numberwidth
"   " get the window with minus the left column
"   let l:windowwidth = winwidth(0) - l:nucolwidth - 3
"   " get the number of lines to fold
"   let l:foldedlinecount = v:foldend - v:foldstart

"   " substitute tabs into spaces
"   let l:onetab = strpart('          ', 0, &tabstop)
"   let l:line = substitute(l:line, '\t', l:onetab, 'g')

"   " grab the first n characters of the line
"   " where n is the window width minus the number of folded lines minus 2
"   let l:line = strpart(l:line, 0, l:windowwidth - len(l:foldedlinecount) - 5)

"   return l:line . '…' . l:foldedlinecount . ' lines folded…>>>'
" endfunction " }}}
" set foldtext=FormatFoldText()
" Folding }}}
"
" Buffers {{{
command! BufDeleteHidden call DeleteHiddenBuffers()
command! Bdh call DeleteHiddenBuffers()

function! DeleteHiddenBuffers() " {{{
  let l:listOfBuffs=[]
  let l:closed = 0
  " map over 1 to the number of tab pages (num or windows), extend (append)
  " the buffer list of the current page
  " in essense: create a list of all the buffers
  call map(range(1, tabpagenr('$')), 'extend(l:listOfBuffs, tabpagebuflist(v:val))')
  " filter: for 1 to the last buffer (bufnr('$')),
  "   if exists and
  "   if buffer number does not exist in listOfBuffs (buffer list) and
  "   if buffer type is empty (normal buffer)
  for l:buf in filter(range(1, bufnr('$')), "bufexists(v:val) && index(l:listOfBuffs, v:val) == -1 && empty(getbufvar(v:val, '&buftype'))")
    " if buffer is currently unmodified (mod == 0)
    if getbufvar(l:buf, '&mod') == 0
      " close buffer and remove everything about it
      silent execute 'bwipeout' l:buf
      let l:closed += 1
    endif
  endfor
  echo 'Closed '.l:closed.' hidden buffers'
endfunction " }}}
" Buffers }}}

" -- End General Key Bindings -- }}}
"
" ==================================================
" Plugin Specific mappings
" ================================================== {{{

" vim-bbye
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
" closes the currently open buffer
nnoremap <Leader>q :Bdelete<CR>
" }}}

" Ultisnips/Coc
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
function! GoDownOnPum()
  " If Pop Up Menu (pum) is open go down
  " else insert tab
  return pumvisible() ? "\<c-n>" : "\<tab>"
endfunction
function! GoUpOnPum()
  " If pum open go up
  " else insert tab
  return pumvisible() ? "\<c-p>" : "\<tab>"
endfunction

function! ExpandOnEnter()
  " Try to expand snippet
  let l:snippet = UltiSnips#ExpandSnippet()
  " If expand successful return snippet
  if g:ulti_expand_res > 0
    return l:snippet
  endif

  " If popup menu visible
  " this will confirm current
  " selection and exit pum without
  " adding a new line
  if pumvisible()
    return "\<c-y>"
  endif
  " Otherwise call delimitMate expand on <CR>
  " this will cause surrounds to expand if appropriate
  " otherwise it just returns a <CR>
  return delimitMate#ExpandReturn()
endfunction

" On enter, check for snippet and expand
" or return <CR>
inoremap <silent><CR> <C-R>=ExpandOnEnter()<CR>
" On tab with dropdown go down
" else insert tab
inoremap <silent><tab> <C-r>=GoDownOnPum()<cr>
inoremap <silent><s-tab> <C-r>=GoUpOnPum()<cr>
" Duplicate ultisnips s/n mode bindings for tab
snoremap <silent><tab> <Esc>:call UltiSnips#ExpandSnippet()<cr>
xnoremap <silent><tab> :call UltiSnips#SaveLastVisualSelection()<cr>gvs
inoremap <silent><C-j> <C-R>=UltiSnips#JumpForwards()<cr>
snoremap <silent><C-j> <Esc>:call UltiSnips#JumpForwards()<cr>
" }}}

" Emmet
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
" call along side EmmetInstall
function! AddEmmetMappings()
  " buffer tells vim to only use map in buffer it is defined
  " we recur so vim will eval the output
  imap <buffer> <leader><tab> <plug>(emmet-expand-abbr)
  " self closing tag
  " foo/<leader><tab> => <foo />
  " does not work yet with non-closing html5 tags like <img>
  inoremap <buffer> /<leader><tab> <esc>:call emmet#expandAbbr(0,"")<cr>h:call emmet#splitJoinTag()<cr>wwi
endfunction
" }}}
"" -- End Plugin Key Bindings -- }}}
