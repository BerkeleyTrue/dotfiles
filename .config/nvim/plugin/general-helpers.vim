" ==================================================
" General Autocmds
" ==================================================

" Func: Disable spell check on camelCase
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
function! s:DisableSpellOnCamelCase()
  syntax match CamelCase /\<[A-Z][a-z]\+[A-Z].\{-}\>/ contains=@NoSpell transparent
  syntax cluster Spell add=CamelCase
endfunction " }}}

" Func: PreserveLastEditLocation
"-------------------------------------------------- {{{
function! s:PreserveLastEditLocation()
  if &filetype =~# 'gitcommit'
    return
  endif

  " last mark is at '"
  " so if the last mark is more than the first line (>0)
  " and less then the files last line
  if line("'\"") > 0 && line("'\"") <= line('$')
    " g'"|g`"" Jump to the last known position, but don't change the jumplist
    " zv open folds enough to view cursor
    " zz center cursor line on screen
    execute 'normal! g`"zvzz'
  endif
endfunction "}}}

" Group: GeneralAu
"-------------------------------------------------- {{{
augroup GeneralAu
  autocmd!
  autocmd BufEnter,BufRead * call s:DisableSpellOnCamelCase()
  " Resize splits when the window is resized
  autocmd VimResized * exe "normal! \<c-w>="
  " Make vim open on the line you closed the buffer on
  autocmd BufReadPost * call s:PreserveLastEditLocation()

  " remove highlight after cursor stops moving
  autocmd cursorhold * set nohlsearch | let @/ = ""
  autocmd cursormoved * set hlsearch

augroup END " }}}
