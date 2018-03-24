" ==================================================
" Filetype Settings
" ==================================================

" General {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++

function! StartUp() " {{{
  if 0 == argc()
    if exists(':Files')
      " Open fzf files on startup
      Files
    elseif exists(':NERDTree')
      " Open NERDTree otherwise
      NERDTree
    endif
  end
endfunction " }}}

augroup GeneralGroup " {{{
  au!
  autocmd BufRead,BufNewFile * :hi SpellBad ctermbg=none ctermfg=none cterm=underline

  " On insert mode set absolute row numbers
  " On leave Return to relative row numbers
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &number | set relativenumber | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &number | set norelativenumber | endif

  " Open nerdTree if vim opened without a file in the buffer
  autocmd VimEnter * call StartUp()

  " Resize splits when the window is resized
  autocmd VimResized * exe "normal! \<c-w>="
  " Make vim open on the line you closed the buffer on
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \     execute 'normal! g`"zvzz' |
    \ endif
augroup END " }}}
" }}}
