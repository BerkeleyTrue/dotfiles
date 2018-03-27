setlocal foldmethod=marker foldmarker={{{,}}}
augroup VimReloadG
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC | echo 'init reloaded' | redraw | call AddHighlight() | AirlineRefresh
augroup END

