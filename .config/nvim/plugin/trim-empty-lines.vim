function TrimEndLines()
  let save_cursor = getpos('.')
  silent! %s#\($\n\s*\)\+\%$##
  call setpos('.', save_cursor)
endfunction

" Group: TrimEndLines
"-------------------------------------------------- {{{
augroup TrimEndLines
  autocmd!
  autocmd BufWritePre *.clj,*.cljs,*.fnl,*.js,*.ts,*.tsx call TrimEndLines()
augroup END "}}}
