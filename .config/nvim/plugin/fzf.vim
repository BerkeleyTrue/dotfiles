" fzf.vim
"++++++++++++++++++++++++++++++++++++++++++++++++++

" Func: Fzf_ag_with_path
"-------------------------------------------------- {{{
function! s:Fzf_ag_with_path(...)
  let query = a:0 >= 1 ? a:1 : '^(?=.)'
  let path = a:0 >= 2 ? a:2 : './'
  let command = fzf#shellescape(query) . ' ' . fzf#shellescape(path)
  call fzf#vim#ag_raw(command)
endfunction "}}}

" let g:fzf_layout = {
"       \ 'window': 'new | wincmd J | resize 1 | call animate#window_percent_height(0.5)'
"       \ }

" Group: Fzf
"-------------------------------------------------- {{{
augroup Fzf
  autocmd!
  autocmd VimEnter * command! -nargs=* -complete=dir Ag call s:Fzf_ag_with_path(<f-args>)
augroup END "}}}
