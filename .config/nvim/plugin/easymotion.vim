" easymotion
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:EasyMotion_smartcase = 1
let g:EasyMotion_startofline = 0
" motion
nmap s <Plug>(easymotion-s2)
nmap t <Plug>(easymotion-t2)
" search
map  / <Plug>(incsearch-easymotion-/)
omap / <Plug>(easymotion-tn)

" ---
" Func: Combine fuzzy and easymotion plugins for incsearch
" --- {{{
function! s:config_easyfuzzymotion(...) abort
  return extend(copy({
  \   'converters': [incsearch#config#fuzzy#converter()],
  \   'modules': [incsearch#config#easymotion#module()],
  \   'keymap': {"\<CR>": '<Over>(easymotion)'},
  \   'is_expr': 0,
  \   'is_stay': 1
  \ }), get(a:, 1, {}))
endfunction " }}}

noremap <silent><expr> <leader>/ incsearch#go(<SID>config_easyfuzzymotion())

" These `n` & `N` mappings are options. You do not have to map `n` & `N` to EasyMotion.
" Without these mappings, `n` & `N` works fine. (These mappings just provide
" different highlight method and have some other features )
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)
" Search in line
" map <Leader>l <Plug>(easymotion-lineforward)
" map <Leader>j <Plug>(easymotion-j)
" map <Leader>k <Plug>(easymotion-k)
" map <Leader>h <Plug>(easymotion-linebackward)
" map <Leader>w <Plug>(easymotion-w)
" map <Leader>W <Plug>(easymotion-W)
" map <Leader>b <Plug>(easymotion-b)
" map <Leader>B <Plug>(easymotion-B)
"
nmap f <Plug>(easymotion-fl)
vmap f <Plug>(easymotion-fl)
omap f <Plug>(easymotion-fl)

nmap F <Plug>(easymotion-Fl)
vmap F <Plug>(easymotion-Fl)
omap F <Plug>(easymotion-Fl)

nmap t <Plug>(easymotion-tl)
vmap t <Plug>(easymotion-tl)
omap t <Plug>(easymotion-tl)

nmap T <Plug>(easymotion-Tl)
vmap T <Plug>(easymotion-Tl)
omap T <Plug>(easymotion-Tl)
