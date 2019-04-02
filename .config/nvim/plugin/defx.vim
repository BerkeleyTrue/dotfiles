" Configs
"++++++++++++++++++++++++++++++++++++++++++++++++++ {{{

" Symbols{{{
call defx#custom#column('filename', {
  \ 'directory_icon': '▸',
  \ 'opened_icon': '▾',
  \ 'min_width': 32,
  \ 'max_width': 32,
  \ })

call defx#custom#column('mark', {
  \ 'readonly_icon': '✗',
  \ 'selected_icon': '✓',
  \ })

let g:defx_git#indicators = {
  \ 'Modified'  : '✹',
  \ 'Staged'    : '✚',
  \ 'Untracked' : '✭',
  \ 'Renamed'   : '➜',
  \ 'Unmerged'  : '═',
  \ 'Ignored'   : '☒',
  \ 'Deleted'   : '✖',
  \ 'Unknown'   : '?'
  \ }
"}}}

"}}}


" Func: Tab_Id
"-------------------------------------------------- {{{
let s:tab_id_max = 0
function! s:tab_id()
  if ! exists('t:defx_tab_id')
    let s:tab_id_max = s:tab_id_max + 1
    let t:defx_tab_id = s:tab_id_max
  endif
  return t:defx_tab_id
endfunction " }}}

" Func: DefxExplorer
"-------------------------------------------------- {{{
function! DefxExplorer(...)
  let l:dir = a:0 >= 1 ? a:1 : './'
  let l:cmd = join([
    \ 'Defx',
    \ '-toggle',
    \ '-split=vertical',
    \ '-winwidth=32',
    \ '-direction=topleft',
    \ '-columns=icons:git:mark:filename',
    \ '-buffer-name=',
    \], ' ')
  execute l:cmd . s:tab_id() . ' ' . l:dir
endfunction
"}}}
"
" Func: DefxSearch
"-------------------------------------------------- {{{
function! DefxSearch(search, dir)

  let l:cmd = join([
    \ 'Defx',
    \ '-toggle',
    \ '-search=' . a:search,
    \ '-split=vertical',
    \ '-winwidth=32',
    \ '-direction=topleft',
    \ '-columns=icons:git:mark:filename',
    \ '-buffer-name=',
    \], ' ')
  execute l:cmd . s:tab_id() . ' ' . a:dir
endfunction "}}}

" Func: DefxChangeRoot
"-------------------------------------------------- {{{
function! DefxChangeRoot()
  let l:isDir = defx#is_directory()

  if (!l:isDir)
    return
  endif

  call defx#call_action('yank_path')
  call defx#call_action('cd', [@0])
endfunction
"}}}

" Keymaps
"++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
nnoremap <silent>zet :call DefxExplorer()<CR>
nnoremap <silent>zef :cal DefxSearch(expand('%:p'), getcwd())<CR>
"}}}

function! s:defx_settings() "{{{
  nnoremap <silent><buffer><expr><CR>     defx#is_directory() ? defx#do_action('open_or_close_tree') : defx#do_action('drop')
  nnoremap <silent><buffer><expr><Space>  defx#do_action('open_or_close_tree')
  nnoremap <silent><buffer><expr> c       defx#do_action('copy')
  nnoremap <silent><buffer><expr> m       defx#do_action('move')
  nnoremap <silent><buffer><expr><C-v>    defx#do_action('drop', 'vsplit')
  nnoremap <silent><buffer><expr> a       defx#do_action('new_file')
  nnoremap <silent><buffer><expr> A       defx#do_action('new_directory')
  nnoremap <silent><buffer><expr> D       defx#do_action('remove')
  nnoremap <silent><buffer><expr> r       defx#do_action('rename')
  nnoremap <silent><buffer><expr> yy      defx#do_action('yank_path')
  nnoremap <silent><buffer><expr> .       defx#do_action('toggle_ignored_files')
  nnoremap <silent><buffer><expr> p       defx#do_action('cd', ['..'])
  nnoremap <silent><buffer> cr            :call DefxChangeRoot()<CR>
  nnoremap <silent><buffer><expr><Tab>    defx#do_action('toggle_select')
  nnoremap <silent><buffer><expr> j       line('.') == line('$') ? 'gg' : 'j'
  nnoremap <silent><buffer><expr> k       line('.') == 1 ? 'G' : 'k'
  nnoremap <silent><buffer><expr> R       defx#do_action('redraw')
  nnoremap <silent><buffer><expr> cd      defx#do_action('change_vim_cwd')
endfunction
"}}}

augroup DEFX "{{{
  autocmd FileType defx call <SID>defx_settings()
  autocmd FileType defx setlocal spell!
augroup END "}}}
