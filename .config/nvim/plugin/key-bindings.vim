" ==================================================
" General Key Bindings
" ================================================== {{{
" All key mappings should go here to easily located
" key mapping conflicts
" This should include commands

" Vim leader key to ,
let g:mapleader=','

" Map jj and kk to escape and move in insert mode
inoremap jj <ESC>
inoremap kk <ESC>

" Remove all hightlighting
nnoremap <Leader>rh :noh<cr>

" ,ev to edit vim, ,sv to source vim
nmap <silent> <leader>ev :edit $MYVIMRC<CR>
nmap <silent> <leader>sv :source $MYVIMRC<CR>

" Turn of default <C-j> binding
let g:BASH_Ctrl_j = 'off'
" Navigation {{{
" Window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
" NOTE: If <C-h> binding is not working could be due
" to neovim issue see: neovim/neovim/issues/2048
" The solution is to enter the following into
" the bash command line
"
" infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti && tic $TERM.ti
nnoremap <C-w>vh <C-w>t<C-w>K
nnoremap <C-w>hv <C-w>t<C-w>H
" resize tab to fit scripts
nnoremap <C-w>vr :call animate#window_absolute_width(90)<cr>

" H to move to the beginning of the line
noremap H ^
" L move to the end of the line
noremap L $
" same but in visual mode
vnoremap L g_
" }}}

" easily create aliases
fun! SetupCommandAlias(from, to)
  exec 'cnoreabbrev <expr> '.a:from
    \ .' ((getcmdtype() is# ":" && getcmdline() is# "'.a:from.'")'
    \ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfun
" Alias Helptags to rbHelp, This makes way more sense to me and will be
" easier to remember
call SetupCommandAlias('RebuildHelpFiles', 'Helptags')

" Alias sudo overwrite
call SetupCommandAlias('sudowrite', 'w !sudo tee %')
" Alias command qq to q!
call SetupCommandAlias('qq', 'q!')
" Alias :W to :w because I keep forgetting to release shift!
call SetupCommandAlias('W', 'w')

" Make OSX alt key compatible with vim
" Now alt will work as expected with vim-move
nmap ˙ <A-h>
nmap ∆ <A-j>
nmap ˚ <A-k>
nmap ¬ <A-l>
imap ˙ <A-h>
imap ∆ <A-j>
imap ˚ <A-k>
imap ¬ <A-l>
vmap ˙ <A-h>
vmap ∆ <A-j>
vmap ˚ <A-k>
vmap ¬ <A-l>

" Unbind K
" I never use this and I keep hitting K accidentally
" note: vim-fireplace trys to bind to K
" map <S-k> <Nop>
" I keep hitting this on failed :q's
" Open commandline instead and wait for further commands
" use <C-f> while in command mode to access this instead
nnoremap q: :

" sort lines in visual mode
vnoremap <leader>s :sort<cr>

" insert new line on enter
nnoremap <cr> o<esc>

" in visual mode use gu to change casing
vnoremap u <nop>
vnoremap gu u

" In normal mode remove ( text object motion. I keep hiting this accidentally
" and never use it intentially
nnoremap ( <nop>
nnoremap ) <nop>

" in normal mode, run quick macro
" use gQ to enter Exmode instead
nnoremap Q @q

" uppercase word under the cursor while in insert mode
inoremap <leader>uw <esc>mzgUiw`za
vnoremap <leader>uw <esc>mzgUiw`zv

" yank to end of line
nnoremap Y y$

" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv
" reselect last paste
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

" Throwing characters {{{
nnoremap z; mqA;<esc>`q:delmarks q<cr>
" Throw a comma on the end of the line
nnoremap z, mqA,<esc>`q:delmarks q<cr>
" Delete last character on the line
nnoremap zdl mqA<esc>x`q:delmarks q<cr>
" Move the current char to the end of the line
nnoremap zl :let @z=@"<cr>x$p:let @"=@z<cr>
" Move line to the end of the next line
" useful for move a comment above a line behind it
nnoremap zJ ddpkJ
" }}}


" Highlight all instances of word under cursor, when idle.
" z/ to toggle highlighting on/off.
function! AutoHighlightToggle() " {{{

  let @/ = ''
  if exists('#auto_highlight')
    au! auto_highlight
    augroup! auto_highlight
    setl updatetime=4000
    echo 'Highlight current word: off'
    return 0
  else
    augroup auto_highlight
      au!
      au CursorHold * let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
    augroup end
    setl updatetime=500
    echo 'Highlight current word: ON'
    return 1
  endif
endfunction
nnoremap z/ :if AutoHighlightToggle()<Bar>set hls<Bar>endif<CR>
" }}}

" Folding {{{

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

function! FormatFoldText() " {{{
  " get the line beginning the fold
  let l:line = getline(v:foldstart)

  " get the left column length?
  let l:nucolwidth = &foldcolumn + &number * &numberwidth
  " get the window with minus the left column
  let l:windowwidth = winwidth(0) - l:nucolwidth - 3
  " get the number of lines to fold
  let l:foldedlinecount = v:foldend - v:foldstart

  " substitute tabs into spaces
  let l:onetab = strpart('          ', 0, &tabstop)
  let l:line = substitute(l:line, '\t', l:onetab, 'g')

  " grab the first n characters of the line
  " where n is the window width minus the number of folded lines minus 2
  let l:line = strpart(l:line, 0, l:windowwidth - len(l:foldedlinecount) - 5)

  return l:line . '…' . l:foldedlinecount . ' lines folded…>>>'
endfunction " }}}
set foldtext=FormatFoldText()
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

" easymotion
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
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
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
map <Leader>w <Plug>(easymotion-w)
map <Leader>W <Plug>(easymotion-W)
map <Leader>b <Plug>(easymotion-b)
map <Leader>B <Plug>(easymotion-B)
map <Leader>f <Plug>(easymotion-fl)
"
vmap f <Plug>(easymotion-fl)
omap f <Plug>(easymotion-fl)
vmap t <Plug>(easymotion-tl)
omap t <Plug>(easymotion-tl)
vmap F <Plug>(easymotion-Fl)
omap F <Plug>(easymotion-Fl)
vmap T <Plug>(easymotion-Tl)
omap T <Plug>(easymotion-Tl)
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
inoremap <CR> <C-R>=ExpandOnEnter()<CR>
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
"
" NrrwRgn
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
command! -nargs=* -bang -range -complete=filetype NN
  \ :<line1>,<line2> call nrrwrgn#NrrwRgn('',<q-bang>)
  \ | set filetype=<args>
" }}}
"
" Ale
"++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
nmap <leader>ak <Plug>(ale_previous_wrap)
nmap <leader>aj <Plug>(ale_next_wrap)
"}}}
"" -- End Plugin Key Bindings -- }}}
