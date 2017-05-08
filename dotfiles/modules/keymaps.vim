" ==================================================
" General Key Mappings {{{
" ==================================================
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
nnoremap <C-w>vr :vertical resize 90<CR>

" Tab navigation
nnoremap tj :tabnext<CR>
nnoremap tk :tabprev<CR>
nnoremap tn :tabnew<CR>:NERDTree<CR>

" H to move to the beginning of the line
noremap H ^
" L move to the end of the line
noremap L $
" same but in visual mode
vnoremap L g_
" }}}

" Alias Helptags to rbHelp, This makes way more sense to me and will be
" easier to remember
cnoreabbrev <expr> rebuildHelpFiles ((getcmdtype() is# ":" && getcmdline() is# "rebuildHelpFiles")?("Helptags"):("rebuildHelpFiles"))

  " Alias sudo overwrite
cnoreabbrev <expr> sudowrite ((getcmdtype() is# ":" && getcmdline() is# "sudowrite")?(":w !sudo tee %"):("sudowrite"))
" Alias command qq to q!
cnoreabbrev <expr> qq ((getcmdtype() is# ":" && getcmdline() is# "qq")?("q!"):("qq"))
" Alias :W to :w because I keep forgetting to release shift!
cnoreabbrev <expr> W ((getcmdtype() is# ":" && getcmdline() is# "W")?("w"):("W"))

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
map <S-k> <Nop>

" sort lines in visual mode
vnoremap <leader>s :sort<cr>

" insert new line on enter
nnoremap <cr> o<esc>

" in visual mode use gu to change casing
vnoremap u <nop>
vnoremap gu u

" uppercase word under the cursor while in insert mode
inoremap <leader>uw <esc>mzgUiw`za

" yank to end of line
nnoremap Y y$

" Throwing characters {{{
nnoremap z; mqA;<esc>`q
" Throw a comma on the end of the line
nnoremap z, mqA,<esc>`q
" Delete last character on the line
nnoremap zdl mqA<esc>x`q
" Move the current char to the end of the line
nnoremap zl :let @z=@"<cr>x$p:let @"=@z<cr>
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

  return l:line . '…' . l:foldedlinecount . ' lines folded…}}}'
endfunction " }}}
set foldtext=FormatFoldText()
" }}}
" Plugin Specific mappings {{{
" ==================================================

" nerdtree {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" Alias the command `nerd` to toggle nerdtree
cnoreabbrev <expr> nerd ((getcmdtype() is# ":" && getcmdline() is# "nerd")?("NERDTreeToggle"):("nerd"))
nnoremap <leader>nt :NERDTreeToggle<cr>
"}}}

" vim-bbye {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" closes the currently open buffer
nnoremap <Leader>q :Bdelete<CR>
" }}}

" easymotion {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:EasyMotion_smartcase = 1
" motion
nmap s <Plug>(easymotion-s2)
nmap t <Plug>(easymotnion-t2)
" search
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)

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
" }}}

" Elm {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <leader>el :ElmEvalLine<CR>
vnoremap <leader>es :<C-u>ElmEvalSelection<CR>
nnoremap <leader>em :ElmMakeCurrentFile<CR>
" }}}

" Ultisnips/Neoplete {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
function! GoDownOnPlum()
  " If Pop Up Menu (pum) is open go down
  " else insert tab
  return pumvisible() ? "\<c-n>" : "\<tab>"
endfunction
function! GoUpOnPlum()
  " If pum open go up
  " else insert tab
  return pumvisible() ? "\<c-p>" : "\<tab>"
endfunction

function! ExpandOnEnter()
  " Try to expand snippet
  let l:snippet = UltiSnips#ExpandSnippetOrJump()
  " If expand successful return snippet
  if g:ulti_expand_or_jump_res > 0
    return l:snippet
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
inoremap <silent><tab> <C-r>=GoDownOnPlum()<cr>
inoremap <silent><s-tab> <C-r>=GoUpOnPlum()<cr>
" Duplicate ultisnips s/n mode bindings for tab
snoremap <silent><tab> <Esc>:call UltiSnips#ExpandSnippet()<cr>
xnoremap <silent><tab> :call UltiSnips#SaveLastVisualSelection()<cr>gvs
" }}}

" Emmet {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
imap <leader><tab> <plug>(emmet-expand-abbr)
" self closing tag
" foo/<leader><tab> => <foo />
" does not work yet with non-closing html5 tags like <img>
imap /<leader><tab> <esc>:call emmet#expandAbbr(0,"")<cr>h:call emmet#splitJoinTag()<cr>wwi
" }}}
"
" }}}
