" ==================================================
" Key Mappings
" ==================================================
" All key mappings should go here to easily located 
" key mapping conflicts
" This should include commands

" Vim leader key to ,
let mapleader=','

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

" Tab navigation
nnoremap tj :tabnext<CR>
nnoremap tk :tabprev<CR>
nnoremap tn :tabnew<CR>:NERDTree<CR>

" Alias Helptags to rbHelp, This makes way more sense to me and will be
" easier to remember
cnoreabbrev <expr> rebuildHelpFiles ((getcmdtype() is# ":" && getcmdline() is# "rebuildHelpFiles")?("Helptags"):("rebuildHelpFiles"))

  " Alias sudo overwrite
cnoreabbrev <expr> sudowrite ((getcmdtype() is# ":" && getcmdline() is# "sudowrite")?(":w !sudo tee %"):("sudowrite"))
" Alias command qq to q!
cnoreabbrev <expr> qq ((getcmdtype() is# ":" && getcmdline() is# "qq")?("q!"):("qq"))

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

" Highlight all instances of word under cursor, when idle.
" z/ to toggle highlighting on/off.
function! AutoHighlightToggle()
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

" ==================================================
" Plugin Specific mappings
" ==================================================

" nerdtree
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" Alias the command `nerd` to toggle nerdtree
cnoreabbrev <expr> nerd ((getcmdtype() is# ":" && getcmdline() is# "nerd")?("NERDTreeToggle"):("nerd"))

" vim-bbye
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" closes the currently open buffer
nnoremap <Leader>q :Bdelete<CR>

" easymotion
" ++++++++++++++++++++++++++++++++++++++++++++++++++
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

" Elm
" ++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <leader>el :ElmEvalLine<CR>
vnoremap <leader>es :<C-u>ElmEvalSelection<CR>
nnoremap <leader>em :ElmMakeCurrentFile<CR>

" Ultisnips/YouCompleteme
" ++++++++++++++++++++++++++++++++++++++++++++++++++
function! GoDownOnYCM()
  " If dropdown open go down
  " else insert tab
  return pumvisible() ? "\<C-n>" : "\<tab>"
endfunction

function! ExpandOnEnter()
  " Try to expand snippet
  let snippet = UltiSnips#ExpandSnippetOrJump()
  " If expand successful return snippet
  if g:ulti_expand_or_jump_res > 0
    return snippet
  endif
  " Otherwise return CR
  return "\<CR>"
endfunction

" On enter, check for snippet and expand
" or return <CR>
inoremap <CR> <C-R>=ExpandOnEnter()<CR>
" On tab with dropdown go down
" else insert tab
inoremap <silent><tab> <C-r>=GoDownOnYCM()<cr>

" Emmet
" ++++++++++++++++++++++++++++++++++++++++++++++++++
imap <leader><tab> <plug>(emmet-expand-abbr)
" self closing tag
" foo/<leader><tab> => <foo />
" does not work yet with non-closing html5 tags like <img>
imap /<leader><tab> <esc>:call emmet#expandAbbr(0,"")<cr>h:call emmet#splitJoinTag()<cr>wwi
