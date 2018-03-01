" ==================================================
" Filetype Settings
" ==================================================

" General {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" Ignore camelCase words when spell checking
fun! IgnoreCamelCaseSpell() " {{{
  syn match CamelCase /\<[A-Z][a-z]\+[A-Z].\{-}\>/ contains=@NoSpell transparent
  syn cluster Spell add=CamelCase
endfun " }}}

function! StartUp() " {{{
  if 0 == argc()
    NERDTree
  end
endfunction " }}}

augroup GeneralGroup " {{{
  au!
  autocmd BufRead,BufNewFile * :call IgnoreCamelCaseSpell()
  autocmd BufRead,BufNewFile * :hi SpellBad ctermbg=none ctermfg=none cterm=underline

  autocmd FocusLost * set nornu
  autocmd FocusGained * set rnu

  " On insert mode set absolute row numbers
  " On leave Return to relative row numbers
  autocmd InsertEnter * set nornu
  autocmd InsertLeave * set rnu

  " Open nerdtree if vim opened without a file in the buffer
  autocmd VimEnter * call StartUp()

  " Resize splits when the window is resized
  autocmd VimResized * exe "normal! \<c-w>="
  autocmd FileType vim setlocal foldmethod=marker foldmarker={{{,}}}
  " Make vim open on the line you closed the buffer on
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \     execute 'normal! g`"zvzz' |
    \ endif
augroup END " }}}
" }}}
"
" WebDev {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" Set text width to 80 and add a column at that width
function! ClosureIt() " {{{
  setlocal textwidth=80
  setlocal colorcolumn=+1
  " this color depends on terminal theming
  hi ColorColumn ctermbg=darkgray ctermfg=red
endfunction " }}}

augroup WebDevGroup " {{{
  autocmd!
  autocmd BufRead *.js normal zR
  " autocmd BufNewFile,BufReadPost *.js,*.jsx,*.example,*.jade,*.coffee,*.json,*.scss,*.sass,*.styl,*.less call TwoSpace()
  autocmd BufNewFile,BufReadPost *.js,*.jsx,*.example call ClosureIt()
  autocmd BufNewFile,BufReadPost *.jsx,*.example let b:syntastic_checkers=['eslint']
  autocmd BufNewFile,BufReadPost *.json,*.eslintrc,*.babelrc set filetype=json
  autocmd BufNewFile,BufReadPost *.coffee setl foldmethod=indent nofoldenable "Auto collapse by indentation"

  " Used for Spectacle example files
  autocmd BufNewFile,BufReadPost *.example set filetype=javascript.jsx
  autocmd BufNewFile,BufReadPost *.example set syntax=javascript.jsx
  " Add emmet to appropriate files
  autocmd FileType html,css,javascript.jsx EmmetInstall
augroup END " }}}
" }}}

" Clojure {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
augroup ClojureGroup " {{{
  autocmd!
  autocmd BufRead,BufNewFile build.boot,*.cljs.edn set filetype=cljs
  " Turn off paredit/sexp insert mappings where parinfer is running
  autocmd FileType clojure,scheme,lisp,racket let g:paredit_mode=0
  autocmd FileType clojure,scheme,lisp,racket let g:sexp_enable_insert_mode_mappings=0
augroup end " }}}
" }}}

" HTML/XML {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
augroup XMLGroup " {{{
  autocmd!
  " Remove whitespace markers in htmlxml
  autocmd filetype html,xml set listchars-=tab:>.
  " Set WSDL files using XML filetype
  autocmd BufNewFile,BufRead *.wsdl setlocal ft=xml
augroup END " }}}
" }}}

" Snippets {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
augroup SnippetsGroup " {{{
  autocmd!
  autocmd BufNewFile,BufReadPost *.snippets setlocal noet ci pi sts=0 sw=4 ts=4 listchars-=tab:>. listchars=tab:\|\ "
  autocmd BufNewFile,BufReadPost *.snippets hi SpecialKey ctermbg=NONE ctermfg=gray
augroup END " }}}
" }}}

" Markdown {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
function! WrapIt() " {{{
  setlocal wrap
  setlocal linebreak
  setlocal textwidth=0
  setlocal wrapmargin=0
  setlocal textwidth=80
endfunction " }}}

augroup MarkdownGroup " {{{
  autocmd!
  autocmd BufNewFile,BufReadPost *.md,*.markdown call WrapIt()
  autocmd FileType markdown setl conceallevel=0
augroup END " }}}
" }}}

" Nginx {{{
" ++++++++++++++++++++++++++++++++++++++++++++++++++
augroup NginxGroup " {{{
  au!
  au BufRead,BufNew *.nginx.conf set filetype=nginx
augroup END " }}}
" }}}

" Linux {{{
augroup LinuxGroup
  au!
  autocmd BufRead,BufNewFile *.xrdb set filetype=xdefaults
augroup END
" }}}
