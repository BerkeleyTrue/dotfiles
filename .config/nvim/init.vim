" ==================================================
" Install Plugins
" ================================================== {{{
"
" Init Vim Plug
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let s:plugFile=$HOME.'/.config/nvim/autoload/plug.vim'
let s:plugFileRaw='https://raw.github.com/junegunn/vim-plug/master/plug.vim'
" download vim-plug if missing
if empty(glob(s:plugFile))
  silent! execute '!curl --create-dirs -fsSLo '.s:plugFile.' '.s:plugFileRaw
  augroup PlugInstallGroup
    au!
    autocmd VimEnter * silent! PlugInstall
  augroup END
endif " }}}

" Func: Install FzF
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let s:homebrewdir='/usr/local/opt'
let s:sharedir='/usr/share'
function! s:InstallFzF()
  if isdirectory(s:homebrewdir.'/fzf')
    let l:fzf = s:homebrewdir.'/fzf'
  elseif isdirectory(s:sharedir.'/vim/vimfiles')
    let l:fzf = s:sharedir.'/vim/vimfiles'
  endif

  if exists('l:fzf')
    Plug l:fzf | Plug 'junegunn/fzf.vim'
  else
    echomsg 'vim:plugins fzf not found'
    return 1
  endif
endfunction " }}}

call plug#begin()
" Lint
Plug 'w0rp/ale'
" Utils
call s:InstallFzF()
Plug 'bronson/vim-crosshairs'
Plug 'christoomey/vim-tmux-navigator'
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'haya14busa/incsearch-fuzzy.vim'
Plug 'haya14busa/incsearch.vim'
Plug 'kien/rainbow_parentheses.vim'
Plug 'majutsushi/tagbar' | Plug 'lvht/tagbar-markdown'
Plug 'matze/vim-move'
Plug 'mhinz/vim-signify'
Plug 'mileszs/ack.vim'
Plug 'moll/vim-bbye'
Plug 'raimondi/delimitMate'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'shougo/neco-vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'tmhedberg/matchit'
Plug 'tmux-plugins/vim-tmux'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'valloric/MatchTagAlways'
Plug 'vim-scripts/BufOnly.vim'
Plug 'vim-scripts/scrollfix'
Plug 'wakatime/vim-wakatime'
Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'yggdroot/indentLine'
" Snippets/completion
Plug 'berkeleyTrue/berkeleys-snippet-emporium'
Plug 'roxma/nvim-completion-manager'
Plug 'sirVer/ultisnips'
" Theme
Plug 'dracula/vim'
Plug 'edkolev/tmuxline.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Lang
Plug 'autozimu/languageclient-neovim', {
  \ 'branch': 'next',
  \ 'do': 'bash install.sh',
  \ }
Plug 'blackrush/vim-gocode'
Plug 'chr4/nginx.vim'
Plug 'chrisbra/csv.vim'
Plug 'derekwyatt/vim-scala'
Plug 'digitaltoad/vim-jade'
Plug 'fatih/vim-go'
Plug 'godlygeek/tabular' | Plug 'plasticboy/vim-markdown'
Plug 'hhsnopek/vim-sugarss'
Plug 'lambdatoast/elm.vim'
Plug 'lervag/vimtex'
Plug 'mattn/emmet-vim'
Plug 'nsf/gocode', { 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh' }
Plug 'othree/xml.vim'
Plug 'potatoesmaster/i3-vim-syntax'
Plug 'shime/vim-livedown'
Plug 'vim-scripts/paredit.vim'
Plug 'wavded/vim-stylus'
" Javascript
Plug 'elzr/vim-json'
Plug 'jparise/vim-graphql'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'moll/vim-node'
Plug 'pangloss/vim-javascript'
Plug 'roxma/nvim-cm-tern',  { 'do': 'npm install' }
" Clojure
Plug 'clojure-vim/nvim-parinfer.js', { 'do': 'lein npm install' }
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-fireplace' " note: attempts to bind to K
Plug 'tpope/vim-sexp-mappings-for-regular-people'
call plug#end() " }}}
"
" ==================================================
" General Config
" ================================================== {{{
set autoread                                   "  autoread the file into buffer on focus
set ambiwidth="single"                         "  force East Asian Width Class chars into a single space
set clipboard=unnamedplus                      "  default yank into + register, which is the default clipboard for linux may break in osx?
set copyindent                                 "  copy the previous indentation on autoindenting
set expandtab                                  "  convert tabs to spaces
set ignorecase                                 "  ignore case when searching
set list                                       "  set list mode for listchars
set listchars=tab:>.,trail:.,extends:#,nbsp:.  "  mark whitespace
set nobackup                                   "  disable backups"
set noswapfile                                 "  disable backups"
set nowrap                                     "  don't wrap lines
set number                                     "  hybrid mode numbers
set shiftround                                 "  use multiple of shiftwidth when indenting with "<" and ">"
set shiftwidth=2                               "  number of spaces to use for autoindenting
set showmatch                                  "  set show matching parenthesis
set spell                                      "  enable spell checking
set spelllang=en_us                            "  set spell language to US english
set synmaxcol=512                              "  prevent long lines from hanging vim
set tabstop=2                                  "  a tab is two spaces
set timeoutlen=1000                            "  add mapping key timeout delay
set title                                      "  change the terminal"s title
set ttimeoutlen=0                              "  remove key code delays
set undolevels=1000                            "  use many muchos levels of undo
set visualbell                                 "  flash screen on error
set wildignore=*.swp,*.bak,*.pyc,*.class       "  ignore these files
" nvim blinking cursor
" see :help 'guicursor'
set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
      \,a:blinkwait200-blinkoff400-blinkon250-Cursor/lCursor
      \,sm:block-blinkwait175-blinkoff150-blinkon175


" Turns on plugins
filetype plugin on

" Turns on automatic indent from plugins
filetype indent on
" }}}

" ==================================================
" Plugin Config
" ================================================== {{{

" pangloss/vim-javascript
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1
" }}}

" Ale Settings
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:ale_sign_error = '✗'
let g:ale_sign_warning = 'W'
let g:ale_echo_msg_format = '%linter%: %severity% - %s'
let g:ale_linters = {'javascript': ['eslint']}
let g:ale_sign_column_always = 1
" }}}

" Rainbow Parens Settings
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
" Enable rainbow parentheses for all buffers
augroup rainbow_parentheses
  autocmd!
  au VimEnter * RainbowParenthesesActivate
  au BufEnter * RainbowParenthesesLoadRound
  au BufEnter * RainbowParenthesesLoadSquare
  au BufEnter * RainbowParenthesesLoadBraces
augroup END
" }}}

" vim-json
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
" VIm does not have the ability to programmatically
" define how a concealed element can be replaced with
" so we must disable indentLine in json files as
" this conflicts with the concealing ability of vim-json
let g:indentLine_fileTypeExclude = [ 'json' ]
" }}}

" deoplete (autocomplete)
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:deoplete#enable_at_startup = 1
" make sure autocompletion in file paths are relative
" to the file buffer
let g:deoplete#file#enable_buffer_path = 1
let g:deoplete#enable_smart_case = 1
" }}}

" Ultisnips
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
" We map the trigger initially to <c-w>
" so our <tab> keymaps are not overwritten
" by ultisnips.
" We then replicate the normal/x/select
" mode binding for ultisnips while
" keeping our own insert mode bindings
" This is not ideal but I'm unable to find another
" way.
let g:UltiSnipsExpandTrigger = '<C-w>'
let g:UltiSnipsJumpForwardTrigger = '<C-j>'
let g:UltiSnipsJumpBackwardTrigger = '<C-k>'
" }}}

" Emmet
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
" Emmet in html/css/jsx
let g:user_emmet_install_global = 0
" Use single quotes for Emmet (bam!)
let g:user_emmet_settings = {
\  'html' : {
\    'quote_char': "'",
\  },
\  'jsx' : {
\    'quote_char': "'",
\  },
\  'javascript.jsx' : {
\      'extends' : 'jsx',
\  },
\}
" }}}

" MatchTagAlways
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:mta_filetypes = {
\ 'html' : 1,
\ 'xhtml' : 1,
\ 'xml' : 1,
\ 'javascript.jsx' : 1,
\}
let g:mta_use_matchparen_group = 1
" }}}

" Vim-scrollfix
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:scrollfix = 50
let g:fixeof = 0
" }}}

" Nerd Commenter
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 0
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" }}}

" Airline config
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:airline_theme='dracula'
" Automatically displays all buffers when there's only one tab open.
function! AleError()
  let l:loclist = ale#engine#GetLoclist(bufnr('%'))
  if !empty(l:loclist)
    let l:item = l:loclist[0]

    return [l:item.lnum, l:item.col]
  endif
  return []
endfunction

function! AleErrorMessage()
  let l:error = AleError()
  let l:count = ale#statusline#Count(bufnr('%'))
  if empty(l:error)
    return ''
  endif
  return printf('E: pos[%d, %d]: (%d)', l:error[0], l:error[1], l:count.total)
endfunction

call airline#parts#define_function('ale_error_message', 'AleErrorMessage')

let g:airline#extensions#tabline#enabled = 2
let g:airline_powerline_fonts = 1
let g:airline#extensions#ale#enabled = 1
let g:airline_section_error = airline#section#create(['ale_error_message'])

" Tmuxline key legend
" #H Hostname of local host
" #h Hostname of local host without the domain name
" #F Current window flag
" #I Current window index
" #S Session name
" #W Current window name
" #(shell-command)  First line of the command's output
" Tmuxline layout
" a > b > c > win > cwin   x < y < z
let g:tmuxline_preset = {
  \ 'a': '#S',
  \ 'b': '#W',
  \ 'c': '#H',
  \ 'win': '#I #W',
  \ 'cwin': '#I #W',
  \ 'x': '%a',
  \ 'y': '#W %R',
  \ 'z': '#H'
\}
" }}}

" NERDTree config
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:NERDTreeShowHidden = 1
let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1
" }}}

" Sexp config
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:sexp_enable_insert_mode_mappings = 0
" }}}

" Delimate config
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
" When opening a pair of surround and hitting <CR>
" this will expand the pair onto new lines
let g:delimitMate_expand_cr = 2
" Same as above but will add padding to surround
let g:delimitMate_expand_space = 1
" Allow inserting closing surround on expansion
" to jump to the already existing closing
" surround instead of inserting a new closing surround
let g:delimitMate_jump_expansion = 1
" }}}

" vim crosshairs
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
set cursorcolumn
set cursorline
" }}}

" Markdown config
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:vim_markdown_conceal = 0
" }}}

" Vimtex config
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:vimtex_fold_enabled = 1
let g:tex_conceal=0
" }}}
"
" LanguageClient-neovim
"++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:LanguageClient_serverCommands = {
  \ 'javascript': ['flow-language-server', '--stdio'],
  \ 'javascript.jsx': ['flow-language-server', '--stdio'],
  \}
let g:LanguageClient_autoStart = 1
"}}}
"
" vim-multi-cursor
"++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:multi_cursor_exit_from_insert_mode = 0
let g:multi_cursor_exit_from_visual_mode = 0
"}}}
" -- End Plugin Config -- }}}

" ==================================================
" General Autocmds
" ================================================== {{{

" Func: Open nav on empty startup
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
function! s:OpenNavOnStartup()
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

" Func: Disable spell check on camelCase
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
function! s:DisableSpellOnCamelCase()
  syntax match CamelCase /\<[A-Z][a-z]\+[A-Z].\{-}\>/ contains=@NoSpell transparent
  syntax cluster Spell add=CamelCase
endfunction " }}}

augroup GeneralGroup
  autocmd!
  autocmd BufEnter,BufRead * call s:DisableSpellOnCamelCase()
  " On insert mode set absolute row numbers
  " On leave Return to relative row numbers
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &number | set relativenumber | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &number | set norelativenumber | endif
  autocmd VimEnter * call s:OpenNavOnStartup()
  " Resize splits when the window is resized
  autocmd VimResized * exe "normal! \<c-w>="
  " Make vim open on the line you closed the buffer on
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \     execute 'normal! g`"zvzz' |
    \ endif
augroup END " }}}

" ==================================================
" Colors
" ================================================== {{{
" Dracula color map
" cterm |   web   | name
" 9     |         | Red (FireBrick?)
" 17    | #6272a4 | Comment (Dark Blue)
" 23    |         | Teal
" 24    |         | Dark Neon Blue
" 60    | #6272a4 | Comment (Dark Blue)
" 61    |         | Purple
" 64    |         | Olive
" 81    | #8be9fd | Blue
" 84    | #50fa7b | Green
" 88    |         | Ruby
" 117   |         | Neon Blue
" 141   |         | Dark Purple
" 203   |         | Red-Orange
" 212   | #ff79c6 | Pink
" 215   |         | Orange
" 228   | #50fa7b | Yellow
" 231   | #f8f8f2 | Foreground (White)
" 234   | #282a36 | Black
" 235   |         | Dark Background
" 236   |         | Background
" 241   |         | Selection (Dark gray)
" 246   | #44475a | Selection (gray)

function! AddHighlight()
  " Underline misspelled words
  highlight clear SpellBad
  highlight SpellBad ctermfg=none ctermbg=none cterm=underline
  highlight SpellLocal ctermfg=none ctermbg=none cterm=underline
  highlight SpellRare ctermfg=none ctermbg=none cterm=underline

  " make the highlighting of tabs and other non-text less annoying
  highlight NonText ctermbg=none ctermfg=8
  highlight SpecialKey ctermbg=none ctermfg=88

  highlight Constant ctermfg=203

  " JavaScript
  " ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
  highlight jsFuncCall ctermfg=141
  highlight link jsGlobalObjects Constant
  highlight link jsGlobalNodeObjects jsGlobalObjects
  highlight link jsThis Constant
  highlight jsString ctermfg=215
  highlight jsTemplateBraces ctermfg=24
  highlight jsTemplateString ctermfg=215
  highlight jsTemplateVar ctermfg=228


  highlight jsObjectKey ctermfg=117
  highlight link jsObject jsObjectKey
  highlight link jsSpreadExpression jsObjectKey
  highlight link jsObjectKeyComputed jsObjectKey
  highlight link jsObjectProp jsObjectKey

  highlight link jsVariableDef jsFuncName
  highlight link jsDestructuringBlock jsFuncName
  highlight link jsDestructuringPropertyValue jsFuncName
  highlight link jsDestructuringProperty jsFuncName
  highlight link jsFuncArgs jsFuncName
  " }}}

  highlight jsonKeyword ctermfg=117
  highlight jsonString ctermfg=86
  highlight link jsonNumber Number
  highlight link jsonBoolean Boolean

  highlight vimMapLhs ctermfg=215
  highlight vimMapModKey ctermfg=117
  highlight vimNotation ctermfg=86

  " Vim crosshairs
  highlight CursorLine   ctermbg=235 ctermfg=NONE
  highlight CursorColumn ctermbg=24 ctermfg=NONE
endfunction

" add Vim theme
colorscheme dracula

augroup AutoColors
  autocmd!
  autocmd VimEnter,ColorScheme * call AddHighlight()
augroup END
" }}}
