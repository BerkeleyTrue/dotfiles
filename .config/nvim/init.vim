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
" Utils
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
call s:InstallFzF()
Plug 'ashisha/image.vim'
Plug 'bronson/vim-crosshairs'
Plug 'camspiers/animate.vim'
Plug 'chrisbra/nrrwrgn'
Plug 'christoomey/vim-tmux-navigator'
Plug 'danilamihailov/beacon.nvim'
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'haya14busa/incsearch-fuzzy.vim'
Plug 'haya14busa/incsearch.vim'
Plug 'kana/vim-textobj-user'
Plug 'kien/rainbow_parentheses.vim'
Plug 'kristijanhusak/defx-git'
Plug 'kristijanhusak/defx-icons'
Plug 'kshenoy/vim-signature'
Plug 'machakann/vim-sandwich'
Plug 'matze/vim-move'
Plug 'mg979/vim-visual-multi'
Plug 'mhinz/vim-signify'
Plug 'mhinz/vim-startify'
Plug 'mileszs/ack.vim'
Plug 'moll/vim-bbye'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'ntpeters/vim-better-whitespace'
Plug 'radenling/vim-dispatch-neovim'
Plug 'raimondi/delimitMate'
Plug 'scrooloose/nerdcommenter'
Plug 'shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'tmhedberg/matchit'
Plug 'tmux-plugins/vim-tmux'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-unimpaired'
Plug 'valloric/MatchTagAlways'
Plug 'vim-scripts/BufOnly.vim'
Plug 'vim-scripts/folddigest.vim'
Plug 'vim-scripts/scrollfix'
Plug 'w0rp/ale'
Plug 'wakatime/vim-wakatime'
Plug 'wellle/targets.vim'
"Plug 'wincent/corpus' requires 5.0
Plug 'yggdroot/indentLine'
"}}}

" Pop Up Menu Completion
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
Plug 'neoclide/coc-neco'
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'shougo/neco-vim'
" }}}

" Snippets
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
Plug 'berkeleyTrue/berkeleys-snippet-emporium'
Plug 'roxma/nvim-yarp',
Plug 'sirVer/ultisnips'
Plug 'wellle/tmux-complete.vim'
" }}}

" Theme
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
Plug 'dracula/vim', { 'commit': '8d8af7abeef92ae81336679688812c585baf241e' }
Plug 'vim-airline/vim-airline'
" }}}

" Lang
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
Plug 'blackrush/vim-gocode'
Plug 'chr4/nginx.vim'
Plug 'chrisbra/csv.vim'
Plug 'derekwyatt/vim-scala'
Plug 'digitaltoad/vim-jade'
Plug 'fatih/vim-go'
Plug 'glench/vim-jinja2-syntax'
Plug 'godlygeek/tabular' | Plug 'plasticboy/vim-markdown'
Plug 'hashivim/vim-terraform'
Plug 'lervag/vimtex'
Plug 'nsf/gocode', { 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh' }
Plug 'othree/xml.vim'
Plug 'pearofducks/ansible-vim'
Plug 'potatoesmaster/i3-vim-syntax'
Plug 'rust-lang/rust.vim'
Plug 'shime/vim-livedown'
Plug 'sirtaj/vim-openscad'
Plug 'tbastos/vim-lua'
" }}}

" Web
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
Plug 'elzr/vim-json'
Plug 'hhsnopek/vim-sugarss'
Plug 'jparise/vim-graphql'
Plug 'leafgarland/typescript-vim'
Plug 'mattn/emmet-vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'meain/vim-package-info', { 'do': 'npm install' }
Plug 'moll/vim-node'
Plug 'pangloss/vim-javascript'
Plug 'posva/vim-vue'
Plug 'wavded/vim-stylus'
" }}}

" ()
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
Plug 'bakpakin/fennel.vim'
Plug 'clojure-vim/vim-jack-in'
Plug 'eraserhd/parinfer-rust', { 'do': 'cargo build --release'}
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-sexp'
Plug 'olical/aniseed'
Plug 'olical/conjure', {'tag': 'v4.5.0'}
Plug 'tpope/vim-dispatch'
Plug 'radenling/vim-dispatch-neovim'
Plug 'tpope/vim-fireplace' " note: attempts to bind to K
Plug 'tpope/vim-sexp-mappings-for-regular-people'
" }}}
call plug#end()
" }}}
"
" ==================================================
" General Config
" ================================================== {{{
set ambiwidth="single"                         "  force East Asian Width Class chars into a single space
set autoread                                   "  autoread the file into buffer on focus
set clipboard=unnamedplus                      "  default yank into + register, which is the default clipboard for linux may break in osx?
set cmdheight=2                                "  better display for messages
set copyindent                                 "  copy the previous indentation on autoindenting
set expandtab                                  "  convert tabs to spaces
set ignorecase                                 "  ignore case when searching
set list                                       "  set list mode for listchars
set listchars=tab:>.                           "  mark whitespace
set mouse=a                                    "  enable the use of the mouse
set nobackup                                   "  disable backups"
set noswapfile                                 "  disable backups"
set nowrap                                     "  don't wrap lines
set number                                     "  hybrid mode numbers
set shiftround                                 "  use multiple of shiftwidth when indenting with "<" and ">"
set shiftwidth=2                               "  number of spaces to use for autoindenting
set shortmess+=c                               "  don't give |ins-completion-menu| messages.
set showmatch                                  "  set show matching parenthesis
set signcolumn=yes                             "  always show signcolumns
set spell                                      "  enable spell checking
set spelllang=en_us                            "  set spell language to US english
set norelativenumber                           "  no relative numbers
set synmaxcol=512                              "  prevent long lines from hanging vim
set tabstop=2                                  "  a tab is two spaces
set termguicolors                              "  use gui colors in term's that support it
set timeoutlen=1000                            "  add mapping key timeout delay
set title                                      "  change the terminal"s title
set ttimeoutlen=0                              "  remove key code delays
set undolevels=1000                            "  use many muchos levels of undo
set updatetime=300                             "  smaller updatetime for CursorHold & CursorHoldI
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
let g:ale_sign_warning = ''
let g:ale_echo_msg_format = '%linter%(%code%): %s'
let g:ale_linters = {
  \ 'javascript': ['eslint'],
  \ 'typescript': ['tslint', 'tsserver'],
  \ 'pug': ['pug-lint'],
  \ 'clojure': ['clj-kondo', 'joker'],
  \ 'yaml.ansible': ['ansible-lint'],
\}
let g:ale_sign_column_always = 1
let g:ale_virtualtext_cursor = 1
let g:ale_virtualtext_prefix = '//=> '
let g:ale_virtualtext_delay = 300
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
let g:UltiSnipsJumpForwardTrigger = '<C-b>'
let g:UltiSnipsJumpBackwardTrigger = '<C-c>'
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
let g:airline_powerline_fonts = 1
let g:airline_detect_spell=0

let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''

" Tab buffer list above the window
let g:airline#extensions#tabline#enabled = 2

let g:airline#extensions#ale#enabled = 0

let g:airline#extensions#coc#enabled = 1
let g:airline#extensions#coc#warning_symbol = ''
let g:airline#extensions#coc#error_symbol = '✗'

let g:airline#extensions#whitespace#enabled = 0


" Func: s:GetWD
"-------------------------------------------------- {{{
function! GetWD()
  let l:wd=expand('%:.')

  return strlen(l:wd) < 14 ? ' '.l:wd : '../'.expand('%:.:h:t').'/'.expand('%:t')
endfunction "}}}

call airline#parts#define_function('pwd', 'GetWD')

" Func: AirlineInit()
"-------------------------------------------------- {{{
function! s:AirlineInit()
  " code
  let g:airline_symbols.linenr = ''
  let g:airline_symbols.maxlinenr = ''
  let g:airline_section_a = airline#section#create(['mode', 'crypt', 'paste', 'iminsert'])
  let g:airline_section_b = airline#section#create_left(['hunks'])
  let g:airline_section_c = airline#section#create_left(['pwd', 'readonly'])

  let g:airline_section_x = airline#section#create_right(['bookmark', 'tagbar', 'vista', 'gutentags', 'omnisharp', 'grepper'])
  let g:airline_section_y = airline#section#create_left(['filetype'])
  let g:airline_section_z = airline#section#create(['linenr', 'maxlinenr', ':%3v'])

endfunction "}}}

" Group: AirlineAuGroup
"-------------------------------------------------- {{{
augroup AirlineAuGroup
  autocmd!
  autocmd VimEnter * call s:AirlineInit()
augroup END "}}}
" }}}

" NERDTree config
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1
let g:NERDTreeQuitOnOpen=1
let g:NERDTreeShowHidden = 1
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
let g:tex_conceal = 0
let g:tex_flavor = 'plain'
" }}}
"
" vim-multi-cursor
"++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:multi_cursor_exit_from_insert_mode = 0
let g:multi_cursor_exit_from_visual_mode = 0
"}}}
"
" vim-sandwich
"++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)

" add spaces inside braket
let g:sandwich#recipes += [
  \   {'buns': ['{ ', ' }'], 'nesting': 1, 'match_syntax': 1, 'kind': ['add', 'replace'], 'action': ['add'], 'input': ['{']},
  \   {'buns': ['[ ', ' ]'], 'nesting': 1, 'match_syntax': 1, 'kind': ['add', 'replace'], 'action': ['add'], 'input': ['[']},
  \   {'buns': ['( ', ' )'], 'nesting': 1, 'match_syntax': 1, 'kind': ['add', 'replace'], 'action': ['add'], 'input': ['(']},
  \   {'buns': ['{\s*', '\s*}'],   'nesting': 1, 'regex': 1, 'match_syntax': 1, 'kind': ['delete', 'replace', 'textobj'], 'action': ['delete'], 'input': ['{']},
  \   {'buns': ['\[\s*', '\s*\]'], 'nesting': 1, 'regex': 1, 'match_syntax': 1, 'kind': ['delete', 'replace', 'textobj'], 'action': ['delete'], 'input': ['[']},
  \   {'buns': ['(\s*', '\s*)'],   'nesting': 1, 'regex': 1, 'match_syntax': 1, 'kind': ['delete', 'replace', 'textobj'], 'action': ['delete'], 'input': ['(']},
  \ ]
"}}}

" vim-jsx-pretty
"++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:vim_jsx_pretty_highlight_close_tag = 1
"}}}
"
" vim-starify
"++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:startify_change_to_dir = 0
" }}}
"
" ntpeters/vim-better-whitespace
" ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
let g:better_whitespace_enabled=0
let g:strip_whitespace_on_save=1
let g:strip_whitespace_confirm=0
let g:strip_only_modified_lines=1
" }}}
"
"" -- End Plugin Config -- }}}

" ==================================================
" Colors
" ================================================== {{{
" fg        = ['#F8F8F2', 253]
" bglighter = ['#424450', 238]
" bglight   = ['#343746', 237]
" bg        = ['#282A36', 236]
" bgdark    = ['#21222C', 235]
" bgdarker  = ['#191A21', 234]

" comment   = ['#6272A4',  61]
" selection = ['#44475A', 239]
" subtle    = ['#424450', 238]

" cyan      = ['#8BE9FD', 117]
" green     = ['#50FA7B',  84]
" orange    = ['#FFB86C', 215]
" pink      = ['#FF79C6', 212]
" purple    = ['#BD93F9', 141]
" red       = ['#FF5555', 203]
" yellow    = ['#F1FA8C', 228]

function! AddHighlight()
  " Underline misspelled words
  highlight clear SpellBad
  highlight SpellBad ctermfg=none ctermbg=none cterm=underline
  highlight SpellLocal ctermfg=none ctermbg=none cterm=underline
  highlight SpellRare ctermfg=none ctermbg=none cterm=underline

  " make the highlighting of tabs and other non-text less annoying
  highlight NonText ctermbg=none ctermfg=8 guibg=none guifg=#808080
  highlight SpecialKey ctermbg=none ctermfg=88 guibg=none guifg=#870000

  highlight link Constant DraculaRed

  " Defx_icons
  "++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
  highlight Defx_icons_js ctermfg=84
  highlight Defx_icons_vim ctermfg=9
  highlight Defx_icons_markdown ctermfg=9
  "}}}

  " JavaScript
  " ++++++++++++++++++++++++++++++++++++++++++++++++++ {{{
  highlight link jsFuncCall DraculaPurple
  highlight link jsGlobalObjects Constant
  highlight link jsGlobalNodeObjects jsGlobalObjects
  highlight link jsThis Constant
  highlight link jsString DraculaOrange
  highlight link jsTemplateString DraculaOrange
  highlight link jsTemplateVar DraculaYellow
  highlight jsTemplateBraces ctermfg=24 guifg=#005487


  highlight link jsObjectKey DraculaCyan
  highlight link jsObject jsObjectKey
  highlight link jsSpreadExpression jsObjectKey
  highlight link jsObjectKeyComputed jsObjectKey
  highlight link jsObjectProp jsObjectKey

  highlight link jsVariableDef jsFuncName
  highlight link jsDestructuringBlock jsFuncName
  highlight link jsDestructuringPropertyValue jsFuncName
  highlight link jsDestructuringProperty jsFuncName
  highlight link jsFuncArgs jsFuncName
  highlight link jsxCloseTag DraculaPink
  highlight link jsxCloseString DraculaPink
  highlight link jsxClosePunct DraculaPink
  highlight link jsxTagName DraculaPink
  " }}}

  highlight link jsonKeyword DraculaCyan
  highlight link jsonString DraculaGreen
  highlight link jsonNumber Number
  highlight link jsonBoolean Boolean

  highlight link vimMapLhs DraculaOrange
  highlight link vimMapModKey DraculaOrange
  highlight link vimNotation DraculaGreen

  " Vim crosshairs
  highlight link CursorLine DraculaBgDark
  highlight CursorColumn ctermbg=24 ctermfg=NONE guibg=#005487 guifg=NONE

  " rainbow parentheses
  highlight link level10c DraculaOrange
endfunction

" Fix for term truecolor
let g:dracula_colorterm = 0
" add Vim theme
colorscheme dracula

augroup AutoColors
  autocmd!
  autocmd VimEnter,ColorScheme * call AddHighlight()
augroup END
" }}}
