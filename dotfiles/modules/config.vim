" ==================================================
" General Vim Config
" ==================================================

set ambiwidth="single"   " force East Asian Width Class chars into a single space
set clipboard=unnamed    " Let tmux/terminal manage clipboard
set copyindent           " copy the previous indentation on autoindenting
set expandtab            " convert tabs to spaces
set ignorecase           " ignore case when searching
set list                 " set list mode for listchars
set listchars=tab:>.,trail:.,extends:#,nbsp:. "mark whitespace
set nobackup             " disable backups"
set noswapfile           " disable backups"
set nowrap               " don't wrap lines
set number               " hybrid mode numbers
set relativenumber       " always show line numbers
set shiftround           " use multiple of shiftwidth when indenting with "<" and ">"
set shiftwidth=2         " number of spaces to use for autoindenting
set showmatch            " set show matching parenthesis
set spell                " enable spell checking
set spelllang=en_us      " set spell language to US english
set synmaxcol=512        " prevent long lines from hanging vim
set tabstop=2            " a tab is two spaces
set timeoutlen=1000      " add mapping key timeout delay
set title                " change the terminal"s title
set ttimeoutlen=0        " remove key code delays
set undolevels=1000      " use many muchos levels of undo
set visualbell           " flash screen on error
set wildignore=*.swp,*.bak,*.pyc,*.class " ignore these files


" Turns on plugins
filetype plugin on

" Turns on automatic indent from plugins
filetype indent on

" Underline misspelled words
highlight clear SpellBad
highlight SpellBad ctermfg=none ctermbg=none cterm=underline
" make the highlighting of tabs and other non-text less annoying
highlight SpecialKey ctermbg=none ctermfg=8
highlight NonText ctermbg=none ctermfg=8
" Italicize comments and html attributes
highlight Comment cterm=italic
highlight htmlArg cterm=italic

" add vim theme
colorscheme dracula

" This let's the cursor change depending on mode
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

" ==================================================
" Plugin Specific Config
" ==================================================

" Syntastic Settings
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:syntastic_mode_map={
  \'mode': 'active',
  \'active_filetypes': [ 'vim' ],
  \'passive_filetypes': []
\}
let g:syntastic_check_on_open = 1
let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='W'
let g:syntastic_html_checkers = [ 'tidy' ]
let g:syntastic_html_tidy_ignore_errors = [ ' proprietary attribute \"ng-']" ]
let g:syntastic_json_checkers = [ 'jsonlint' ]
let g:syntastic_javascript_checkers = [ 'eslint' ]
let g:syntastic_vim_checkers = [ 'vint' ]

" Rainbow Parens Settings
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" Enable rainbow parentheses for all buffers
augroup rainbow_parentheses
  autocmd!
  au VimEnter * RainbowParenthesesActivate
  au BufEnter * RainbowParenthesesLoadRound
  au BufEnter * RainbowParenthesesLoadSquare
  au BufEnter * RainbowParenthesesLoadBraces
augroup END

" vim-json
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:vim_json_syntax_concealcursor=1
let g:indentLine_noConcealCursor=1

" deoplete (autocomplete)
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:deoplete#enable_at_startup = 1

" Ultisnips
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<C-j>'
let g:UltiSnipsJumpBackwardTrigger = '<C-k>'

" Emmet
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" Emmet in html/css/jsx
let g:user_emmet_install_global=0
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

" MatchTagAlways
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:mta_filetypes = {
\ 'html' : 1,
\ 'xhtml' : 1,
\ 'xml' : 1,
\ 'javascript.jsx' : 1,
\}
let g:mta_use_matchparen_group = 0

" Vim-scrollfix
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:scrollfix=50
let g:fixeof=0

" NerdTree
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:NERDTreeShowHidden=1


" Nerd Commenter
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims=1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs=1
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1


" Theme config
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:airline_theme='dracula'
" Automatically displays all buffers when there's only one tab open.
let g:airline#extensions#tabline#enabled=2
let g:airline#extensions#tabline#left_sep=' '
let g:airline#extensions#tabline#left_alt_sep='|'
let g:airline_powerline_fonts=1

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
let g:tmuxline_preset={
  \ 'a': '#S',
  \ 'b': '#W',
  \ 'c': '#H',
  \ 'win': '#I #W',
  \ 'cwin': '#I #W',
  \ 'x': '%a',
  \ 'y': '#W %R',
  \ 'z': '#H'
\}

let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1
