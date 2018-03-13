" ==================================================
" General Vim Config
" ==================================================

set ambiwidth="single"   " force East Asian Width Class chars into a single space
" default yank into + register, which is the default clipboard for linux
" may break in osx?
set clipboard=unnamedplus 
set copyindent           " copy the previous indentation on autoindenting
set expandtab            " convert tabs to spaces
set ignorecase           " ignore case when searching
set list                 " set list mode for listchars
set listchars=tab:>.,trail:.,extends:#,nbsp:. "mark whitespace
set nobackup             " disable backups"
set noswapfile           " disable backups"
set nowrap               " don't wrap lines
set number               " hybrid mode numbers
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
" nvim blinking cursor
" see :help 'guicursor'
set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
      \,a:blinkwait200-blinkoff400-blinkon250-Cursor/lCursor
      \,sm:block-blinkwait175-blinkoff150-blinkon175


" Turns on plugins
filetype plugin on

" Turns on automatic indent from plugins
filetype indent on

" ==================================================
" Plugin Specific Config
" ==================================================

" Ale Settings
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:ale_sign_error = '✗'
let g:ale_sign_warning = 'W'
let g:ale_echo_msg_format = '%linter%: %severity% - %s'
let g:ale_linters = {'javascript': ['eslint']}
let g:ale_sign_column_always = 1

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
" VIm does not have the ability to programmatically
" define how a concealed element can be replaced with
" so we must disable indentLine in json files as
" this conflicts with the concealing ability of vim-json
let g:indentLine_fileTypeExclude = [ 'json' ]

" deoplete (autocomplete)
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:deoplete#enable_at_startup = 1
" make sure autocompletion in file paths are relative
" to the file buffer
let g:deoplete#file#enable_buffer_path = 1
let g:deoplete#enable_smart_case = 1

" Ultisnips
" ++++++++++++++++++++++++++++++++++++++++++++++++++
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

" Emmet
" ++++++++++++++++++++++++++++++++++++++++++++++++++
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

" MatchTagAlways
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:mta_filetypes = {
\ 'html' : 1,
\ 'xhtml' : 1,
\ 'xml' : 1,
\ 'javascript.jsx' : 1,
\}
let g:mta_use_matchparen_group = 1

" Vim-scrollfix
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:scrollfix = 50
let g:fixeof = 0

" Nerd Commenter
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 0
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'


" Airline config
" ++++++++++++++++++++++++++++++++++++++++++++++++++
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
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
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


" NERDTree config
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:NERDTreeShowHidden = 1
let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1

" Sexp config
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:sexp_enable_insert_mode_mappings = 0

" Delimate config
" ++++++++++++++++++++++++++++++++++++++++++++++++++
" When opening a pair of surround and hitting <CR>
" this will expand the pair onto new lines
let g:delimitMate_expand_cr = 2
" Same as above but will add padding to surround
let g:delimitMate_expand_space = 1
" Allow inserting closing surround on expansion
" to jump to the already existing closing
" surround instead of inserting a new closing surround
let g:delimitMate_jump_expansion = 1


" vim crosshairs
" ++++++++++++++++++++++++++++++++++++++++++++++++++
set cursorcolumn
set cursorline

" Markdown config
" ++++++++++++++++++++++++++++++++++++++++++++++++++
let g:vim_markdown_conceal=0
