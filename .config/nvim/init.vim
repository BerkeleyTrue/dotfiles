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
Plug 'kyazdani42/nvim-web-devicons'
Plug 'machakann/vim-sandwich'
Plug 'matze/vim-move'
Plug 'mg979/vim-visual-multi'
Plug 'mhinz/vim-signify'
Plug 'mhinz/vim-startify'
Plug 'mileszs/ack.vim'
Plug 'moll/vim-bbye'
Plug 'ntpeters/vim-better-whitespace'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-treesitter/nvim-treesitter-refactor'
Plug 'nvim-treesitter/playground'
Plug 'radenling/vim-dispatch-neovim'
Plug 'raimondi/delimitMate'
Plug 'romgrk/barbar.nvim'
Plug 'romgrk/nvim-treesitter-context'
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
Plug 'w0rp/ale'
Plug 'wakatime/vim-wakatime'
Plug 'wellle/targets.vim'
Plug 'wincent/corpus'
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
Plug 'dracula/vim' ", { 'commit': '8d8af7abeef92ae81336679688812c585baf241e' }
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

" make sure aniseed path is available for macros lookup
let &runtimepath.=','.stdpath('config').'/pack/packer/start/aniseed'
" check if aniseed is installed, if not, run make aniseed to install and
" init
lua <<EOF
  local ok, res = pcall(require, 'aniseed.env');
  if ok then
    res.init()
  else
    print('Aniseed not found. Running aniseed install now')
    print(vim.api.nvim_call_function('system', {'make aniseed'}))
    local ok, res = pcall(require, 'aniseed.env')

    if not ok then
      print('Could not load after install')
    else
      res.init()
    end

  end
EOF
