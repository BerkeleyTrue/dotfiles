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
" moving/searching
Plug 'matze/vim-move'
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'haya14busa/incsearch-fuzzy.vim'
Plug 'haya14busa/incsearch.vim'
Plug 'mileszs/ack.vim'

" text obj manipulation
Plug 'tpope/vim-repeat'
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-user'

" file tree
Plug 'shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'kristijanhusak/defx-git'
Plug 'kristijanhusak/defx-icons'

" ui
Plug 'kien/rainbow_parentheses.vim'
Plug 'kyazdani42/nvim-web-devicons'

" treesitter
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-treesitter/nvim-treesitter-refactor'
Plug 'nvim-treesitter/playground'
Plug 'romgrk/nvim-treesitter-context'

" parens
Plug 'raimondi/delimitMate'
Plug 'machakann/vim-sandwich'
Plug 'tmhedberg/matchit'
Plug 'valloric/MatchTagAlways'

" tmux
Plug 'tmux-plugins/vim-tmux'
Plug 'tmux-plugins/vim-tmux-focus-events'

"git
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-unimpaired'
" buffers
Plug 'vim-scripts/BufOnly.vim'
Plug 'moll/vim-bbye'

Plug 'w0rp/ale'
Plug 'wakatime/vim-wakatime'
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
