" ==================================================
" Plugin Start
" ==================================================

let s:homebrewdir='/usr/local/opt'
let s:sharedir='/usr/share'

function! s:InstallFzF() " {{{
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

function! BuildNodeHost(info) " {{{
  " info is a dictionary with 3 fields
  " - name:   name of the plugin
  " - status: 'installed', 'updated', or 'unchanged'
  " - force:  set on PlugInstall! or PlugUpdate!
  if a:info.status ==? 'installed' || a:info.force
    !npm install --production
    execute ':UpdateRemotePlugins'
  endif
endfunction " }}}

call plug#begin()
" Lint
" Plug 'vim-syntastic/syntastic'
Plug 'w0rp/ale'
" Utils
call s:InstallFzF()
Plug 'majutsushi/tagbar' | Plug 'lvht/tagbar-markdown'
Plug 'bronson/vim-crosshairs'
Plug 'easymotion/vim-easymotion'
Plug 'kien/rainbow_parentheses.vim'
Plug 'matze/vim-move'
Plug 'mhinz/vim-signify'
Plug 'mileszs/ack.vim'
Plug 'moll/vim-bbye'
Plug 'neovim/node-host', { 'do': function('BuildNodeHost') }
Plug 'raimondi/delimitMate'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'shougo/neco-vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'tmhedberg/matchit'
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
" Plug 'vim-airline/vim-airline' | Plug 'scrooloose/nerdtree' | Plug 'Xuyuanp/nerdtree-git-plugin' | Plug 'ryanoasis/vim-devicons'
Plug 'vim-airline/vim-airline-themes'
" Lang
Plug 'blackrush/vim-gocode'
Plug 'chr4/nginx.vim'
Plug 'chrisbra/csv.vim'
Plug 'clojure-vim/nvim-parinfer.js'
Plug 'derekwyatt/vim-scala'
Plug 'digitaltoad/vim-jade'
Plug 'elzr/vim-json'
Plug 'fatih/vim-go'
Plug 'godlygeek/tabular' | Plug 'plasticboy/vim-markdown'
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-sexp'
Plug 'lambdatoast/elm.vim'
Plug 'mattn/emmet-vim'
Plug 'mboughaba/i3config.vim'
Plug 'moll/vim-node'
Plug 'mxw/vim-jsx'
Plug 'nsf/gocode', { 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh' }
Plug 'othree/xml.vim'
Plug 'pangloss/vim-javascript'
Plug 'roxma/nvim-cm-tern',  { 'do': 'npm install' }
Plug 'shime/vim-livedown'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'vim-scripts/paredit.vim'
Plug 'wavded/vim-stylus'
call plug#end()

