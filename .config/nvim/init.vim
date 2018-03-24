" ==================================================
" Init Vim Plug
" ================================================== {{{
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

" ==================================================
" Install FzF
" ================================================== {{{
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


" ==================================================
" Install Plugins
" ================================================== {{{
call plug#begin()
" Lint
Plug 'w0rp/ale'
" Utils
call s:InstallFzF()
Plug 'bronson/vim-crosshairs'
Plug 'easymotion/vim-easymotion'
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
Plug 'blackrush/vim-gocode'
Plug 'chr4/nginx.vim'
Plug 'chrisbra/csv.vim'
Plug 'derekwyatt/vim-scala'
Plug 'digitaltoad/vim-jade'
Plug 'fatih/vim-go'
Plug 'godlygeek/tabular' | Plug 'plasticboy/vim-markdown'
Plug 'lambdatoast/elm.vim'
Plug 'lervag/vimtex'
Plug 'mattn/emmet-vim'
Plug 'mboughaba/i3config.vim'
Plug 'nsf/gocode', { 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh' }
Plug 'othree/xml.vim'
Plug 'shime/vim-livedown'
Plug 'vim-scripts/paredit.vim'
Plug 'wavded/vim-stylus'
" Javascript
Plug 'elzr/vim-json'
Plug 'moll/vim-node'
Plug 'mxw/vim-jsx'
Plug 'pangloss/vim-javascript'
Plug 'roxma/nvim-cm-tern',  { 'do': 'npm install' }
" Clojure
Plug 'clojure-vim/nvim-parinfer.js', { 'do': 'lein npm install' }
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-fireplace' " note: attempts to bind to K
Plug 'tpope/vim-sexp-mappings-for-regular-people'
call plug#end() " }}}

" ==================================================
" Open nav on empty startup
" ================================================== {{{
function! OpenNavOnStartup()
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


" ==================================================
" General Autocmds
" ================================================== {{{
augroup GeneralGroup
  au!
  autocmd BufRead,BufNewFile * :highlight SpellBad ctermbg=none ctermfg=none cterm=underline

  " On insert mode set absolute row numbers
  " On leave Return to relative row numbers
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &number | set relativenumber | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &number | set norelativenumber | endif
  autocmd VimEnter * call OpenNavOnStartup()
  " Resize splits when the window is resized
  autocmd VimResized * exe "normal! \<c-w>="
  " Make vim open on the line you closed the buffer on
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \     execute 'normal! g`"zvzz' |
    \ endif
augroup END " }}}

function! LoadModules(modules)
  for l:name in a:modules
    exec 'source $HOME/.config/nvim/modules/' . l:name . '.vim'
  endfor
endfunction


let g:modules = [
  \'utils',
  \'config',
  \'keymaps',
  \'colors'
\]
call LoadModules(g:modules)
