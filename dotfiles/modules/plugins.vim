" ==================================================
" Plugin Start
" ==================================================

function! BuildNodeHost(info)
  " info is a dictionary with 3 fields
  " - name:   name of the plugin
  " - status: 'installed', 'updated', or 'unchanged'
  " - force:  set on PlugInstall! or PlugUpdate!
  if a:info.status ==? 'installed' || a:info.force
    !npm install --production
    execute ':UpdateRemotePlugins'
  endif
endfunction

call plug#begin()
" Lint
Plug 'vim-syntastic/syntastic'
" Utils
Plug 'Raimondi/delimitMate'
Plug 'Valloric/MatchTagAlways'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'Yggdroot/indentLine'
Plug 'easymotion/vim-easymotion'
Plug 'kien/rainbow_parentheses.vim'
Plug 'matze/vim-move'
Plug 'mhinz/vim-signify'
Plug 'mileszs/ack.vim'
Plug 'moll/vim-bbye'
Plug 'neovim/node-host', { 'do': function('BuildNodeHost') }
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'ternjs/tern_for_vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'tmhedberg/matchit'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/BufOnly.vim'
Plug 'vim-scripts/scrollfix'
Plug 'wakatime/vim-wakatime'
" Snippets
Plug 'BerkeleyTrue/berkeleys-snippet-emporium'
Plug 'SirVer/ultisnips'
Plug 'Shougo/deoplete.nvim'
" Theme
Plug 'dracula/vim'
Plug 'edkolev/tmuxline.vim'
Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline' | Plug 'scrooloose/nerdtree' | Plug 'Xuyuanp/nerdtree-git-plugin' | Plug 'ryanoasis/vim-devicons'
Plug 'vim-airline/vim-airline-themes'
" Lang
Plug 'Blackrush/vim-gocode'
Plug 'chrisbra/csv.vim'
Plug 'derekwyatt/vim-scala'
Plug 'digitaltoad/vim-jade'
Plug 'elzr/vim-json'
Plug 'evanmiller/nginx-vim-syntax'
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-sexp'
Plug 'jnwhiteh/vim-golang'
Plug 'lambdatoast/elm.vim'
Plug 'mattn/emmet-vim'
Plug 'moll/vim-node'
Plug 'mxw/vim-jsx'
Plug 'othree/xml.vim'
Plug 'pangloss/vim-javascript'
" Plug 'clojure-vim/clj-refactor.nvim'
Plug 'clojure-vim/nvim-parinfer.js'
Plug 'suan/vim-instant-markdown'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'vim-scripts/paredit.vim'
Plug 'wavded/vim-stylus'
call plug#end()

