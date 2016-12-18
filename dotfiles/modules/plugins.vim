" ==================================================
" Plugin Start 
" ==================================================

function! BuildYCM(info)
  " info is a dictionary with 3 fields
  " - name:   name of the plugin
  " - status: 'installed', 'updated', or 'unchanged'
  " - force:  set on PlugInstall! or PlugUpdate!
  if a:info.status ==? 'installed' || a:info.force
    !./install.py
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
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'ternjs/tern_for_vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'tmhedberg/matchit'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/BufOnly.vim'
Plug 'vim-scripts/scrollfix'
" Snippets
Plug 'BerkeleyTrue/berkeleys-snippet-emporium'
Plug 'SirVer/ultisnips'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
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
Plug 'jnwhiteh/vim-golang'
Plug 'lambdatoast/elm.vim'
Plug 'mattn/emmet-vim'
Plug 'moll/vim-node'
Plug 'mxw/vim-jsx'
Plug 'othree/xml.vim'
Plug 'pangloss/vim-javascript'
Plug 'suan/vim-instant-markdown'
Plug 'tpope/vim-fireplace'
Plug 'vim-scripts/paredit.vim'
Plug 'wavded/vim-stylus'
call plug#end()

