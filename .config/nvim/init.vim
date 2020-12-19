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
call s:InstallFzF()

" file tree
Plug 'shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'kristijanhusak/defx-git'
Plug 'kristijanhusak/defx-icons'

Plug 'neoclide/coc-neco'
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'shougo/neco-vim'

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
call plug#end()

" make sure aniseed path is available for macros lookup
let &runtimepath.=','.stdpath('config').'/pack/packer/start/aniseed'
" check if aniseed is installed, if not, run make aniseed to install and
" init
lua <<EOF
  local isInConf = vim.fn.stdpath("config") == vim.fn.getcwd()

  if isInConf then
    print("[init.vim]: in conf dir: force compiling")
  end

  local ok, res = pcall(require, 'aniseed.env')

  if ok then
    res.init({ force = isInConf })
  else
    print('Aniseed not found. Running aniseed install now')
    print(vim.api.nvim_call_function('system', {'make aniseed'}))
    local ok, res = pcall(require, 'aniseed.env')

    if not ok then
      print('Could not load after install')
    else
      res.init({ force = isInConf })
    end

  end
EOF
