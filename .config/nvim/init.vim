" ==================================================
" Load modules
" ==================================================

let s:plugFile=$HOME.'/.config/nvim/autoload/plug.vim'
let s:plugFileRaw='https://raw.github.com/junegunn/vim-plug/master/plug.vim'
" download vim-plug if missing
if empty(glob(s:plugFile))
  silent! execute '!curl --create-dirs -fsSLo '.s:plugFile.' '.s:plugFileRaw
  augroup PlugInstallGroup
    au!
    autocmd VimEnter * silent! PlugInstall
  augroup END
endif

function! LoadModules(modules)
  for l:name in a:modules
    exec 'source $HOME/.config/nvim/modules/' . l:name . '.vim'
  endfor
endfunction

let g:modules = [
  \'plugins',
  \'utils',
  \'config',
  \'filetypes',
  \'keymaps',
  \'colors'
\]
call LoadModules(g:modules)
