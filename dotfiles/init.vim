" ==================================================
" Load modules
" ==================================================
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
  \'keymaps'
\]
call LoadModules(g:modules)
