" ==================================================
" Load modules
" ==================================================
function! LoadModules(modules)
  for name in a:modules
    exec 'source $HOME/.config/nvim/modules/' . name . '.vim'
  endfor
endfunction

let modules = [
  \'plugins',
  \'utils',
  \'config',
  \'filetypes',
  \'keymaps'
\]
call LoadModules(modules)
