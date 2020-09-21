" Add nvm managed node to PATH
let s:nvm = expand('$NVM_DIR/nvm.sh')


if filereadable(s:nvm)
  let s:get_nvm_path = 'source '.s:nvm.' && nvm which default'
  let s:nvm_path = fnamemodify(system(s:get_nvm_path), ':h')

  if $PATH !~ s:nvm_path
    let $PATH.=':'.s:nvm_path
  endif
endif
