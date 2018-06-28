setlocal foldmethod=indent
setlocal nofoldenable "Auto collapse by indentation"
setlocal textwidth=80
setlocal colorcolumn=+1

highlight ColorColumn ctermbg=darkgray ctermfg=red

let b:syntastic_checkers=['eslint']

EmmetInstall
call AddEmmetMappings()
