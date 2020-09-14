setlocal foldmethod=manual
setlocal nofoldenable "Auto collapse by indentation"

setlocal textwidth=80
setlocal colorcolumn=+1

highlight ColorColumn guifg=#FF5555 ctermfg=203 guibg=#44475A ctermbg=239

EmmetInstall
call AddEmmetMappings()
