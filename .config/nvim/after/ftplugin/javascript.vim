setlocal foldmethod=manual
setlocal nofoldenable "Auto collapse by indentation"

setlocal textwidth=80
setlocal colorcolumn=+1
setlocal comments=s:/**,m:\ *,ex:*/,s:/*,m0:\ *,ex:*/://
setlocal formatoptions=jcroql
setlocal cindent
setlocal indentexpr="" "gets overwritten by runtime javascript indent file
setlocal cinoptions=c0.5s,C1

highlight ColorColumn guifg=#FF5555 ctermfg=203 guibg=#44475A ctermbg=239
