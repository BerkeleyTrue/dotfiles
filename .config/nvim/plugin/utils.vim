" Set tabs to two spaces
function! TwoSpace()
  setlocal shiftwidth=2
  setlocal tabstop=2
  setlocal softtabstop=2
  setlocal expandtab
endfunction

function! ConvertSpace()
  set tabstop=4 softtabstop=4 noet
  retab!
  set tabstop=2 softtabstop=2 et
  retab
endfunction
command! TabSpaceConvert call ConvertSpace()

function! ParinferToggleMode()
  if g:parinfer_mode ==# 'indent'
    let g:parinfer_mode = 'paren'
  else
    let g:parinfer_mode = 'indent'
  endif
endfunction

function! ParinferOff()
  let g:parinfer_mode = 'off'
endfunction

command! ParinferToggleMode call ParinferToggleMode()
command! ParinferOff call ParinferOff()
