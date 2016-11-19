" Set tabs to two spaces
function! TwoSpace()
  setlocal shiftwidth=2
  setlocal tabstop=2
  setlocal sts=2
  setlocal expandtab
endfunction

" Delete trailing white space on write
func! DeleteTrailingWS()
  exe 'normal mz'
  %s/\s\+$//ge
  exe 'normal `z'
endfunc

function! ConvertSpace()
  set ts=4 sts=4 noet
  retab!
  set ts=2 sts=2 et
  retab
endfunction
command! TabSpaceConvert call ConvertSpace()

function! RotateWindowsFunc()
  wincmd H
endfunction
