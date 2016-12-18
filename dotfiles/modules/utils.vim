" Set tabs to two spaces
function! TwoSpace()
  setlocal shiftwidth=2
  setlocal tabstop=2
  setlocal softtabstop=2
  setlocal expandtab
endfunction

" Removes all extraneous whitespace in the file
function! g:StripWhitespace(line1, line2)
    " Save the current search and cursor position
    let l:_s=@/
    let l:l = line('.')
    let l:c = col('.')

    " Strip the whitespace
    silent! execute ':' . a:line1 . ',' . a:line2 . 's/\s\+$//e'

    " Restore the saved search and cursor position
    let @/=l:_s
    call cursor(l:l, l:c)
endfunction
command! -range=% StripWhitespace call StripWhitespace(<line1>, <line2>)

function! ConvertSpace()
  set tabstop=4 softtabstop=4 noet
  retab!
  set tabstop=2 softtabstop=2 et
  retab
endfunction
command! TabSpaceConvert call ConvertSpace()

function! RotateWindowsFunc()
  wincmd H
endfunction

function! GetIdentifer()
  echom synIDattr(synID(line('.'), col('.'), 1), 'name')
endfunction
command! GetIdentifer call GetIdentifer()
