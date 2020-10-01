" CONTROL VARIABLES:
" g:scrollfix - percentage from top of screen where to lock cursor
"               -1 - disables. Default: 60
" g:fixeof    - 1=>fix cursor also near end-of-file; 0=>no. Default:0
" g:scrollinfo - 1=>inform when scrollfix is turned on, 0=>no. Default: 1

if exists('g:scrollfix_plugin') | finish | endif
let g:scrollfix_plugin = 1

"------------------- Parameterization Variables -----------------------
if !exists('g:scrollfix')
  let g:scrollfix=60  " percentable of screen height to keep visual cursor on
  " let g:scrollfix=-1 " -1=>disable scrollfix
endif
if !exists('g:fixeof')
  "let g:fixeof=1 => fix cursor also near end-of-file
  "let g:fixeof=0 => do not fix cursor near end-of-file
  let g:fixeof=0
endif
if !exists('g:scrollinfo')
  let g:scrollinfo=1 " 1=>inform when scrollfix is turned on, 0=>no
endif
"---------------- End of Parameterization Variables -------------------

" aug scrollfix
"   au!
"   au CursorMoved,CursorMovedI * :call ScrollFix()
" aug END

function! ScrollFix()
  " if disabled return
  if g:scrollfix<0 | return | endif
  " if softtabstop set, unset
  if &softtabstop!=0 | set softtabstop=0 | endif

  " keep cursor on fixed visual line of the window

  " window height * the cursor desired window hieght
  " gives the desired num of lines from the top of the window
  let fixline = ( winheight(0) * g:scrollfix ) / 100

  " get meta of current window
  let dict = winsaveview()

  " if the current cursor line num in the buffer
  " is the same as fixline, return
  "
  " means: window topline is the same as the buffer topline
  " and cursor is at the desired location
  if dict['lnum'] <= fixline | return | endif

  " if current line in the buffer
  " minus the top buffer line number in the window + 1
  " is equal to the fixline,
  " return
  "
  " means: the cursor is just below the fix line?
  if dict['lnum'] - dict['topline'] + 1 == fixline | return | endif

  " fix the cursor at the end of the buffer as well
  if g:fixeof
    " if last line is less than topline + total visible file lines
    " and visual-line is >= fixline, don't fix cursor
    "
    " means; if the top line in the buffer + the total num of visible lines
    " is greater than the last line number, we are at the bottom of the file
    " and current visual line is greater than or equal the desired line
    " don't fix
    if line('$') < dict['topline'] + winheight(0) && dict['lnum'] >= fixline
      return
    endif
  endif

  " set the topline = to the current cursor line plus the fixline
  let dict['topline'] = dict['lnum'] - fixline + 1
  " let g:last = dict
  call winrestview(dict)

  if g:scrollinfo
    if !exists('b:fixline') || b:fixline != fixline
      let b:fixline = fixline
      let save_lz = &lazyredraw
      " force redraw the screen
      set nolazyredraw
      redraw
      echo 'scroll fixed at line ' . b:fixline . ' of ' . winheight(0).' ('. g:scrollfix '%)'
      let &lazyredraw = save_lz
    endif
  endif
endfunction
