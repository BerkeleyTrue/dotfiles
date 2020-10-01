function! s:HasFlag(flags, flag)
  return a:flags =~ '\%(^\|,\)'.a:flag.'\%(=\([^,]*\)\)\?\%(,\|$\)'
endfunction

function! s:CheckOptions()
  let options = !exists('g:folddigest_options') ? '' : g:folddigest_options
  let s:use_flexnumwidth = s:HasFlag(options, 'flexnumwidth')
  let s:use_nofoldclose = s:HasFlag(options, 'nofoldclose')
  let s:use_vertical = s:HasFlag(options, 'vertical')
  if exists('g:folddigest_size') && (g:folddigest_size + 0) > 0
    let s:digest_size = g:folddigest_size + 0
  else
    let s:digest_size = 0
  endif
endfunction

function! s:IndicateRawline(linenum)
  execute 'match Search /\m^\%'.a:linenum.'l\(\s*\d\+ \)\=\%(\%(| \)*+--\|\^\|\$\)/'
endfunction

function! s:MarkMasterWindow()
  call setwinvar(winnr(), 'folddigested', 1)
  call setbufvar(bufnr('%'), 'folddigested', 1)
endfunction

function! s:GoMasterWindow(...)
  let flags = a:0 > 0 ? a:1 : ""
  let winnr = 1

  while 1
    let bufnr = winbufnr(winnr)
    if bufnr < 0
      " Not found master window
      let bufnr = getbufvar(bufnr('%'), 'bufnr')
      let winnr = bufwinnr(bufnr)
      break
    endif
    if (getwinvar(winnr, 'folddigested') + 0) != 0 && (getbufvar(bufnr, 'folddigested') + 0) != 0
      " Found master window
      break
    endif
    let winnr = winnr + 1
  endwhile

  if winnr > 0
    execute winnr.'wincmd w'
    return winnr
  elseif !s:HasFlag(flags, 'nosplit')
    let bufname = getbufvar(bufnr('%'), 'bufname')
    if s:use_vertical
      if 0 < s:digest_size && s:digest_size < winwidth(0)
  let size = winwidth(0) - s:digest_size
      else
  let size = ''
      endif
      silent execute "rightbelow ".size." vsplit ".bufname
    else
      if 0 < s:digest_size && s:digest_size < winheight(0)
  let size = winheight(0) - s:digest_size
      else
  let size = ''
      endif
      silent execute "rightbelow ".size." split ".bufname
    endif
    call s:MarkMasterWindow()
    return winnr()
  endif
  return 0
endfunction

function! s:Jump()
  let mx = '\m^\%(\s*\(\d\+\) \)\=\%(\(\%(| \)*\)+--\(.*\)$\|\^$\|\$$\)'
  let linenr = line('.')
  let lastline = linenr == line('$') ? 1 : 0
  let line = getline(linenr)
  if line !~ mx
    echohl Error
    echo "Format error has been detected"
    echohl None
    return
  endif
  let linenum = substitute(line, mx, '\1', '') + 0
  let level = strlen(substitute(line, mx, '\2', '')) / 2 + 1
  let text = substitute(line, mx, '\3', '')
  call s:IndicateRawline(linenr)
  call s:GoMasterWindow()
  if lastline
    normal! G
  else
    execute linenum
  endif
  silent! normal! zO
  if !lastline
    normal! zt
  else
    normal! zb
  endif
endfunction

function! s:Refresh()
  if s:GoMasterWindow('nosplit') > 0
    call FoldDigest()
  endif
endfunction

let s:do_auto_refresh = 1

function! s:AutoRefresh()
  if s:do_auto_refresh
    let s:do_auto_refresh = 0
    call s:Refresh()
    let s:do_auto_refresh = 1
  endif
endfunction

function! s:MakeDigestBuffer()
  let bufnum = bufnr('%')
  let bufname = expand('%:p')
  call s:MarkMasterWindow()
  let name = "==FOLDDIGEST== ".expand('%:t')." [".bufnum."]"
  let winnr = bufwinnr(name)
  let s:do_auto_refresh = 0
  if winnr < 1
    let size = s:digest_size > 0 ? s:digest_size : ""
    if s:use_vertical
      silent execute size." vsplit ++enc= ".escape(name, ' ')
    else
      silent execute size." split ++enc= ".escape(name, ' ')
    endif
  else
    execute winnr.'wincmd w'
  endif
  let s:do_auto_refresh = 1
  setlocal buftype=nofile bufhidden=hide noswapfile nowrap ft=
  setlocal foldcolumn=0 nonumber
  match none
  call setbufvar(bufnr('%'), 'bufnr', bufnum)
  call setbufvar(bufnr('%'), 'bufname', bufname)
  silent % delete _
  silent 1 put a
  silent 0 delete _
  " Set buffer local syntax and mapping
  syntax match folddigestLinenr display "^\s*\d\+"
  syntax match folddigestTreemark display "\%(| \)*+--"
  syntax match folddigestFirstline display "\^$"
  syntax match folddigestLastline display "\$$"
  hi def link folddigestLinenr Linenr
  hi def link folddigestTreemark Identifier
  hi def link folddigestFirstline Identifier
  hi def link folddigestLastline Identifier
  nnoremap <silent><buffer> <CR> :call <SID>Jump()<CR>
  nnoremap <silent><buffer> r :call <SID>Refresh()<CR>
endfunction

function! s:Foldtext(linenum, text)
  let text = substitute(a:text, '\m^\s*\%(/\*\|//\)\s*', '', '')
  let text = substitute(text, s:mx_foldmarker, '', 'g')
  let text = substitute(text, s:mx_commentstring, '\1', '')
  let text = substitute(text, '\m\%(^\s\+\|\s\+$\)', '', 'g')
  return text
endfunction

function! s:AddRegA(linenum, text)
  let linestr = '       '.a:linenum
  let linestr = strpart(linestr, strlen(linestr) - s:numwidth).' '
  call setreg('A', linestr.a:text."\<NL>")
endfunction

function! s:AddFoldDigest(linenum)
  let text = s:Foldtext(a:linenum, getline(a:linenum))
  let head = strpart('| | | | | | | | | | | | | | | | | | | ', 0, (foldlevel(a:linenum) - 1) * 2).'+--'
  call s:AddRegA(a:linenum, head.text)
endfunction

function! s:GenerateFoldDigest()
  " Configure script options
  " get the num of digets in the line umber
  " Don't think this is used for getting fold data
  let s:numwidth = strlen(line('$').'')
  if !s:use_flexnumwidth || s:numwidth < 0 || s:numwidth > 7
    let s:numwidth = 7
  endif
  let foldnum = 0
  " store where the cursor is currently
  let cursorline = line('.')
  let cursorfoldnum = -1
  let firstfoldline = 0

  " go to the top of the file
  normal! zRgg

  " does first level have a fold
  if foldlevel(1) > 0
    call s:AddFoldDigest(1)
    let firstfoldline = 1
  else
    call s:AddRegA(1, '^')
  endif

  while 1
    let prevline = line('.')
    normal! zj
    let currline = line('.')
    " on last line
    if prevline == currline
      break
    endif
    " get the fold level of the current line
    if foldlevel(currline) > 0
      " set seen first fold line
      if firstfoldline == 0
        let firstfoldline = currline
      endif

      " if prev line is above or on the cursor and the current line is below the
      " cursor
      if prevline <= cursorline && cursorline < currline
        " store the cursor fold number
        let cursorfoldnum = foldnum
      endif

      " inc fold num
      let foldnum = foldnum + 1
      call s:AddFoldDigest(currline)

    endif
  endwhile

  if cursorfoldnum < 0
    " if cursor is on the last line add one fold?
    let cursorfoldnum = (cursorline == line('$') ? 1 : 0) + foldnum
  endif

  call s:AddRegA(line('$'), '$')
  return cursorfoldnum + 1
endfunction

function! FoldDigest()
  if bufname('%') =~# '\m^==FOLDDIGEST=='
    echohl Error
    echo "Can't make digest for FOLDDIGEST buffer"
    echohl None
    return
  endif
  call s:CheckOptions()
  " Save cursor position
  let save_line = line('.')
  let save_winline = winline()
  " Save undolevels
  let save_undolevels = &undolevels
  set undolevels=-1
  " Save regsiter "a"
  let save_regcont_a = getreg('a')
  let save_regtype_a = getregtype('a')
  call setreg('a', '')
  " Suppress bell when "normal! zj" in a last fold
  let save_visualbell = &visualbell
  let save_t_vb = &t_vb
  set vb t_vb=
  " Generate regexp pattern for Foldtext()
  let s:mx_foldmarker = "\\V\\%(".substitute(escape(getwinvar(winnr(), '&foldmarker'), '\'), ',', '\\|', 'g')."\\)\\d\\*"
  let s:mx_commentstring = '\V'.substitute(escape(getbufvar(bufnr('%'), '&commentstring'), '\'), '%s', '\\(\\.\\*\\)', 'g').''
  let currfold = s:GenerateFoldDigest()
  " Revert bell
  let &visualbell = save_visualbell
  let &t_vb = save_t_vb
  " Revert cursor line
  execute save_line
  if !s:use_nofoldclose
    silent! normal! zMzO
  endif
  " Keep same cursor position as possible
  if save_winline > winline()
    execute "normal! ".(save_winline - winline())."\<C-Y>"
  elseif save_winline < winline()
    execute "normal! ".(winline() - save_winline)."\<C-E>"
  endif
  call s:MakeDigestBuffer()
  echo "currfold=".currfold
  if 0 < currfold && currfold <= line('$')
    execute currfold
    call s:IndicateRawline(currfold)
    if currfold == line('$')
      normal! zb
    endif
  endif
  " Revert register "a"
  call setreg('a', save_regcont_a, save_regtype_a)
  " Revert undolevels
  let &undolevels = save_undolevels
endfunction

augroup FoldDigest
  autocmd!
  autocmd BufEnter ==FOLDDIGEST==* call <SID>AutoRefresh()
augroup END
