" Dracula color map
" 9   :         : Red (FireBrick?)
" 17  : #6272a4 : Comment (Dark Blue)
" 23  :         : Teal
" 24  :         : Dark Neon Blue
" 60  : #6272a4 : Comment (Dark Blue)
" 61  :         : Purple
" 64  :         : Olive
" 81  : #8be9fd : Blue
" 84  : #50fa7b : Green
" 88  :         : Ruby
" 117 :         : Neon Blue
" 141 :         : Dark Purple
" 203 :         : Red-Orange
" 212 : #ff79c6 : Pink
" 215 :         : Orange
" 228 : #50fa7b : Yellow
" 231 : #f8f8f2 : Foreground (White)
" 234 : #282a36 : Black
" 235 :         : Dark Background
" 236 :         : Background
" 241 :         : Selection (Dark gray)
" 246 : #44475a : Selection (gray)

function! AddHighlight()
  " Underline misspelled words
  highlight clear SpellBad
  highlight SpellBad ctermfg=none ctermbg=none cterm=underline
  highlight SpellLocal ctermfg=none ctermbg=none cterm=underline
  highlight SpellRare ctermfg=none ctermbg=none cterm=underline

  " make the highlighting of tabs and other non-text less annoying
  highlight NonText ctermbg=none ctermfg=8
  highlight SpecialKey ctermbg=none ctermfg=88

  " Add Special JavaScript highlighting
  highlight jsFuncCall ctermfg=141
  highlight jsGlobalObjects ctermfg=9
  highlight jsString ctermfg=215
  highlight jsTemplateBraces ctermfg=24
  highlight jsTemplateString ctermfg=215
  highlight jsTemplateVar ctermfg=228
  highlight jsThis ctermfg=203
  highlight jsObjectKey ctermfg=117

  highlight jsonKeyword ctermfg=117
  highlight jsonString ctermfg=86

  highlight vimMapLhs ctermfg=215
  highlight vimMapModKey ctermfg=117
  highlight vimNotation ctermfg=86
endfunction

" add Vim theme
colorscheme dracula

augroup AutoColors
  autocmd!
  autocmd VimEnter * call AddHighlight()
  autocmd ColorScheme * call AddHighlight()
augroup END

