(module plugins.nerd-commenter
  {:require {nvim aniseed.nvim}})


; Add spaces after comment delimiters by default
(set nvim.g.NERDSpaceDelims 1)
; Use compact syntax for prettified multi-line comments
(set nvim.g.NERDCompactSexyComs 0)
; Enable trimming of trailing whitespace when uncommenting
(set nvim.g.NERDTrimTrailingWhitespace 1)
; ; Align line-wise comment delimiters flush left instead of following code indentation
(set nvim.g.NERDDefaultAlign "left")
