(module plugins.nerd-commenter
  {:require {utils utils}})


(defn main []
  (utils.set-nvim-g! {; Add spaces after comment delimiters by default
                      :NERDSpaceDelims 1
                      ; Use compact syntax for prettified multi-line comments
                      :NERDCompactSexyComs 0
                      ; Enable trimming of trailing whitespace when uncommenting
                      :NERDTrimTrailingWhitespace 1
                      ; ; Align line-wise comment delimiters flush left instead of following code indentation
                      :NERDDefaultAlign "left"}))
