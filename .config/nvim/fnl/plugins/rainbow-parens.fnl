(module plugins.rainbow-parens
  {:require {: utils}})

(defn main []
  (let [(ok res) (pcall utils.ex.packadd :rainbow_parentheses.vim)]
    (if
      ok (->>
           [{:event :VimEnter
             :pattern :*
             :cmd :RainbowParenthesesActivate}
            {:event :BufEnter
             :pattern :*
             :cmd :RainbowParenthesesLoadRound}
            {:event :BufEnter
             :pattern :*
             :cmd :RainbowParenthesesLoadSquare}
            {:event :BufEnter
             :pattern :*
             :cmd :RainbowParenthesesLoadBraces}]
           (utils.augroup :rainbow-parens-ag))
      (print "Could not load rainbow parens"))))
