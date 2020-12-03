(module plugins.rainbow-parens
  {:require {utils utils}})

(defn main []
  (->>
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
    (utils.augroup :rainbow-parens-ag)))
