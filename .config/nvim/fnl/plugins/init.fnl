(module plugins
  {:require {: r}
   :require-macros [macros]})

(defn main []
  (->>
    [:airline
     :ale
     :beacon
     :better-whitespace
     :colorizer
     :completion
     :conjure
     :defx
     :delimit-mate
     :easy-align
     :easy-motion
     :emmet
     :lightbulb
     :lspconfig
     :lspkind
     :markdown
     :mta
     :multi-cursor
     :nerd-commenter
     :rainbow-parens
     :sandwich
     :sexp
     :signit
     :startify
     :telescope
     :terraform
     :tree-sitter
     :ultisnips
     :vimtex]
    (r.map #(.. *module-name* "." $1))
    (r.forEach #(run-main $1))))

(comment (main))
