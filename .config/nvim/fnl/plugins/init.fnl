(module plugins
  {:require {a aniseed.core
             nvim aniseed.nvim
             r r}
   :require-macros [macros]})

(defn main []
  (->>
    [:airline
     :ale
     :beacon
     :better-whitespace
     :colorizer
     :delimit-mate
     :emmet
     :js
     :markdown
     :mta
     :multi-cursor
     :nerd-commenter
     :rainbow-parens
     :sexp
     :sandwich
     :startify
     :telescope
     :terraform
     :tree-sitter
     :ultisnips
     :vimtex]
    (r.map #(.. *module-name* "." $1))
    (r.forEach #(run-main $1))))

(comment (main))
