(module plugins
  {:require {: r}
   :require-macros [macros]})

(comment ((. (require :plugins.easy-motion) main)))
(defn main []
  (->>
    [:airline
     :ale
     :beacon
     :better-whitespace
     :colorizer
     :delimit-mate
     :easy-align
     :easy-motion
     :emmet
     :js
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
