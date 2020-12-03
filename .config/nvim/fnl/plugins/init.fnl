(module plugins
  {:require {a aniseed.core
             nvim aniseed.nvim
             r r}
   :require-macros [macros]})

(def- ns (->
           *module*
           (. :aniseed/module)))

(defn main []
  (->>
    [:ale
     :beacon
     :colorizer
     :emmet
     :js
     :markdown
     :mta
     :nerd-commenter
     :rainbow-parens
     :sexp
     :telescope
     :terraform
     :tree-sitter
     :ultisnips]
    (r.map #(.. ns "." $1))
    (r.forEach #(run-main $1))))

(comment (main))
