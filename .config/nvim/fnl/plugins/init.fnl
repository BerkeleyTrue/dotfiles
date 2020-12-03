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
    [:colorizer
     :beacon
     :terraform
     :js
     :ale
     :rainbow-parens
     :ultisnips]
    (r.map #(.. ns "." $1))
    (r.forEach #(run-main $1))))

(comment (main))
